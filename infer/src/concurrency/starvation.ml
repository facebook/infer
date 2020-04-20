(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter
module Domain = StarvationDomain

let pname_pp = MF.wrap_monospaced Procname.pp

let attrs_of_pname = Summary.OnDisk.proc_resolve_attributes

module Payload = SummaryPayload.Make (struct
  type t = Domain.summary

  let field = Payloads.Fields.starvation
end)

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type extras = FormalMap.t

  let log_parse_error error pname actuals =
    L.debug Analysis Verbose "%s pname:%a actuals:%a@." error Procname.pp pname
      (PrettyPrintable.pp_collection ~pp_item:HilExp.pp)
      actuals


  let rec get_access_expr (hilexp : HilExp.t) =
    match hilexp with
    | AccessExpression access_exp ->
        Some access_exp
    | Cast (_, hilexp) | Exception hilexp | UnaryOperator (_, hilexp, _) ->
        get_access_expr hilexp
    | BinaryOperator _ | Closure _ | Constant _ | Sizeof _ ->
        None


  let get_access_expr_list actuals = List.map actuals ~f:get_access_expr

  let do_assume assume_exp (astate : Domain.t) =
    let open Domain in
    let add_thread_choice (acc : Domain.t) bool_value =
      let thread = if bool_value then ThreadDomain.UIThread else ThreadDomain.BGThread in
      {acc with thread}
    in
    let add_future_done_choice acc_exp (acc : Domain.t) bool_value =
      AttributeDomain.find_opt acc_exp acc.attributes
      |> Option.bind ~f:(function Attribute.FutureDoneGuard future -> Some future | _ -> None)
      |> Option.value_map ~default:acc ~f:(fun future ->
             let attributes =
               AttributeDomain.add future (Attribute.FutureDoneState bool_value) acc.attributes
             in
             {acc with attributes} )
    in
    match HilExp.get_access_exprs assume_exp with
    | [access_expr] when AttributeDomain.is_thread_guard access_expr astate.attributes ->
        HilExp.eval_boolean_exp access_expr assume_exp
        |> Option.fold ~init:astate ~f:add_thread_choice
    | [access_expr] when AttributeDomain.is_future_done_guard access_expr astate.attributes ->
        HilExp.eval_boolean_exp access_expr assume_exp
        |> Option.fold ~init:astate ~f:(add_future_done_choice access_expr)
    | _ ->
        astate


  (* get attribute of an expression, and if there is none, check if the expression can
     be implicitly ascribed a property (runnable/executor). *)
  let get_exp_attributes tenv exp (astate : Domain.t) =
    let open Domain in
    AttributeDomain.find_opt exp astate.attributes
    |> IOption.if_none_evalopt ~f:(fun () ->
           StarvationModels.get_executor_thread_annotation_constraint tenv exp
           |> Option.map ~f:(fun constr -> Attribute.WorkScheduler constr) )
    |> IOption.if_none_evalopt ~f:(fun () ->
           StarvationModels.get_run_method_from_runnable tenv exp
           |> Option.map ~f:(fun procname -> Attribute.Runnable procname) )


  let do_work_scheduling tenv callee actuals loc (astate : Domain.t) =
    let open Domain in
    let schedule_work (runnable, thread) =
      get_exp_attributes tenv runnable astate
      |> Option.bind ~f:(function Attribute.Runnable procname -> Some procname | _ -> None)
      |> Option.fold ~init:astate ~f:(Domain.schedule_work loc thread)
    in
    let work_opt =
      match get_access_expr_list actuals with
      | Some executor :: Some runnable :: _ when StarvationModels.schedules_work tenv callee ->
          let thread =
            get_exp_attributes tenv executor astate
            |> Option.bind ~f:(function Attribute.WorkScheduler c -> Some c | _ -> None)
            |> Option.value ~default:StarvationModels.ForUnknownThread
          in
          Some (runnable, thread)
      | Some runnable :: _ when StarvationModels.schedules_work_on_ui_thread tenv callee ->
          Some (runnable, StarvationModels.ForUIThread)
      | Some runnable :: _ when StarvationModels.schedules_work_on_bg_thread tenv callee ->
          Some (runnable, StarvationModels.ForNonUIThread)
      | _ ->
          None
    in
    Option.value_map work_opt ~default:astate ~f:schedule_work


  let do_assignment tenv lhs_access_exp rhs_exp (astate : Domain.t) =
    get_access_expr rhs_exp
    |> Option.bind ~f:(fun exp -> get_exp_attributes tenv exp astate)
    |> Option.value_map ~default:astate ~f:(fun attribute ->
           let attributes = Domain.AttributeDomain.add lhs_access_exp attribute astate.attributes in
           {astate with attributes} )


  let do_call ProcData.{tenv; summary; extras} lhs callee actuals loc (astate : Domain.t) =
    let open Domain in
    let make_ret_attr return_attribute = {empty_summary with return_attribute} in
    let make_thread thread = {empty_summary with thread} in
    let actuals_acc_exps = get_access_expr_list actuals in
    let get_returned_executor_summary () =
      StarvationModels.get_returned_executor ~attrs_of_pname tenv callee actuals
      |> Option.map ~f:(fun thread_constraint -> make_ret_attr (WorkScheduler thread_constraint))
    in
    let get_thread_assert_summary () =
      match ConcurrencyModels.get_thread_assert_effect callee with
      | BackgroundThread ->
          Some (make_thread BGThread)
      | MainThread ->
          Some (make_thread UIThread)
      | MainThreadIfTrue ->
          Some (make_ret_attr ThreadGuard)
      | UnknownThread ->
          None
    in
    let get_future_is_done_summary () =
      if StarvationModels.is_future_is_done tenv callee actuals then
        List.hd actuals_acc_exps |> Option.join
        |> Option.map ~f:(fun future -> make_ret_attr (FutureDoneGuard future))
      else None
    in
    let get_mainLooper_summary () =
      if StarvationModels.is_getMainLooper tenv callee actuals then
        Some (make_ret_attr (Looper ForUIThread))
      else None
    in
    let get_callee_summary () = Payload.read ~caller_summary:summary ~callee_pname:callee in
    let treat_handler_constructor () =
      if StarvationModels.is_handler_constructor tenv callee actuals then
        match actuals_acc_exps with
        | Some receiver :: Some looper :: _ ->
            let constr =
              AttributeDomain.find_opt looper astate.attributes
              |> Option.bind ~f:(function Attribute.Looper c -> Some c | _ -> None)
              |> Option.value ~default:StarvationModels.ForUnknownThread
            in
            let attributes =
              AttributeDomain.add receiver (WorkScheduler constr) astate.attributes
            in
            Some {astate with attributes}
        | _ ->
            None
      else None
    in
    let treat_thread_constructor () =
      if StarvationModels.is_thread_constructor tenv callee actuals then
        match actuals_acc_exps with
        | Some receiver :: rest ->
            ( match rest with
            | Some exp1 :: Some exp2 :: _ ->
                (* two additional arguments, either could be a runnable, see docs *)
                [exp1; exp2]
            | Some runnable :: _ ->
                (* either just one argument, or more but 2nd is not an access expression *)
                [runnable]
            | _ ->
                [] )
            |> List.map ~f:(fun r () -> StarvationModels.get_run_method_from_runnable tenv r)
            |> IList.eval_until_first_some
            |> Option.map ~f:(fun procname ->
                   let attributes =
                     AttributeDomain.add receiver (Runnable procname) astate.attributes
                   in
                   {astate with attributes} )
        | _ ->
            None
      else None
    in
    let treat_assume () =
      if StarvationModels.is_assume_true tenv callee actuals then
        List.hd actuals |> Option.map ~f:(fun exp -> do_assume exp astate)
      else None
    in
    (* constructor calls are special-cased because they side-effect the receiver and do not
       return anything *)
    let treat_modeled_summaries () =
      IList.eval_until_first_some
        [ get_returned_executor_summary
        ; get_thread_assert_summary
        ; get_future_is_done_summary
        ; get_mainLooper_summary
        ; get_callee_summary ]
      |> Option.map ~f:(fun summary ->
             let subst = Lock.make_subst extras actuals in
             let callsite = CallSite.make callee loc in
             Domain.integrate_summary ~tenv ~lhs ~subst callsite astate summary )
    in
    IList.eval_until_first_some
      [treat_handler_constructor; treat_thread_constructor; treat_assume; treat_modeled_summaries]
    |> Option.value ~default:astate


  let exec_instr (astate : Domain.t) ({ProcData.summary; tenv; extras} as procdata) _
      (instr : HilInstr.t) =
    let open ConcurrencyModels in
    let open StarvationModels in
    let get_lock_path = Domain.Lock.make extras in
    let procname = Summary.get_proc_name summary in
    let is_java = Procname.is_java procname in
    let do_lock locks loc astate =
      List.filter_map ~f:get_lock_path locks |> Domain.acquire ~tenv astate ~procname ~loc
    in
    let do_unlock locks astate = List.filter_map ~f:get_lock_path locks |> Domain.release astate in
    match instr with
    | Assign (lhs_access_exp, rhs_exp, _) ->
        do_assignment tenv lhs_access_exp rhs_exp astate
    | Metadata (Sil.ExitScope (vars, _)) ->
        {astate with attributes= Domain.AttributeDomain.exit_scope vars astate.attributes}
    | Metadata _ ->
        astate
    | Assume (assume_exp, _, _, _) ->
        do_assume assume_exp astate
    | Call (_, Indirect _, _, _, _) ->
        astate
    | Call (_, Direct callee, actuals, _, _) when should_skip_analysis tenv callee actuals ->
        astate
    | Call (ret_base, Direct callee, actuals, _, loc) -> (
      match get_lock_effect callee actuals with
      | Lock locks ->
          do_lock locks loc astate
      | GuardLock guard ->
          Domain.lock_guard tenv astate guard ~procname ~loc
      | GuardConstruct {guard; lock; acquire_now} -> (
        match get_lock_path lock with
        | Some lock_path ->
            Domain.add_guard tenv astate guard lock_path ~acquire_now ~procname ~loc
        | None ->
            log_parse_error "Couldn't parse lock in guard constructor" callee actuals ;
            astate )
      | Unlock locks ->
          do_unlock locks astate
      | GuardUnlock guard ->
          Domain.unlock_guard astate guard
      | GuardDestroy guard ->
          Domain.remove_guard astate guard
      | LockedIfTrue _ | GuardLockedIfTrue _ ->
          astate
      | NoEffect when is_synchronized_library_call tenv callee ->
          (* model a synchronized call without visible internal behaviour *)
          let locks = List.hd actuals |> Option.to_list in
          do_lock locks loc astate |> do_unlock locks
      | NoEffect when is_java && is_strict_mode_violation tenv callee actuals ->
          Domain.strict_mode_call ~callee ~loc astate
      | NoEffect when is_java && is_monitor_wait tenv callee actuals ->
          Domain.wait_on_monitor ~loc extras actuals astate
      | NoEffect when is_java && is_future_get tenv callee actuals ->
          Domain.future_get ~callee ~loc actuals astate
      | NoEffect when is_java -> (
          let ret_exp = HilExp.AccessExpression.base ret_base in
          let astate = do_work_scheduling tenv callee actuals loc astate in
          match may_block tenv callee actuals with
          | Some sev ->
              Domain.blocking_call ~callee sev ~loc astate
          | None ->
              do_call procdata ret_exp callee actuals loc astate )
      | NoEffect ->
          (* in C++/Obj C we only care about deadlocks, not starvation errors *)
          let ret_exp = HilExp.AccessExpression.base ret_base in
          do_call procdata ret_exp callee actuals loc astate )


  let pp_session_name _node fmt = F.pp_print_string fmt "starvation"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

(** Compute the attributes (of static variables) set up by the class initializer. *)
let set_class_init_attributes procname (astate : Domain.t) =
  let open Domain in
  let attributes =
    ConcurrencyUtils.get_java_class_initializer_summary_of procname
    |> Option.bind ~f:Payload.of_summary
    |> Option.value_map ~default:AttributeDomain.top ~f:(fun summary -> summary.attributes)
  in
  ({astate with attributes} : t)


(** Compute the attributes of instance variables that all constructors agree on. *)
let set_constructor_attributes tenv summary (astate : Domain.t) =
  let procname = Summary.get_proc_name summary in
  let open Domain in
  (* make a local [this] variable, for replacing all constructor attribute map keys' roots *)
  let local_this = Pvar.mk Mangled.this procname |> Var.of_pvar in
  let make_local exp =
    (* contract here matches that of [StarvationDomain.summary_of_astate] *)
    let var, typ = HilExp.AccessExpression.get_base exp in
    if Var.is_global var then
      (* let expressions rooted at globals unchanged, these are probably from class initialiser *)
      exp
    else (
      assert (Var.is_this var) ;
      HilExp.AccessExpression.replace_base ~remove_deref_after_base:false (local_this, typ) exp )
  in
  let localize_attrs attributes =
    AttributeDomain.(fold (fun exp attr acc -> add (make_local exp) attr acc) attributes empty)
  in
  let attributes =
    ConcurrencyUtils.get_java_constructor_summaries_of tenv summary
    |> List.filter_map ~f:Payload.of_summary
    (* make instances of [this] local to the current procedure and select only the attributes *)
    |> List.map ~f:(fun (summary : Domain.summary) -> localize_attrs summary.attributes)
    (* join all the attribute maps together *)
    |> List.reduce ~f:AttributeDomain.join
    |> Option.value ~default:AttributeDomain.top
  in
  {astate with attributes}


let set_initial_attributes tenv summary astate =
  let procname = Summary.get_proc_name summary in
  if not Config.starvation_whole_program then astate
  else
    match procname with
    | Procname.Java java_pname when Procname.Java.is_class_initializer java_pname ->
        (* we are analyzing the class initializer, don't go through on-demand again *)
        astate
    | Procname.Java java_pname
      when Procname.Java.(is_constructor java_pname || is_static java_pname) ->
        (* analyzing a constructor or static method, so we need the attributes established by the
           class initializer *)
        set_class_init_attributes summary astate
    | Procname.Java _ ->
        (* we are analyzing an instance method, so we need constructor-established attributes
           which will include those by the class initializer *)
        set_constructor_attributes tenv summary astate
    | _ ->
        astate


let analyze_procedure {Callbacks.exe_env; summary} =
  let proc_desc = Summary.get_proc_desc summary in
  let procname = Procdesc.get_proc_name proc_desc in
  let tenv = Exe_env.get_tenv exe_env procname in
  if StarvationModels.should_skip_analysis tenv procname [] then summary
  else
    let formals = FormalMap.make proc_desc in
    let proc_data = ProcData.make summary tenv formals in
    let loc = Procdesc.get_loc proc_desc in
    let set_lock_state_for_synchronized_proc astate =
      if Procdesc.is_java_synchronized proc_desc then
        Domain.Lock.make_java_synchronized formals procname
        |> Option.to_list
        |> Domain.acquire ~tenv astate ~procname ~loc
      else astate
    in
    let set_thread_status_by_annotation (astate : Domain.t) =
      let thread =
        if ConcurrencyModels.annotated_as_worker_thread ~attrs_of_pname tenv procname then
          Domain.ThreadDomain.BGThread
        else if ConcurrencyModels.runs_on_ui_thread ~attrs_of_pname tenv procname then
          Domain.ThreadDomain.UIThread
        else astate.thread
      in
      {astate with thread}
    in
    let filter_blocks =
      if StarvationModels.is_annotated_nonblocking ~attrs_of_pname tenv procname then
        Domain.filter_blocking_calls
      else Fn.id
    in
    let initial =
      Domain.bottom
      (* set the attributes of instance variables set up by all constructors or the class initializer *)
      |> set_initial_attributes tenv summary
      |> set_lock_state_for_synchronized_proc |> set_thread_status_by_annotation
    in
    Analyzer.compute_post proc_data ~initial
    |> Option.map ~f:filter_blocks
    |> Option.map ~f:(Domain.summary_of_astate proc_desc)
    |> Option.fold ~init:summary ~f:(fun acc payload -> Payload.update_summary payload acc)


(** per-file report map, which takes care of deduplication *)
module ReportMap : sig
  type t

  val empty : t

  type report_add_t = Tenv.t -> Procdesc.t -> Location.t -> Errlog.loc_trace -> string -> t -> t

  val add_deadlock : report_add_t

  val add_starvation : StarvationModels.severity -> report_add_t

  val add_strict_mode_violation : report_add_t

  val add_lockless_violation : report_add_t

  val issue_log_of : t -> IssueLog.t

  val store_multi_file : t -> unit
  (** generate and store issue logs for all source files involved in this report map; for use in the
      whole-program mode only *)
end = struct
  type problem =
    | Starvation of StarvationModels.severity
    | Deadlock of int
    | StrictModeViolation of int
    | LocklessViolation of int

  let issue_type_of_problem = function
    | Deadlock _ ->
        IssueType.deadlock
    | Starvation _ ->
        IssueType.starvation
    | StrictModeViolation _ ->
        IssueType.strict_mode_violation
    | LocklessViolation _ ->
        IssueType.lockless_violation


  type report_t = {problem: problem; pname: Procname.t; ltr: Errlog.loc_trace; message: string}

  type t = report_t list Location.Map.t

  type report_add_t = Tenv.t -> Procdesc.t -> Location.t -> Errlog.loc_trace -> string -> t -> t

  let empty : t = Location.Map.empty

  let add tenv pdesc loc report loc_map =
    if Reporting.is_suppressed tenv pdesc (issue_type_of_problem report.problem) then loc_map
    else
      Location.Map.update loc
        (function reports_opt -> Some (report :: Option.value reports_opt ~default:[]))
        loc_map


  let add_deadlock tenv pdesc loc ltr message (map : t) =
    let pname = Procdesc.get_proc_name pdesc in
    let report = {problem= Deadlock (-List.length ltr); pname; ltr; message} in
    add tenv pdesc loc report map


  let add_starvation sev tenv pdesc loc ltr message map =
    let pname = Procdesc.get_proc_name pdesc in
    let report = {pname; problem= Starvation sev; ltr; message} in
    add tenv pdesc loc report map


  let add_strict_mode_violation tenv pdesc loc ltr message (map : t) =
    let pname = Procdesc.get_proc_name pdesc in
    let report = {problem= StrictModeViolation (-List.length ltr); pname; ltr; message} in
    add tenv pdesc loc report map


  let add_lockless_violation tenv pdesc loc ltr message (map : t) =
    let pname = Procdesc.get_proc_name pdesc in
    let report = {problem= LocklessViolation (-List.length ltr); pname; ltr; message} in
    add tenv pdesc loc report map


  let issue_log_of loc_map =
    let log_report ~issue_log loc {problem; pname; ltr; message} =
      let issue_type = issue_type_of_problem problem in
      Reporting.log_issue_external ~issue_log pname Exceptions.Error ~loc ~ltr issue_type message
    in
    let mk_deduped_report ({message} as report) =
      { report with
        message= Printf.sprintf "%s Additional report(s) on the same line were suppressed." message
      }
    in
    let log_reports compare loc reports issue_log =
      if Config.deduplicate then
        match reports with
        | [] ->
            issue_log
        | [(_, report)] ->
            log_report ~issue_log loc report
        | reports ->
            List.max_elt ~compare reports
            |> Option.fold ~init:issue_log ~f:(fun acc (_, rep) ->
                   mk_deduped_report rep |> log_report ~issue_log:acc loc )
      else
        List.fold reports ~init:issue_log ~f:(fun acc (_, rep) -> log_report ~issue_log:acc loc rep)
    in
    let filter_map_deadlock = function {problem= Deadlock l} as r -> Some (l, r) | _ -> None in
    let filter_map_starvation = function
      | {problem= Starvation s} as r ->
          Some (s, r)
      | _ ->
          None
    in
    let filter_map_strict_mode_violation = function
      | {problem= StrictModeViolation l} as r ->
          Some (l, r)
      | _ ->
          None
    in
    let filter_map_lockless_violation = function
      | {problem= LocklessViolation l} as r ->
          Some (l, r)
      | _ ->
          None
    in
    let compare_reports weight_compare (w, r) (w', r') =
      match weight_compare w w' with 0 -> String.compare r.message r'.message | result -> result
    in
    let log_location loc problems issue_log =
      let deadlocks = List.filter_map problems ~f:filter_map_deadlock in
      let starvations = List.filter_map problems ~f:filter_map_starvation in
      let strict_mode_violations = List.filter_map problems ~f:filter_map_strict_mode_violation in
      let lockless_violations = List.filter_map problems ~f:filter_map_lockless_violation in
      log_reports (compare_reports Int.compare) loc deadlocks issue_log
      |> log_reports (compare_reports Int.compare) loc lockless_violations
      |> log_reports (compare_reports StarvationModels.compare_severity) loc starvations
      |> log_reports (compare_reports Int.compare) loc strict_mode_violations
    in
    Location.Map.fold log_location loc_map IssueLog.empty


  let store_multi_file loc_map =
    let update_loc_map key value file_loc_map_opt =
      let file_loc_map = Option.value file_loc_map_opt ~default:Location.Map.empty in
      Some (Location.Map.add key value file_loc_map)
    in
    let source_map =
      Location.Map.fold
        (fun key value acc -> SourceFile.Map.update key.Location.file (update_loc_map key value) acc)
        loc_map SourceFile.Map.empty
    in
    SourceFile.Map.iter
      (fun file loc_map -> issue_log_of loc_map |> IssueLog.store ~entry:StarvationIssues ~file)
      source_map
end

let should_report_deadlock_on_current_proc current_elem endpoint_elem =
  let open Domain in
  (not Config.deduplicate)
  ||
  match (endpoint_elem.CriticalPair.elem.event, current_elem.CriticalPair.elem.event) with
  | _, (MayBlock _ | StrictModeCall _ | MonitorWait _)
  | (MayBlock _ | StrictModeCall _ | MonitorWait _), _ ->
      (* should never happen *)
      L.die InternalError "Deadlock cannot occur without two lock events: %a" CriticalPair.pp
        current_elem
  | LockAcquire endpoint_lock, LockAcquire current_lock -> (
      (* first elem is a class object (see [lock_of_class]), so always report because the
         reverse ordering on the events will not occur since we don't search the class for static locks *)
      Lock.is_class_object endpoint_lock
      ||
      match Lock.compare_wrt_reporting endpoint_lock current_lock with
      | 0 ->
          Location.compare current_elem.CriticalPair.loc endpoint_elem.CriticalPair.loc < 0
      | c ->
          c < 0 )


let should_report pdesc =
  (not (PredSymb.equal_access (Procdesc.get_access pdesc) Private))
  &&
  match Procdesc.get_proc_name pdesc with
  | Procname.Java java_pname ->
      (not (Procname.Java.is_autogen_method java_pname))
      && not (Procname.Java.is_class_initializer java_pname)
  | Procname.ObjC_Cpp _ ->
      true
  | _ ->
      false


let fold_reportable_summaries (tenv, current_summary) clazz ~init ~f =
  let methods =
    Tenv.lookup tenv clazz
    |> Option.value_map ~default:[] ~f:(fun tstruct -> tstruct.Struct.methods)
  in
  let f acc mthd =
    Ondemand.get_proc_desc mthd
    |> Option.value_map ~default:acc ~f:(fun other_pdesc ->
           if should_report other_pdesc then
             Payload.read ~caller_summary:current_summary ~callee_pname:mthd
             |> Option.map ~f:(fun payload -> (mthd, payload))
             |> Option.fold ~init:acc ~f
           else acc )
  in
  List.fold methods ~init ~f


(*  Note about how many times we report a deadlock: normally twice, at each trace starting point.
    Due to the fact we look for deadlocks in the summaries of the class at the root of a path,
    this will fail when (a) the lock is of class type (ie as used in static sync methods), because
    then the root is an identifier of type java.lang.Class and (b) when the lock belongs to an
    inner class but this is no longer obvious in the path, because of nested-class path normalisation.
    The net effect of the above issues is that we will only see these locks in conflicting pairs
    once, as opposed to twice with all other deadlock pairs. *)

(** report warnings possible on the parallel composition of two threads/critical pairs
    [should_report_starvation] means [pair] is on the UI thread and not on a constructor *)
let report_on_parallel_composition ~should_report_starvation tenv pdesc pair lock other_pname
    other_pair report_map =
  let open Domain in
  let pname = Procdesc.get_proc_name pdesc in
  let make_trace_and_loc () =
    let first_trace = CriticalPair.make_trace ~header:"[Trace 1] " pname pair in
    let second_trace = CriticalPair.make_trace ~header:"[Trace 2] " other_pname other_pair in
    let ltr = first_trace @ second_trace in
    let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
    (ltr, loc)
  in
  if CriticalPair.can_run_in_parallel pair other_pair then
    let acquisitions = other_pair.CriticalPair.elem.acquisitions in
    match other_pair.CriticalPair.elem.event with
    | MayBlock (_, sev) as event
      when should_report_starvation
           && Acquisitions.lock_is_held_in_other_thread tenv lock acquisitions ->
        let error_message =
          Format.asprintf
            "Method %a runs on UI thread and%a, which may be held by another thread which %a."
            pname_pp pname Lock.pp_locks lock Event.describe event
        in
        let ltr, loc = make_trace_and_loc () in
        ReportMap.add_starvation sev tenv pdesc loc ltr error_message report_map
    | MonitorWait monitor_lock
      when should_report_starvation
           && Acquisitions.lock_is_held_in_other_thread tenv lock acquisitions
           && not (Lock.equal lock monitor_lock) ->
        let error_message =
          Format.asprintf
            "Method %a runs on UI thread and%a, which may be held by another thread which %a."
            pname_pp pname Lock.pp_locks lock Event.describe other_pair.CriticalPair.elem.event
        in
        let ltr, loc = make_trace_and_loc () in
        ReportMap.add_starvation High tenv pdesc loc ltr error_message report_map
    | LockAcquire other_lock
      when CriticalPair.may_deadlock tenv pair other_pair
           && should_report_deadlock_on_current_proc pair other_pair ->
        let error_message =
          Format.asprintf
            "Potential deadlock. %a (Trace 1) and %a (Trace 2) acquire locks %a and %a in reverse \
             orders."
            pname_pp pname pname_pp other_pname Lock.describe lock Lock.describe other_lock
        in
        let ltr, loc = make_trace_and_loc () in
        ReportMap.add_deadlock tenv pdesc loc ltr error_message report_map
    | _ ->
        report_map
  else report_map


let report_on_pair tenv summary (pair : Domain.CriticalPair.t) report_map =
  let open Domain in
  let pdesc = Summary.get_proc_desc summary in
  let pname = Summary.get_proc_name summary in
  let event = pair.elem.event in
  let should_report_starvation =
    CriticalPair.is_uithread pair && not (Procname.is_constructor pname)
  in
  let make_trace_and_loc () =
    let loc = CriticalPair.get_loc pair in
    let ltr = CriticalPair.make_trace ~include_acquisitions:false pname pair in
    (ltr, loc)
  in
  match event with
  | MayBlock (_, sev) when should_report_starvation ->
      let error_message =
        Format.asprintf "Method %a runs on UI thread and may block; %a." pname_pp pname
          Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_starvation sev tenv pdesc loc ltr error_message report_map
  | MonitorWait _ when should_report_starvation ->
      let error_message =
        Format.asprintf "Method %a runs on UI thread and may block; %a." pname_pp pname
          Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_starvation High tenv pdesc loc ltr error_message report_map
  | StrictModeCall _ when should_report_starvation ->
      let error_message =
        Format.asprintf "Method %a runs on UI thread and may violate Strict Mode; %a." pname_pp
          pname Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_strict_mode_violation tenv pdesc loc ltr error_message report_map
  | LockAcquire _ when StarvationModels.is_annotated_lockless ~attrs_of_pname tenv pname ->
      let error_message =
        Format.asprintf "Method %a is annotated %s but%a." pname_pp pname
          (MF.monospaced_to_string Annotations.lockless)
          Event.describe event
      in
      let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
      let ltr = CriticalPair.make_trace pname pair in
      ReportMap.add_lockless_violation tenv pdesc loc ltr error_message report_map
  | LockAcquire lock when Acquisitions.lock_is_held lock pair.elem.acquisitions ->
      let error_message =
        Format.asprintf "Potential self deadlock. %a%a twice." pname_pp pname Lock.pp_locks lock
      in
      let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
      let ltr = CriticalPair.make_trace ~header:"In method " pname pair in
      ReportMap.add_deadlock tenv pdesc loc ltr error_message report_map
  | LockAcquire lock when not Config.starvation_whole_program ->
      Lock.root_class lock
      |> Option.value_map ~default:report_map ~f:(fun other_class ->
             (* get the class of the root variable of the lock in the lock acquisition
                and retrieve all the summaries of the methods of that class;
                then, report on the parallel composition of the current pair and any pair in these
                summaries that can indeed run in parallel *)
             fold_reportable_summaries (tenv, summary) other_class ~init:report_map
               ~f:(fun acc (other_pname, {critical_pairs}) ->
                 CriticalPairs.fold
                   (report_on_parallel_composition ~should_report_starvation tenv pdesc pair lock
                      other_pname)
                   critical_pairs acc ) )
  | _ ->
      report_map


let reporting {Callbacks.procedures; exe_env} =
  if Config.starvation_whole_program then IssueLog.empty
  else
    let report_on_summary tenv summary report_map (payload : Domain.summary) =
      Domain.CriticalPairs.fold (report_on_pair tenv summary) payload.critical_pairs report_map
    in
    let report_procedure report_map procname =
      Ondemand.analyze_proc_name_no_caller procname
      |> Option.value_map ~default:report_map ~f:(fun summary ->
             let proc_desc = Summary.get_proc_desc summary in
             let tenv = Exe_env.get_tenv exe_env procname in
             if should_report proc_desc then
               Payload.read_toplevel_procedure procname
               |> Option.fold ~init:report_map ~f:(report_on_summary tenv summary)
             else report_map )
    in
    List.fold procedures ~init:ReportMap.empty ~f:report_procedure |> ReportMap.issue_log_of


(* given a scheduled-work item, read the summary of the scheduled method from the disk
   and adapt its contents to the thread it was scheduled too *)
let get_summary_of_scheduled_work (work_item : Domain.ScheduledWorkItem.t) =
  let astate = {Domain.bottom with thread= work_item.thread} in
  let callsite = CallSite.make work_item.procname work_item.loc in
  Summary.OnDisk.get work_item.procname
  |> Option.bind ~f:(fun (summary : Summary.t) -> Payloads.starvation summary.payloads)
  |> Option.map ~f:(Domain.integrate_summary callsite astate)
  |> Option.map ~f:(fun (astate : Domain.t) -> astate.critical_pairs)


(* given a summary, do [f work critical_pairs] for each [work] item scheduled in the summary,
   where [critical_pairs] are those of the method scheduled, adapted to the thread it's scheduled for *)
let iter_summary ~f exe_env (summary : Summary.t) =
  let open Domain in
  Payloads.starvation summary.payloads
  |> Option.iter ~f:(fun ({scheduled_work; critical_pairs} : summary) ->
         let pname = Summary.get_proc_name summary in
         let tenv = Exe_env.get_tenv exe_env pname in
         if ConcurrencyModels.is_modeled_ui_method tenv pname then f pname critical_pairs ;
         ScheduledWorkDomain.iter
           (fun work -> get_summary_of_scheduled_work work |> Option.iter ~f:(f pname))
           scheduled_work )


module WorkHashSet = struct
  module T = struct
    type t = Procname.t * Domain.CriticalPair.t

    (* [compare] for critical pairs ignore various fields, so using a generated equality here would
       break the polymorphic hash function.  We use [phys_equal] instead and rely on the clients to
       not add duplicate items. *)
    let equal = phys_equal

    let hash = Hashtbl.hash
  end

  include Caml.Hashtbl.Make (T)

  let add_pairs work_set caller pairs =
    let open Domain in
    CriticalPairs.iter (fun pair -> replace work_set (caller, pair) ()) pairs
end

let report exe_env work_set =
  let open Domain in
  let wrap_report (procname, (pair : CriticalPair.t)) () init =
    Summary.OnDisk.get procname
    |> Option.fold ~init ~f:(fun acc summary ->
           let pdesc = Summary.get_proc_desc summary in
           let tenv = Exe_env.get_tenv exe_env procname in
           let acc = report_on_pair tenv summary pair acc in
           match pair.elem.event with
           | LockAcquire lock ->
               let should_report_starvation =
                 CriticalPair.is_uithread pair && not (Procname.is_constructor procname)
               in
               WorkHashSet.fold
                 (fun (other_procname, (other_pair : CriticalPair.t)) () acc ->
                   report_on_parallel_composition ~should_report_starvation tenv pdesc pair lock
                     other_procname other_pair acc )
                 work_set acc
           | _ ->
               acc )
  in
  WorkHashSet.fold wrap_report work_set ReportMap.empty |> ReportMap.store_multi_file


let whole_program_analysis () =
  L.progress "Starvation whole program analysis starts.@." ;
  let work_set = WorkHashSet.create 1 in
  let exe_env = Exe_env.mk () in
  L.progress "Processing on-disk summaries...@." ;
  SpecsFiles.iter ~f:(iter_summary exe_env ~f:(WorkHashSet.add_pairs work_set)) ;
  L.progress "Loaded %d pairs@." (WorkHashSet.length work_set) ;
  L.progress "Reporting on processed summaries...@." ;
  report exe_env work_set
