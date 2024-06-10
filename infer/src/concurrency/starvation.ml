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

type analysis_data =
  {interproc: StarvationDomain.summary InterproceduralAnalysis.t; formals: FormalMap.t}

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type nonrec analysis_data = analysis_data

  let log_parse_error error pname actuals =
    L.debug Analysis Verbose "%s pname:%a actuals:%a@." error Procname.pp pname
      (PrettyPrintable.pp_collection ~pp_item:HilExp.pp)
      actuals


  let hilexp_of_sil ~add_deref (astate : Domain.t) silexp typ =
    (* If there is a structure like this
     * varA -> varB, varB -> SomeConstantLock (static class)
     * and there is code unlock(varB), we want to varB to resolve to SomeConstantLock
     * Unfortuantely, this can't be done directly, because SomeConstantLock is not an access expression
     * so we do it via side effects of constant_var *)
    let constant_var = ref None in
    let f_resolve_id var =
      Domain.VarDomain.get var astate.var_state
      |> Option.bind ~f:(function
           | StarvationDomain.AccessExpressionOrConst.AE exp ->
               Some exp
           | StarvationDomain.AccessExpressionOrConst.Const exp ->
               ( match !constant_var with
               | None ->
                   ()
               | Some _e ->
                   L.internal_error "Double resolved id %a in expression %a" (Const.pp Pp.text) exp
                     Exp.pp silexp ) ;
               constant_var := Some exp ;
               None )
    in
    let hil = HilExp.of_sil ~include_array_indexes:false ~f_resolve_id ~add_deref silexp typ in
    let hil = match !constant_var with None -> hil | Some c -> HilExp.Constant c in
    hil


  let hilexp_of_sils ~add_deref astate silexps =
    List.map silexps ~f:(fun (exp, typ) -> hilexp_of_sil ~add_deref astate exp typ)


  let rec get_access_expr_or_const (hilexp : HilExp.t) =
    match hilexp with
    | AccessExpression access_exp ->
        Some (Domain.AccessExpressionOrConst.AE access_exp)
    | Constant c ->
        Some (Domain.AccessExpressionOrConst.Const c)
    | Cast (_, hilexp) | Exception hilexp | UnaryOperator (_, hilexp, _) ->
        get_access_expr_or_const hilexp
    | BinaryOperator _ | Closure _ | Sizeof _ ->
        None


  let get_access_expr (hilexp : HilExp.t) =
    match get_access_expr_or_const hilexp with
    | Some (Domain.AccessExpressionOrConst.AE access_exp) ->
        Some access_exp
    | _ ->
        None


  let get_access_expr_list actuals = List.map actuals ~f:get_access_expr

  let do_assume formals assume_exp (astate : Domain.t) =
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
    let astate =
      match HilExp.get_access_exprs assume_exp with
      | [access_expr] when AttributeDomain.is_thread_guard access_expr astate.attributes ->
          HilExp.eval_boolean_exp access_expr assume_exp
          |> Option.fold ~init:astate ~f:add_thread_choice
      | [access_expr] when AttributeDomain.is_future_done_guard access_expr astate.attributes ->
          HilExp.eval_boolean_exp access_expr assume_exp
          |> Option.fold ~init:astate ~f:(add_future_done_choice access_expr)
      | _ ->
          astate
    in
    match (assume_exp : HilExp.t) with
    | (BinaryOperator (Eq, exp, null) | UnaryOperator (LNot, BinaryOperator (Ne, exp, null), _))
      when HilExp.is_null_literal null ->
        StarvationDomain.null_check formals exp astate
    | (BinaryOperator (Eq, null, exp) | UnaryOperator (LNot, BinaryOperator (Ne, null, exp), _))
      when HilExp.is_null_literal null ->
        StarvationDomain.null_check formals exp astate
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
      | Some runnable :: _ when StarvationModels.schedules_first_arg_on_ui_thread tenv callee ->
          Some (runnable, StarvationModels.ForUIThread)
      | _ :: Some runnable :: _ when StarvationModels.schedules_second_arg_on_ui_thread tenv callee
        ->
          Some (runnable, StarvationModels.ForUIThread)
      | Some runnable :: _ when StarvationModels.schedules_first_arg_on_bg_thread tenv callee ->
          Some (runnable, StarvationModels.ForNonUIThread)
      | _ ->
          None
    in
    Option.value_map work_opt ~default:astate ~f:schedule_work


  let do_assignment tenv formals lhs_access_exp rhs_exp (astate : Domain.t) =
    let astate =
      get_access_expr rhs_exp
      |> Option.bind ~f:(fun exp -> get_exp_attributes tenv exp astate)
      |> Option.value_map ~default:astate ~f:(fun attribute ->
             let attributes =
               Domain.AttributeDomain.add lhs_access_exp attribute astate.attributes
             in
             {astate with attributes} )
    in
    if HilExp.is_null_literal rhs_exp then astate
    else Domain.set_non_null formals lhs_access_exp astate


  let do_call {interproc= {proc_desc; tenv; analyze_dependency}; formals} lhs callee actuals loc
      (astate : Domain.t) =
    let open Domain in
    let procname = Procdesc.get_proc_name proc_desc in
    let make_ret_attr return_attribute = {empty_summary with return_attribute} in
    let make_thread thread = {empty_summary with thread} in
    let actuals_acc_exps = get_access_expr_list actuals in
    let get_returned_executor_summary () =
      StarvationModels.get_returned_executor tenv callee actuals
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
    let get_callee_summary () = analyze_dependency callee |> AnalysisResult.to_option in
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
        List.hd actuals |> Option.map ~f:(fun exp -> do_assume formals exp astate)
      else None
    in
    let treat_arbitrary_code_exec () =
      if StarvationModels.may_execute_arbitrary_code tenv callee actuals then
        StarvationDomain.arbitrary_code_execution ~callee ~loc astate |> Option.some
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
             let subst = Lock.make_subst formals actuals in
             let callsite = CallSite.make callee loc in
             Domain.integrate_summary ~tenv ~procname ~lhs ~subst formals callsite astate summary )
    in
    IList.eval_until_first_some
      [ treat_handler_constructor
      ; treat_thread_constructor
      ; treat_assume
      ; treat_arbitrary_code_exec
      ; treat_modeled_summaries ]
    |> Option.value ~default:astate


  let do_metadata (metadata : Sil.instr_metadata) astate =
    match metadata with ExitScope (vars, _) -> Domain.remove_dead_vars astate vars | _ -> astate


  let do_load tenv formals ~lhs rhs_exp rhs_typ (astate : Domain.t) =
    let lhs_var = fst lhs in
    let add_deref = match (lhs_var : Var.t) with LogicalVar _ -> true | ProgramVar _ -> false in
    let rhs_hil_exp = hilexp_of_sil ~add_deref astate rhs_exp rhs_typ in
    let astate =
      get_access_expr_or_const rhs_hil_exp
      |> Option.value_map ~default:astate ~f:(fun acc_exp ->
             {astate with var_state= Domain.VarDomain.set lhs_var acc_exp astate.var_state} )
    in
    let lhs_hil_acc_exp = HilExp.AccessExpression.base lhs in
    do_assignment tenv formals lhs_hil_acc_exp rhs_hil_exp astate


  let do_cast tenv formals id base_typ actuals astate =
    match actuals with
    | [(e, typ); _sizeof] ->
        do_load tenv formals ~lhs:(Var.of_id id, base_typ) e typ astate
    | _ ->
        astate


  let check_function_pointer_model fn_ptr =
    match Config.starvation_c_function_pointer_models with
    | `Assoc models ->
        let pointer_name : string = Format.asprintf "%a" HilExp.pp fn_ptr in
        List.find_map
          ~f:(function
            | model_pointer_name, `String model_target
              when String.equal model_pointer_name pointer_name ->
                L.debug Analysis Verbose "Mapping %s -> %s" pointer_name model_target ;
                Some model_target
            | _ ->
                None )
          models
    | _ ->
        None


  let do_function_pointer_call analysis_data loc id typ fn_ptr sil_actuals astate =
    let ret_base = (Var.of_id id, typ) in
    let ret_exp = HilExp.AccessExpression.base ret_base in
    let actuals = hilexp_of_sils ~add_deref:false astate sil_actuals in
    let astate =
      match hilexp_of_sils ~add_deref:false astate [fn_ptr] with
      | [] ->
          astate
      | fn_ptr :: _rest -> (
        match check_function_pointer_model fn_ptr with
        | Some model_target ->
            let callee = Procname.from_string_c_fun model_target in
            do_call analysis_data ret_exp callee actuals loc astate
        | None ->
            astate )
    in
    astate


  let exec_instr (astate : Domain.t) ({interproc= {proc_desc; tenv}; formals} as analysis_data) _ _
      instr =
    let open ConcurrencyModels in
    let open StarvationModels in
    let get_lock_path = Domain.Lock.make formals in
    let procname = Procdesc.get_proc_name proc_desc in
    let is_java = Procname.is_java procname in
    let do_lock locks loc astate =
      List.filter_map ~f:get_lock_path locks |> Domain.acquire ~tenv astate ~procname ~loc
    in
    let do_unlock locks astate = List.filter_map ~f:get_lock_path locks |> Domain.release astate in
    match (instr : Sil.instr) with
    | Metadata metadata ->
        do_metadata metadata astate
    | Prune (exp, _loc, _then_branch, _if_kind) ->
        let hil_exp = hilexp_of_sil ~add_deref:false astate exp StdTyp.boolean in
        do_assume formals hil_exp astate
    | Load {id} when Ident.is_none id ->
        astate
    | Load {id; e; typ} ->
        do_load tenv formals ~lhs:(Var.of_id id, typ) e typ astate
    | Store {e1= Lvar lhs_pvar; typ; e2} when Pvar.is_ssa_frontend_tmp lhs_pvar ->
        do_load tenv formals ~lhs:(Var.of_pvar lhs_pvar, typ) e2 typ astate
    | Store {e1; typ; e2} ->
        let rhs_hil_exp = hilexp_of_sil ~add_deref:false astate e2 typ in
        hilexp_of_sil ~add_deref:true astate e1 (Typ.mk_ptr typ)
        |> get_access_expr
        |> Option.value_map ~default:astate ~f:(fun lhs_hil_acc_exp ->
               do_assignment tenv formals lhs_hil_acc_exp rhs_hil_exp astate )
    | Call (_, Const (Cfun callee), actuals, _, _)
      when should_skip_analysis tenv callee (hilexp_of_sils ~add_deref:false astate actuals) ->
        astate
    | Call ((id, base_typ), Const (Cfun callee), actuals, _, _)
      when Procname.equal callee BuiltinDecl.__cast ->
        do_cast tenv formals id base_typ actuals astate
    | Call ((id, typ), Const (Cfun callee), fn_ptr :: fn_args, loc, _)
      when Procname.equal callee BuiltinDecl.__call_c_function_ptr ->
        do_function_pointer_call analysis_data loc id typ fn_ptr fn_args astate
    | Call ((id, typ), Const (Cfun callee), sil_actuals, loc, _) -> (
        let ret_base = (Var.of_id id, typ) in
        let actuals = hilexp_of_sils ~add_deref:false astate sil_actuals in
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
            Domain.wait_on_monitor ~loc formals actuals astate
        | NoEffect when is_java && is_future_get tenv callee actuals ->
            Domain.future_get ~callee ~loc actuals astate
        | NoEffect when is_java ->
            let ret_exp = HilExp.AccessExpression.base ret_base in
            let astate = do_work_scheduling tenv callee actuals loc astate in
            if may_block tenv callee actuals then Domain.blocking_call ~callee ~loc astate
            else if may_do_ipc tenv callee actuals then Domain.ipc ~callee ~loc astate
            else if is_regex_op tenv callee actuals then Domain.regex_op ~callee ~loc astate
            else do_call analysis_data ret_exp callee actuals loc astate
        | NoEffect ->
            (* in C++/Obj C we only care about deadlocks, not starvation errors *)
            let ret_exp = HilExp.AccessExpression.base ret_base in
            do_call analysis_data ret_exp callee actuals loc astate )
    | Call ((id, _), _, _, _, _) ->
        (* call havocs LHS *)
        Domain.remove_dead_vars astate [Var.of_id id]


  let pp_session_name _node fmt = F.pp_print_string fmt "starvation"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Normal))

(** Compute the attributes (of static variables) set up by the class initializer. *)
let set_class_init_attributes procname (astate : Domain.t) =
  let open Domain in
  let attributes =
    ConcurrencyUtils.get_java_class_initializer_summary_of procname
    |> Option.value_map ~default:AttributeDomain.top ~f:(fun summary -> summary.attributes)
  in
  ({astate with attributes} : t)


(** Compute the attributes of instance variables that all constructors agree on. *)
let set_constructor_attributes ({InterproceduralAnalysis.proc_desc} as interproc) (astate : Domain.t)
    =
  let procname = Procdesc.get_proc_name proc_desc in
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
    ConcurrencyUtils.get_java_constructor_summaries_of interproc
    (* make instances of [this] local to the current procedure and select only the attributes *)
    |> List.map ~f:(fun (summary : Domain.summary) -> localize_attrs summary.attributes)
    (* join all the attribute maps together *)
    |> List.reduce ~f:AttributeDomain.join
    |> Option.value ~default:AttributeDomain.top
  in
  {astate with attributes}


let set_initial_attributes ({InterproceduralAnalysis.proc_desc} as interproc) astate =
  let procname = Procdesc.get_proc_name proc_desc in
  if not Config.starvation_whole_program then astate
  else
    match procname with
    | Procname.Java java_pname when Procname.Java.is_class_initializer java_pname ->
        (* we are analyzing the class initializer, don't go through on-demand again *)
        astate
    | Procname.Java java_pname when Procname.Java.(is_constructor java_pname || is_static java_pname)
      ->
        (* analyzing a constructor or static method, so we need the attributes established by the
           class initializer *)
        set_class_init_attributes interproc astate
    | Procname.Java _ ->
        (* we are analyzing an instance method, so we need constructor-established attributes
           which will include those by the class initializer *)
        set_constructor_attributes interproc astate
    | _ ->
        astate


let analyze_procedure ({InterproceduralAnalysis.proc_desc; tenv} as interproc) =
  let procname = Procdesc.get_proc_name proc_desc in
  if StarvationModels.should_skip_analysis tenv procname [] then None
  else
    let formals = FormalMap.make (Procdesc.get_attributes proc_desc) in
    let proc_data = {interproc; formals} in
    let loc = Procdesc.get_loc proc_desc in
    let locks_for_synchronized_proc =
      if Procdesc.is_java_synchronized proc_desc || Procdesc.is_csharp_synchronized proc_desc then
        Domain.Lock.make_java_synchronized formals procname |> Option.to_list
      else []
    in
    let set_lock_state_for_synchronized_proc astate =
      Domain.acquire ~tenv astate ~procname ~loc locks_for_synchronized_proc
    in
    let release_lock_state_for_synchronized_proc astate =
      Domain.release astate locks_for_synchronized_proc
    in
    let set_thread_status_by_annotation (astate : Domain.t) =
      let thread =
        if ConcurrencyModels.annotated_as_worker_thread tenv procname then
          Domain.ThreadDomain.BGThread
        else if ConcurrencyModels.runs_on_ui_thread tenv procname then Domain.ThreadDomain.UIThread
        else
          ConcurrencyModels.annotated_as_named_thread procname
          |> Option.value_map ~f:(fun n -> Domain.ThreadDomain.NamedThread n) ~default:astate.thread
      in
      {astate with thread}
    in
    let set_ignore_blocking_calls_flag astate =
      if StarvationModels.is_annotated_nonblocking tenv procname then
        Domain.set_ignore_blocking_calls_flag astate
      else astate
    in
    let initial =
      Domain.initial
      (* set the attributes of instance variables set up by all constructors or the class initializer *)
      |> set_initial_attributes interproc
      |> set_lock_state_for_synchronized_proc |> set_thread_status_by_annotation
      |> set_ignore_blocking_calls_flag
    in
    Analyzer.compute_post proc_data ~initial proc_desc
    |> Option.map ~f:release_lock_state_for_synchronized_proc
    |> Option.map ~f:(Domain.summary_of_astate proc_desc)


(** per-file report map, which takes care of deduplication *)
module ReportMap : sig
  type t

  val empty : t

  type report_add_t =
    Tenv.t -> ProcAttributes.t -> Location.t -> Errlog.loc_trace -> string -> t -> t

  val add_arbitrary_code_execution_under_lock : report_add_t

  val add_deadlock : report_add_t

  val add_ipc : report_add_t

  val add_lockless_violation : report_add_t

  val add_regex_op : report_add_t

  val add_starvation : report_add_t

  val add_strict_mode_violation : report_add_t

  val issue_log_of : t -> IssueLog.t

  val store_multi_file : t -> unit
  (** generate and store issue logs for all source files involved in this report map; for use in the
      whole-program mode only *)
end = struct
  type report_t =
    {issue_type: IssueType.t; pname: Procname.t; depth: int; ltr: Errlog.loc_trace; message: string}

  type t = report_t list IssueType.Map.t Location.Map.t

  type report_add_t =
    Tenv.t -> ProcAttributes.t -> Location.t -> Errlog.loc_trace -> string -> t -> t

  let empty : t = Location.Map.empty

  let add tenv pattrs loc ltr message issue_type loc_map =
    if Reporting.is_suppressed tenv pattrs issue_type then loc_map
    else
      let pname = ProcAttributes.get_proc_name pattrs in
      let report = {issue_type; pname; ltr; message; depth= -List.length ltr} in
      Location.Map.update loc
        (fun issue_map_opt ->
          let issue_map = Option.value issue_map_opt ~default:IssueType.Map.empty in
          IssueType.Map.update issue_type
            (fun reports_opt ->
              let reports = Option.value reports_opt ~default:[] in
              Some (report :: reports) )
            issue_map
          |> Option.some )
        loc_map


  let add_deadlock tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.deadlock map


  let add_ipc tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.ipc_on_ui_thread map


  let add_regex_op tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.regex_op_on_ui_thread map


  let add_starvation tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.starvation map


  let add_strict_mode_violation tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.strict_mode_violation map


  let add_lockless_violation tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.lockless_violation map


  let add_arbitrary_code_execution_under_lock tenv pattrs loc ltr message map =
    add tenv pattrs loc ltr message IssueType.arbitrary_code_execution_under_lock map


  let deduplicated_issue_order =
    IssueType.
      [ deadlock
      ; lockless_violation
      ; ipc_on_ui_thread
      ; regex_op_on_ui_thread
      ; starvation
      ; strict_mode_violation
      ; arbitrary_code_execution_under_lock ]


  let issue_log_of loc_map =
    let log_report loc issue_log {issue_type; pname; ltr; message} =
      Reporting.log_issue_external ~issue_log pname ~loc ~ltr Starvation issue_type message
    in
    let mk_deduped_report ({message} as report) =
      { report with
        message= Printf.sprintf "%s Additional report(s) on the same line were suppressed." message
      }
    in
    let compare_reports r r' =
      match Int.compare r.depth r'.depth with
      | 0 ->
          String.compare r.message r'.message
      | result ->
          result
    in
    let log_reports loc issue_map issue_log issue =
      let reports = IssueType.Map.find_opt issue issue_map |> Option.value ~default:[] in
      if Config.deduplicate then
        let rep_opt =
          match reports with
          | [] ->
              None
          | [report] ->
              Some report
          | reports ->
              List.max_elt ~compare:compare_reports reports |> Option.map ~f:mk_deduped_report
        in
        Option.fold rep_opt ~init:issue_log ~f:(log_report loc)
      else List.fold reports ~init:issue_log ~f:(log_report loc)
    in
    let log_location loc issue_map issue_log =
      List.fold deduplicated_issue_order ~init:issue_log ~f:(log_reports loc issue_map)
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
      (fun file loc_map -> issue_log_of loc_map |> IssueLog.store ~checker:Starvation ~file)
      source_map
end

let should_report_deadlock_on_current_proc current_elem endpoint_elem =
  let open Domain in
  (not Config.deduplicate)
  ||
  match (endpoint_elem.CriticalPair.elem.event, current_elem.CriticalPair.elem.event) with
  | _, (Ipc _ | MayBlock _ | MonitorWait _ | MustNotOccurUnderLock _ | RegexOp _ | StrictModeCall _)
  | (Ipc _ | MayBlock _ | MonitorWait _ | MustNotOccurUnderLock _ | RegexOp _ | StrictModeCall _), _
    ->
      (* should never happen *)
      L.die InternalError "Deadlock cannot occur without two lock events: %a" CriticalPair.pp
        current_elem
  | LockAcquire {locks= endpoint_locks}, LockAcquire {locks= current_locks} -> (
      (* first elem is a class object (see [lock_of_class]), so always report because the
         reverse ordering on the events will not occur since we don't search the class for static locks *)
      List.exists ~f:Lock.is_class_object endpoint_locks
      ||
      match List.compare Lock.compare_wrt_reporting endpoint_locks current_locks with
      | 0 ->
          Location.compare current_elem.CriticalPair.loc endpoint_elem.CriticalPair.loc < 0
      | c ->
          c < 0 )


let should_report attrs =
  let procname = ProcAttributes.get_proc_name attrs in
  let should_report' procname =
    match procname with
    | Procname.Java java_pname ->
        (not (Procname.Java.is_autogen_method java_pname))
        && not (Procname.Java.is_class_initializer java_pname)
    | Procname.ObjC_Cpp _ ->
        true
    | Procname.C _ ->
        true
    | _ ->
        false
  in
  should_report' procname


let fold_reportable_summaries analyze_ondemand tenv clazz ~init ~f =
  let methods =
    Tenv.lookup tenv clazz
    |> Option.value_map ~default:[] ~f:(fun tstruct -> tstruct.Struct.methods)
  in
  let f acc mthd =
    Attributes.load mthd
    |> Option.value_map ~default:acc ~f:(fun other_attrs ->
           if should_report other_attrs then
             analyze_ondemand mthd
             |> Option.map ~f:(fun payload -> (mthd, payload))
             |> Option.fold ~init:acc ~f
           else acc )
  in
  List.fold methods ~init ~f


let is_private attrs = ProcAttributes.equal_access (ProcAttributes.get_access attrs) Private

(* Note about how many times we report a deadlock: normally twice, at each trace starting point.
   Due to the fact we look for deadlocks in the summaries of the class at the root of a path,
   this will fail when (a) the lock is of class type (ie as used in static sync methods), because
   then the root is an identifier of type java.lang.Class and (b) when the lock belongs to an
   inner class but this is no longer obvious in the path, because of nested-class path normalisation.
   The net effect of the above issues is that we will only see these locks in conflicting pairs
   once, as opposed to twice with all other deadlock pairs. *)

(** report warnings possible on the parallel composition of two threads/critical pairs
    [should_report_starvation] means [pair] is on the UI thread and not on a constructor *)
let report_on_parallel_composition ~should_report_starvation tenv pattrs pair lock other_pname
    other_pair report_map =
  if is_private pattrs || Attributes.load other_pname |> Option.exists ~f:is_private then report_map
  else
    let open Domain in
    let pname = ProcAttributes.get_proc_name pattrs in
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
      | (Ipc _ | MayBlock _ | RegexOp _) as event
        when should_report_starvation
             && Acquisitions.lock_is_held_in_other_thread tenv lock acquisitions ->
          let error_message =
            Format.asprintf
              "%a runs on UI thread and%a, which may be held by another thread which %a. This may \
               regress scroll performance or cause ANRs."
              pname_pp pname Lock.pp_locks lock Event.describe event
          in
          let ltr, loc = make_trace_and_loc () in
          ReportMap.add_starvation tenv pattrs loc ltr error_message report_map
      | MonitorWait {lock= monitor_lock}
        when should_report_starvation
             && Acquisitions.lock_is_held_in_other_thread tenv lock acquisitions
             && not (Lock.equal lock monitor_lock) ->
          let error_message =
            Format.asprintf
              "%a runs on UI thread and%a, which may be held by another thread which %a. This may \
               regress scroll performance or cause ANRs."
              pname_pp pname Lock.pp_locks lock Event.describe other_pair.CriticalPair.elem.event
          in
          let ltr, loc = make_trace_and_loc () in
          ReportMap.add_starvation tenv pattrs loc ltr error_message report_map
      | LockAcquire _ -> (
        match CriticalPair.may_deadlock tenv ~lhs:pair ~lhs_lock:lock ~rhs:other_pair with
        | Some other_lock when should_report_deadlock_on_current_proc pair other_pair ->
            let error_message =
              Format.asprintf
                "%a (Trace 1) and %a (Trace 2) acquire locks %a and %a in reverse orders." pname_pp
                pname pname_pp other_pname Lock.describe lock Lock.describe other_lock
            in
            let ltr, loc = make_trace_and_loc () in
            ReportMap.add_deadlock tenv pattrs loc ltr error_message report_map
        | _ ->
            report_map )
      | _ ->
          report_map
    else report_map


let report_on_pair ~analyze_ondemand tenv pattrs (pair : Domain.CriticalPair.t) report_map =
  let open Domain in
  let pname = ProcAttributes.get_proc_name pattrs in
  let event = pair.elem.event in
  let should_report_starvation =
    CriticalPair.is_uithread pair && not (Procname.is_constructor pname)
  in
  let is_not_private = not (is_private pattrs) in
  let make_trace_and_loc () =
    let loc = CriticalPair.get_loc pair in
    let ltr = CriticalPair.make_trace ~include_acquisitions:false pname pair in
    (ltr, loc)
  in
  match event with
  | Ipc _ when is_not_private && should_report_starvation ->
      let error_message =
        Format.asprintf
          "%a runs on UI thread and may perform blocking IPC, potentially regressing scroll \
           performance or causing ANRs; %a."
          pname_pp pname Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_ipc tenv pattrs loc ltr error_message report_map
  | MayBlock _ when is_not_private && should_report_starvation ->
      let error_message =
        Format.asprintf
          "%a runs on UI thread and may block, potentially regressing scroll performance or \
           causing ANRs; %a."
          pname_pp pname Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_starvation tenv pattrs loc ltr error_message report_map
  | MonitorWait _ when is_not_private && should_report_starvation ->
      let error_message =
        Format.asprintf
          "%a runs on UI thread and may block, potentially regressing scroll performance or \
           causing ANRs; %a."
          pname_pp pname Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_starvation tenv pattrs loc ltr error_message report_map
  | RegexOp _ when is_not_private && should_report_starvation ->
      let error_message =
        Format.asprintf
          "%a runs on UI thread and may perform costly regular expression operations, potentially \
           regressing scroll performance; %a."
          pname_pp pname Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_regex_op tenv pattrs loc ltr error_message report_map
  | StrictModeCall _ when is_not_private && should_report_starvation ->
      let error_message =
        Format.asprintf "%a runs on UI thread and may violate Strict Mode; %a." pname_pp pname
          Event.describe event
      in
      let ltr, loc = make_trace_and_loc () in
      ReportMap.add_strict_mode_violation tenv pattrs loc ltr error_message report_map
  | MustNotOccurUnderLock _ when not (Acquisitions.is_empty pair.elem.acquisitions) -> (
      (* warn only at the innermost procedure taking a lock around the final call *)
      let procs_with_acquisitions =
        Acquisitions.fold
          (fun (acquisition : Acquisition.t) acc -> Procname.Set.add acquisition.elem.procname acc)
          pair.elem.acquisitions Procname.Set.empty
      in
      match Procname.Set.is_singleton_or_more procs_with_acquisitions with
      | IContainer.Empty ->
          L.die InternalError "Found empty set of acquisitions after checking for non-emptiness.@\n"
      | IContainer.More ->
          (* acquisitions found in more than one proc, ignore *)
          report_map
      | IContainer.Singleton acquiring_pname when not (Procname.equal acquiring_pname pname) ->
          (* we are at a caller of the acquiring procname, so ignore *)
          report_map
      | IContainer.Singleton _ ->
          let error_message =
            Format.asprintf
              "%a %a under a lock; executed code may acquire arbitrary locks leading to potential \
               deadlock."
              pname_pp pname Event.describe event
          in
          let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
          let ltr = CriticalPair.make_trace pname pair in
          ReportMap.add_arbitrary_code_execution_under_lock tenv pattrs loc ltr error_message
            report_map )
  | LockAcquire _ when is_not_private && StarvationModels.is_annotated_lockless tenv pname ->
      let error_message =
        Format.asprintf "%a is annotated %s but%a." pname_pp pname
          (MF.monospaced_to_string Annotations.lockless)
          Event.describe event
      in
      let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
      let ltr = CriticalPair.make_trace pname pair in
      ReportMap.add_lockless_violation tenv pattrs loc ltr error_message report_map
  | LockAcquire {locks} when is_not_private -> (
    match
      List.find locks ~f:(fun lock -> Acquisitions.lock_is_held lock pair.elem.acquisitions)
    with
    | Some lock ->
        let error_message =
          Format.asprintf "Potential self deadlock. %a%a twice." pname_pp pname Lock.pp_locks lock
        in
        let loc = CriticalPair.get_earliest_lock_or_call_loc ~procname:pname pair in
        let ltr = CriticalPair.make_trace ~header:"In method " pname pair in
        ReportMap.add_deadlock tenv pattrs loc ltr error_message report_map
    | None when Config.starvation_whole_program ->
        report_map
    | None ->
        List.fold locks ~init:report_map ~f:(fun acc lock ->
            Lock.root_class lock
            |> Option.value_map ~default:acc ~f:(fun other_class ->
                   (* get the class of the root variable of the lock in the lock acquisition
                      and retrieve all the summaries of the methods of that class;
                      then, report on the parallel composition of the current pair and any pair in these
                      summaries that can indeed run in parallel *)
                   fold_reportable_summaries analyze_ondemand tenv other_class ~init:acc
                     ~f:(fun acc (other_pname, summary) ->
                       Domain.fold_critical_pairs_of_summary
                         (report_on_parallel_composition ~should_report_starvation tenv pattrs pair
                            lock other_pname )
                         summary acc ) ) ) )
  | _ ->
      report_map


let reporting {InterproceduralAnalysis.procedures; file_exe_env; analyze_file_dependency} =
  if Config.starvation_whole_program then IssueLog.empty
  else
    let report_on_proc tenv pattrs report_map payload =
      Domain.fold_critical_pairs_of_summary
        (report_on_pair
           ~analyze_ondemand:(fun proc_name ->
             analyze_file_dependency proc_name |> AnalysisResult.to_option )
           tenv pattrs )
        payload report_map
    in
    let report_procedure report_map procname =
      match Attributes.load procname with
      | None ->
          report_map
      | Some attributes ->
          analyze_file_dependency procname |> AnalysisResult.to_option
          |> Option.value_map ~default:report_map ~f:(fun summary ->
                 let tenv = Exe_env.get_proc_tenv file_exe_env procname in
                 if should_report attributes then report_on_proc tenv attributes report_map summary
                 else report_map )
    in
    List.fold procedures ~init:ReportMap.empty ~f:report_procedure |> ReportMap.issue_log_of
