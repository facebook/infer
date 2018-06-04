(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

let debug fmt = L.(debug Analysis Verbose fmt)

let is_on_ui_thread pn =
  RacerDConfig.(match Models.get_thread pn with Models.MainThread -> true | _ -> false)


module Payload = SummaryPayload.Make (struct
  type t = StarvationDomain.summary

  let update_payloads post (payloads: Payloads.t) = {payloads with starvation= Some post}

  let of_payloads (payloads: Payloads.t) = payloads.starvation
end)

(* using an indentifier for a class object, create an access path representing that lock;
   this is for synchronizing on class objects only *)
let lock_of_class class_id =
  let ident = Ident.create_normal class_id 0 in
  let type_name = Typ.Name.Java.from_string "java.lang.Class" in
  let typ = Typ.mk (Typ.Tstruct type_name) in
  let typ' = Typ.mk (Typ.Tptr (typ, Typ.Pk_pointer)) in
  AccessPath.of_id ident typ'


module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = StarvationDomain

  type extras = FormalMap.t

  let exec_instr (astate: Domain.astate) {ProcData.pdesc; tenv; extras} _ (instr: HilInstr.t) =
    let open RacerDConfig in
    let is_formal base = FormalMap.is_formal base extras in
    let get_path actuals =
      match actuals with
      | HilExp.AccessExpression access_exp :: _ -> (
        match AccessExpression.to_access_path access_exp with
        | (((Var.ProgramVar pvar, _) as base), _) as path
          when is_formal base || Pvar.is_global pvar ->
            Some (AccessPath.inner_class_normalize path)
        | _ ->
            (* ignore paths on local or logical variables *)
            None )
      | HilExp.Constant (Const.Cclass class_id) :: _ ->
          (* this is a synchronized/lock(CLASSNAME.class) construct *)
          Some (lock_of_class class_id)
      | _ ->
          None
    in
    let do_lock actuals loc astate =
      get_path actuals |> Option.value_map ~default:astate ~f:(Domain.acquire astate loc)
    in
    let do_unlock actuals astate =
      get_path actuals |> Option.value_map ~default:astate ~f:(Domain.release astate)
    in
    match instr with
    | Call (_, Direct callee, actuals, _, loc) -> (
      match Models.get_lock callee actuals with
      | Lock ->
          do_lock actuals loc astate
      | Unlock ->
          do_unlock actuals astate
      | LockedIfTrue ->
          astate
      | NoEffect when Models.is_synchronized_library_call tenv callee ->
          (* model a synchronized call without visible internal behaviour *)
          do_lock actuals loc astate |> do_unlock actuals
      | NoEffect when is_on_ui_thread callee ->
          let explanation = F.asprintf "it calls %a" (MF.wrap_monospaced Typ.Procname.pp) callee in
          Domain.set_on_ui_thread astate explanation
      | NoEffect ->
        match Models.may_block tenv callee actuals with
        | Some sev ->
            let caller = Procdesc.get_proc_name pdesc in
            Domain.blocking_call ~caller ~callee sev loc astate
        | None ->
            Payload.read pdesc callee
            |> Option.value_map ~default:astate ~f:(Domain.integrate_summary astate callee loc) )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "starvation"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Normal) (TransferFunctions)

let die_if_not_java proc_desc =
  let is_java =
    Procdesc.get_proc_name proc_desc |> Typ.Procname.get_language |> Language.(equal Java)
  in
  if not is_java then L.(die InternalError "Not supposed to run on non-Java code yet.")


let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  die_if_not_java proc_desc ;
  let pname = Procdesc.get_proc_name proc_desc in
  let formals = FormalMap.make proc_desc in
  let proc_data = ProcData.make proc_desc tenv formals in
  let initial =
    if not (Procdesc.is_java_synchronized proc_desc) then StarvationDomain.empty
    else
      let loc = Procdesc.get_loc proc_desc in
      let lock =
        match pname with
        | Typ.Procname.Java java_pname when Typ.Procname.Java.is_static java_pname ->
            (* this is crafted so as to match synchronized(CLASSNAME.class) constructs *)
            Typ.Procname.Java.get_class_type_name java_pname |> Typ.Name.name
            |> Ident.string_to_name |> lock_of_class |> Option.some
        | _ ->
            FormalMap.get_formal_base 0 formals |> Option.map ~f:(fun base -> (base, []))
      in
      Option.value_map lock ~default:StarvationDomain.empty
        ~f:(StarvationDomain.acquire StarvationDomain.empty loc)
  in
  let initial =
    RacerDConfig.Models.runs_on_ui_thread tenv proc_desc
    |> Option.value_map ~default:initial ~f:(StarvationDomain.set_on_ui_thread initial)
  in
  Analyzer.compute_post proc_data ~initial
  |> Option.value_map ~default:summary ~f:(fun astate -> Payload.update_summary astate summary)


let get_summaries_of_methods_in_class tenv clazz =
  let tstruct_opt = Tenv.lookup tenv clazz in
  let methods =
    Option.value_map tstruct_opt ~default:[] ~f:(fun tstruct -> tstruct.Typ.Struct.methods)
  in
  let summaries = List.rev_filter_map methods ~f:Ondemand.analyze_proc_name in
  List.rev_filter_map summaries ~f:(fun sum ->
      Option.map sum.Summary.payloads.Payloads.starvation ~f:(fun p ->
          (Summary.get_proc_name sum, p) ) )


(** per-procedure report map, which takes care of deduplication *)
module ReportMap = struct
  type issue_t = Starvation of StarvationDomain.Event.severity_t | Deadlock [@@deriving compare]

  type report_t =
    { issue: issue_t
    ; pname: Typ.Procname.t [@compare.ignore]
    ; ltr: Errlog.loc_trace [@compare.ignore]
    ; message: string [@compare.ignore] }
  [@@deriving compare]

  module LocMap = PrettyPrintable.MakePPMap (Location)

  let empty : report_t list LocMap.t = LocMap.empty

  let add issue pname loc ltr message map =
    let rep = {issue; pname; ltr; message} in
    let preexisting = try LocMap.find loc map with Caml.Not_found -> [] in
    LocMap.add loc (rep :: preexisting) map


  let add_deadlock pname loc ltr exn map = add Deadlock pname loc ltr exn map

  let add_starvation sev pname loc ltr exn map = add (Starvation sev) pname loc ltr exn map

  let log map =
    let log_report loc {issue; pname; ltr; message} =
      let issue_type =
        match issue with Deadlock -> IssueType.deadlock | Starvation _ -> IssueType.starvation
      in
      let exn = Exceptions.Checkers (issue_type, Localise.verbatim_desc message) in
      Reporting.log_issue_external pname Exceptions.Kerror ~loc ~ltr exn
    in
    let mk_deduped_report num_of_reports ({message} as report) =
      { report with
        message=
          Printf.sprintf "%s %d more starvation report(s) on the same line suppressed." message
            (num_of_reports - 1) }
    in
    let log_location loc reports =
      let deadlocks, starvations =
        List.partition_tf ~f:(function {issue= Deadlock} -> true | _ -> false) reports
      in
      (* report all deadlocks *)
      List.iter ~f:(log_report loc) deadlocks ;
      match starvations with
      | [] ->
          ()
      | [report] ->
          log_report loc report
      | _ ->
          List.max_elt ~compare:compare_report_t starvations
          |> Option.iter ~f:(fun rep ->
                 mk_deduped_report (List.length starvations) rep |> log_report loc )
    in
    LocMap.iter log_location map
end

let should_report_deadlock_on_current_proc current_elem endpoint_elem =
  let open StarvationDomain in
  match (current_elem.Order.first, current_elem.Order.eventually) with
  | {Event.elem= MayBlock _}, _ | _, {Event.elem= MayBlock _} ->
      (* should never happen *)
      L.die InternalError "Deadlock cannot occur without two lock events: %a" Order.pp current_elem
  | {Event.elem= LockAcquire ((Var.LogicalVar _, _), [])}, _ ->
      (* first elem is a class object (see [lock_of_class]), so always report because the
         reverse ordering on the events will not occur *)
      true
  | {Event.elem= LockAcquire ((Var.LogicalVar _, _), _ :: _)}, _
  | _, {Event.elem= LockAcquire ((Var.LogicalVar _, _), _)} ->
      (* first elem has an ident root, but has a non-empty access path, which means we are
         not filtering out local variables (see [exec_instr]), or,
         second elem has an ident root, which should not happen if we are filtering locals *)
      L.die InternalError "Deadlock cannot occur on these logical variables: %a @." Order.pp
        current_elem
  | {Event.elem= LockAcquire ((_, typ1), _)}, {Event.elem= LockAcquire ((_, typ2), _)} ->
      (* use string comparison on types as a stable order to decide whether to report a deadlock *)
      let c = String.compare (Typ.to_string typ1) (Typ.to_string typ2) in
      c < 0
      || Int.equal 0 c
         && (* same class, so choose depending on location *)
            Location.compare current_elem.Order.eventually.Event.loc
              endpoint_elem.Order.eventually.Event.loc
            < 0


(*  Note about how many times we report a deadlock: normally twice, at each trace starting point.
         Due to the fact we look for deadlocks in the summaries of the class at the root of a path,
         this will fail when (a) the lock is of class type (ie as used in static sync methods), because
         then the root is an identifier of type java.lang.Class and (b) when the lock belongs to an
         inner class but this is no longer obvious in the path, because of nested-class path normalisation.
         The net effect of the above issues is that we will only see these locks in conflicting pairs
         once, as opposed to twice with all other deadlock pairs. *)
let report_deadlocks tenv current_pdesc {StarvationDomain.order; ui} report_map' =
  let open StarvationDomain in
  let current_pname = Procdesc.get_proc_name current_pdesc in
  let report_endpoint_elem current_elem endpoint_pname elem report_map =
    if
      not
        ( Order.may_deadlock current_elem elem
        && should_report_deadlock_on_current_proc current_elem elem )
    then report_map
    else
      let () = debug "Possible deadlock:@.%a@.%a@." Order.pp current_elem Order.pp elem in
      match (current_elem.Order.eventually, elem.Order.eventually) with
      | {Event.elem= LockAcquire _}, {Event.elem= LockAcquire _} ->
          let error_message =
            Format.asprintf
              "Potential deadlock.@.Trace 1 (starts at %a), %a.@.Trace 2 (starts at %a), %a."
              (MF.wrap_monospaced Typ.Procname.pp)
              current_pname Order.pp current_elem
              (MF.wrap_monospaced Typ.Procname.pp)
              endpoint_pname Order.pp elem
          in
          let first_trace = Order.make_trace ~header:"[Trace 1] " current_pname current_elem in
          let second_trace = Order.make_trace ~header:"[Trace 2] " endpoint_pname elem in
          let ltr = first_trace @ second_trace in
          let loc = Order.get_loc current_elem in
          ReportMap.add_deadlock current_pname loc ltr error_message report_map
      | _, _ ->
          report_map
  in
  let report_on_current_elem elem report_map =
    match elem with
    | {Order.eventually= {Event.elem= Event.MayBlock _}} ->
        report_map
    | {Order.eventually= {Event.elem= Event.LockAcquire endpoint_lock}} ->
        Lock.owner_class endpoint_lock
        |> Option.value_map ~default:report_map ~f:(fun endpoint_class ->
               (* get the class of the root variable of the lock in the endpoint elem
                   and retrieve all the summaries of the methods of that class *)
               let endpoint_summaries = get_summaries_of_methods_in_class tenv endpoint_class in
               (* for each summary related to the endpoint, analyse and report on its pairs *)
               List.fold endpoint_summaries ~init:report_map ~f:
                 (fun acc (endp_pname, endpoint_summary) ->
                   let endp_order = endpoint_summary.order in
                   let endp_ui = endpoint_summary.ui in
                   if UIThreadDomain.is_empty ui || UIThreadDomain.is_empty endp_ui then
                     OrderDomain.fold (report_endpoint_elem elem endp_pname) endp_order acc
                   else acc ) )
  in
  OrderDomain.fold report_on_current_elem order report_map'


let report_starvation tenv current_pdesc {StarvationDomain.events; ui} report_map' =
  let open StarvationDomain in
  let current_pname = Procdesc.get_proc_name current_pdesc in
  let report_remote_block ui_explain event current_lock endpoint_pname endpoint_elem report_map =
    match (endpoint_elem.Order.first, endpoint_elem.Order.eventually) with
    | {Event.elem= Event.LockAcquire lock}, {Event.elem= Event.MayBlock (block_descr, sev)}
      when Lock.equal current_lock lock ->
        let error_message =
          Format.asprintf
            "Method %a runs on UI thread (because %s) and %a, which may be held by another thread \
             which %s."
            (MF.wrap_monospaced Typ.Procname.pp)
            current_pname ui_explain Lock.pp lock block_descr
        in
        let first_trace = Event.make_trace ~header:"[Trace 1] " current_pname event in
        let second_trace = Order.make_trace ~header:"[Trace 2] " endpoint_pname endpoint_elem in
        let ltr = first_trace @ second_trace in
        let loc = Event.get_loc event in
        ReportMap.add_starvation sev current_pname loc ltr error_message report_map
    | _ ->
        report_map
  in
  let report_on_current_elem ui_explain event report_map =
    match event.Event.elem with
    | Event.MayBlock (_, sev) ->
        let error_message =
          Format.asprintf "Method %a runs on UI thread (because %s), and may block; %a."
            (MF.wrap_monospaced Typ.Procname.pp)
            current_pname ui_explain Event.pp_no_trace event
        in
        let loc = Event.get_loc event in
        let ltr = Event.make_trace current_pname event in
        ReportMap.add_starvation sev current_pname loc ltr error_message report_map
    | Event.LockAcquire endpoint_lock ->
        Lock.owner_class endpoint_lock
        |> Option.value_map ~default:report_map ~f:(fun endpoint_class ->
               (* get the class of the root variable of the lock in the endpoint elem
                 and retrieve all the summaries of the methods of that class *)
               let endpoint_summaries = get_summaries_of_methods_in_class tenv endpoint_class in
               (* for each summary related to the endpoint, analyse and report on its pairs *)
               List.fold endpoint_summaries ~init:report_map ~f:
                 (fun acc (endpoint_pname, {order; ui}) ->
                   (* skip methods known to run on ui thread, as they cannot run in parallel to us *)
                   if UIThreadDomain.is_empty ui then
                     OrderDomain.fold
                       (report_remote_block ui_explain event endpoint_lock endpoint_pname)
                       order acc
                   else acc ) )
  in
  match ui with
  | AbstractDomain.Types.Bottom ->
      report_map'
  | AbstractDomain.Types.NonBottom ui_explain ->
      EventDomain.fold (report_on_current_elem ui_explain) events report_map'


let reporting {Callbacks.procedures; exe_env} =
  let report_procedure (tenv, proc_desc) =
    die_if_not_java proc_desc ;
    Payload.read proc_desc (Procdesc.get_proc_name proc_desc)
    |> Option.iter ~f:(fun summary ->
           report_deadlocks tenv proc_desc summary ReportMap.empty
           |> report_starvation tenv proc_desc summary |> ReportMap.log )
  in
  List.iter procedures ~f:report_procedure ;
  let sourcefile = exe_env.Exe_env.source_file in
  IssueLog.store Config.starvation_issues_dir_name sourcefile
