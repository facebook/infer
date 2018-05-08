(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format
module L = Logging

let debug fmt = L.(debug Analysis Verbose fmt)

let is_java_static pname =
  match pname with
  | Typ.Procname.Java java_pname ->
      Typ.Procname.Java.is_static java_pname
  | _ ->
      false


let is_on_main_thread pn =
  RacerDConfig.(match Models.get_thread pn with Models.MainThread -> true | _ -> false)


module Summary = Summary.Make (struct
  type payload = StarvationDomain.summary

  let update_payload post (summary: Specs.summary) =
    {summary with payload= {summary.payload with starvation= Some post}}


  let read_payload (summary: Specs.summary) = summary.payload.starvation
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
    match instr with
    | Call (_, Direct callee, actuals, _, loc) -> (
      match Models.get_lock callee actuals with
      | Lock ->
          get_path actuals |> Option.value_map ~default:astate ~f:(Domain.acquire astate loc)
      | Unlock ->
          get_path actuals |> Option.value_map ~default:astate ~f:(Domain.release astate)
      | LockedIfTrue ->
          astate
      | NoEffect ->
          if Models.may_block tenv callee actuals then
            let caller = Procdesc.get_proc_name pdesc in
            Domain.blocking_call ~caller ~callee loc astate
          else if is_on_main_thread callee then Domain.set_on_main_thread astate
          else
            Summary.read_summary pdesc callee
            |> Option.value_map ~default:astate ~f:(Domain.integrate_summary astate callee loc) )
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "starvation"
end

module Analyzer = LowerHil.MakeAbstractInterpreter (ProcCfg.Normal) (TransferFunctions)

let get_class_of_pname = function
  | Typ.Procname.Java java_pname ->
      Some (Typ.Procname.Java.get_class_type_name java_pname)
  | _ ->
      None


let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  let pname = Procdesc.get_proc_name proc_desc in
  let formals = FormalMap.make proc_desc in
  let proc_data = ProcData.make proc_desc tenv formals in
  let initial =
    if not (Procdesc.is_java_synchronized proc_desc) then StarvationDomain.empty
    else
      let loc = Procdesc.get_loc proc_desc in
      let lock =
        if is_java_static pname then
          (* this is crafted so as to match synchronized(CLASSNAME.class) constructs *)
          get_class_of_pname pname
          |> Option.map ~f:(fun tn -> Typ.Name.name tn |> Ident.string_to_name |> lock_of_class)
        else FormalMap.get_formal_base 0 formals |> Option.map ~f:(fun base -> (base, []))
      in
      Option.value_map lock ~default:StarvationDomain.empty
        ~f:(StarvationDomain.acquire StarvationDomain.empty loc)
  in
  let initial =
    if RacerDConfig.Models.runs_on_ui_thread proc_desc then
      StarvationDomain.set_on_main_thread initial
    else initial
  in
  Analyzer.compute_post proc_data ~initial
  |> Option.value_map ~default:summary ~f:(fun lock_state ->
         let lock_order = StarvationDomain.to_summary lock_state in
         Summary.update_summary lock_order summary )


let get_summary caller_pdesc callee_pdesc =
  Summary.read_summary caller_pdesc (Procdesc.get_proc_name callee_pdesc)
  |> Option.map ~f:(fun summary -> (callee_pdesc, summary))


let make_trace_with_header ?(header= "") elem start_loc pname =
  let trace = StarvationDomain.LockOrder.make_loc_trace elem in
  let trace_descr = Format.asprintf "%s %a" header Typ.Procname.pp pname in
  Errlog.make_trace_element 0 start_loc trace_descr [] :: trace


let make_loc_trace pname trace_id start_loc elem =
  let header = Printf.sprintf "[Trace %d] " trace_id in
  make_trace_with_header ~header elem start_loc pname


let get_summaries_of_methods_in_class get_proc_desc tenv current_pdesc clazz =
  let tstruct_opt = Tenv.lookup tenv clazz in
  let methods =
    Option.value_map tstruct_opt ~default:[] ~f:(fun tstruct -> tstruct.Typ.Struct.methods)
  in
  let pdescs = List.rev_filter_map methods ~f:get_proc_desc in
  List.rev_filter_map pdescs ~f:(get_summary current_pdesc)


let log_issue current_pname current_loc ltr exn =
  Reporting.log_issue_external current_pname Exceptions.Kerror ~loc:current_loc ~ltr exn


(*  Note about how many times we report a deadlock: normally twice, at each trace starting point.
         Due to the fact we look for deadlocks in the summaries of the class at the root of a path,
         this will fail when (a) the lock is of class type (ie as used in static sync methods), because
         then the root is an identifier of type java.lang.Class and (b) when the lock belongs to an
         inner class but this is no longer obvious in the path, because of nested-class path normalisation.
         The net effect of the above issues is that we will only see these locks in conflicting pairs
         once, as opposed to twice with all other deadlock pairs. *)
let report_deadlocks get_proc_desc tenv current_pdesc (summary, current_main) =
  let open StarvationDomain in
  let current_loc = Procdesc.get_loc current_pdesc in
  let current_pname = Procdesc.get_proc_name current_pdesc in
  let report_endpoint_elem current_elem endpoint_pname endpoint_loc elem =
    if LockOrder.may_deadlock current_elem elem then
      let () = debug "Possible deadlock:@.%a@.%a@." LockOrder.pp current_elem LockOrder.pp elem in
      match (current_elem.LockOrder.eventually, elem.LockOrder.eventually) with
      | {LockEvent.event= LockAcquire _}, {LockEvent.event= LockAcquire _} ->
          let error_message =
            Format.asprintf
              "Potential deadlock.@.Trace 1 (starts at %a), %a.@.Trace 2 (starts at %a), %a."
              Typ.Procname.pp current_pname LockOrder.pp current_elem Typ.Procname.pp
              endpoint_pname LockOrder.pp elem
          in
          let exn =
            Exceptions.Checkers (IssueType.starvation, Localise.verbatim_desc error_message)
          in
          let first_trace = List.rev (make_loc_trace current_pname 1 current_loc current_elem) in
          let second_trace = make_loc_trace endpoint_pname 2 endpoint_loc elem in
          let ltr = List.rev_append first_trace second_trace in
          log_issue current_pname current_loc ltr exn
      | _, _ ->
          ()
  in
  let report_on_current_elem elem =
    match elem with
    | {LockOrder.first= None} | {LockOrder.eventually= {LockEvent.event= LockEvent.MayBlock _}} ->
        ()
    | {LockOrder.eventually= {LockEvent.event= LockEvent.LockAcquire endpoint_lock}} ->
      match LockIdentity.owner_class endpoint_lock with
      | None ->
          ()
      | Some endpoint_class ->
          (* get the class of the root variable of the lock in the endpoint event
                   and retrieve all the summaries of the methods of that class *)
          let endpoint_summaries =
            get_summaries_of_methods_in_class get_proc_desc tenv current_pdesc endpoint_class
          in
          (* for each summary related to the endpoint, analyse and report on its pairs *)
          List.iter endpoint_summaries ~f:(fun (endpoint_pdesc, (summary, endpoint_main)) ->
              if not (current_main && endpoint_main) then
              let endpoint_loc = Procdesc.get_loc endpoint_pdesc in
              let endpoint_pname = Procdesc.get_proc_name endpoint_pdesc in
              LockOrderDomain.iter (report_endpoint_elem elem endpoint_pname endpoint_loc) summary
          )
  in
  LockOrderDomain.iter report_on_current_elem summary


let report_blocks_on_main_thread get_proc_desc tenv current_pdesc summary =
  let open StarvationDomain in
  let current_loc = Procdesc.get_loc current_pdesc in
  let current_pname = Procdesc.get_proc_name current_pdesc in
  let report_remote_block current_elem current_lock endpoint_pname endpoint_loc endpoint_elem =
    match endpoint_elem with
    | { LockOrder.first= Some {LockEvent.event= LockEvent.LockAcquire lock}
      ; eventually= {LockEvent.event= LockEvent.MayBlock block_descr} }
      when LockIdentity.equal current_lock lock ->
        let error_message =
          Format.asprintf "UI thread %a, which may be held by another thread which %s"
            LockIdentity.pp lock block_descr
        in
        let exn =
          Exceptions.Checkers (IssueType.starvation, Localise.verbatim_desc error_message)
        in
        let first_trace = List.rev (make_loc_trace current_pname 1 current_loc current_elem) in
        let second_trace = make_loc_trace endpoint_pname 2 endpoint_loc endpoint_elem in
        let ltr = List.rev_append first_trace second_trace in
        log_issue current_pname current_loc ltr exn
    | _ ->
        ()
  in
  let report_on_current_elem ({LockOrder.eventually} as elem) =
    match eventually with
    | {LockEvent.event= LockEvent.MayBlock _} ->
        let error_message =
          Format.asprintf "UI-thread method may block; %a" LockEvent.pp_event
            eventually.LockEvent.event
        in
        let exn =
          Exceptions.Checkers (IssueType.starvation, Localise.verbatim_desc error_message)
        in
        let ltr = make_trace_with_header elem current_loc current_pname in
        log_issue current_pname current_loc ltr exn
    | {LockEvent.event= LockEvent.LockAcquire endpoint_lock} ->
      match LockIdentity.owner_class endpoint_lock with
      | None ->
          ()
      | Some endpoint_class ->
          (* get the class of the root variable of the lock in the endpoint event
                     and retrieve all the summaries of the methods of that class *)
          let endpoint_summaries =
            get_summaries_of_methods_in_class get_proc_desc tenv current_pdesc endpoint_class
          in
          (* for each summary related to the endpoint, analyse and report on its pairs *)
          List.iter endpoint_summaries ~f:(fun (endpoint_pdesc, (summary, main)) ->
              (* skip methods known to run on ui thread, as they cannot run in parallel to us *)
              if main then ()
              else
                let endpoint_loc = Procdesc.get_loc endpoint_pdesc in
                let endpoint_pname = Procdesc.get_proc_name endpoint_pdesc in
                LockOrderDomain.iter
                  (report_remote_block elem endpoint_lock endpoint_pname endpoint_loc)
                  summary )
  in
  LockOrderDomain.iter report_on_current_elem summary


let reporting {Callbacks.procedures; get_proc_desc; exe_env} =
  let report_procedure (tenv, proc_desc) =
    Summary.read_summary proc_desc (Procdesc.get_proc_name proc_desc)
    |> Option.iter ~f:(fun ((s, main) as summary) ->
           report_deadlocks get_proc_desc tenv proc_desc summary ;
           if main then report_blocks_on_main_thread get_proc_desc tenv proc_desc s )
  in
  List.iter procedures ~f:report_procedure ;
  let sourcefile = exe_env.Exe_env.source_file in
  IssueLog.store Config.starvation_issues_dir_name sourcefile
