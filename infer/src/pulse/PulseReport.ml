(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface

let report ~is_suppressed ~latent proc_desc err_log diagnostic =
  let open Diagnostic in
  if is_suppressed && not Config.pulse_report_issues_for_tests then ()
  else
    (* Report suppressed issues with a message to distinguish them from non-suppressed issues.
       Useful for infer's tests. *)
    let loc_instantiated = Diagnostic.get_location_instantiated diagnostic in
    let extra_trace =
      if is_suppressed && Config.pulse_report_issues_for_tests then
        let depth = 0 in
        let tags = [] in
        let location = Diagnostic.get_location diagnostic in
        [Errlog.make_trace_element depth location "*** SUPPRESSED ***" tags]
      else []
    in
    let extras =
      let copy_type = get_copy_type diagnostic |> Option.map ~f:Typ.to_string in
      let taint_source, taint_sink =
        let proc_name_of_taint Taint.{proc_name} = Format.asprintf "%a" Procname.pp proc_name in
        match diagnostic with
        | TaintFlow {flow_kind= FlowFromSource; source= source, _} ->
            (Some (proc_name_of_taint source), None)
        | TaintFlow {flow_kind= FlowToSink; sink= sink, _} ->
            (None, Some (proc_name_of_taint sink))
        | TaintFlow {flow_kind= TaintedFlow; source= source, _; sink= sink, _} ->
            (Some (proc_name_of_taint source), Some (proc_name_of_taint sink))
        | _ ->
            (None, None)
      in
      let taint_policy_privacy_effect =
        match diagnostic with
        | TaintFlow {flow_kind= TaintedFlow; policy_privacy_effect; _} ->
            policy_privacy_effect
        | _ ->
            None
      in
      let taint_extra : Jsonbug_t.taint_extra option =
        match (taint_source, taint_sink, taint_policy_privacy_effect) with
        | None, None, None ->
            None
        | _, _, _ ->
            Some {taint_source; taint_sink; taint_policy_privacy_effect}
      in
      let config_usage_extra : Jsonbug_t.config_usage_extra option =
        match diagnostic with
        | ConfigUsage {pname; config; branch_location= {file; line}} ->
            Some
              { config_name= F.asprintf "%a" ConfigName.pp config
              ; function_name= Procname.to_string pname
              ; filename= SourceFile.to_string ~force_relative:true file
              ; line_number= line }
        | _ ->
            None
      in
      Jsonbug_t.
        { cost_polynomial= None
        ; cost_degree= None
        ; nullsafe_extra= None
        ; copy_type
        ; config_usage_extra
        ; taint_extra }
    in
    let issue_type = get_issue_type ~latent diagnostic in
    let message = get_message diagnostic in
    L.d_printfln ~color:Red "Reporting issue: %a: %s" IssueType.pp issue_type message ;
    Reporting.log_issue proc_desc err_log ~loc:(get_location diagnostic) ?loc_instantiated
      ~ltr:(extra_trace @ get_trace diagnostic)
      ~extras Pulse issue_type message


let report_latent_issue proc_desc err_log latent_issue ~is_suppressed =
  LatentIssue.to_diagnostic latent_issue |> report ~latent:true ~is_suppressed proc_desc err_log


(* skip reporting on Java classes annotated with [@Nullsafe] if requested *)
let is_nullsafe_error tenv ~is_nullptr_dereference jn =
  (not Config.pulse_nullsafe_report_npe)
  && is_nullptr_dereference
  && match NullsafeMode.of_java_procname tenv jn with Default -> false | Local _ | Strict -> true


(* skip reporting for constant dereference (eg null dereference) if the source of the null value is
   not on the path of the access, otherwise the report will probably be too confusing: the actual
   source of the null value can be obscured as any value equal to 0 (or the constant) can be
   selected as the candidate for the trace, even if it has nothing to do with the error besides
   being equal to the value being dereferenced *)
let is_constant_deref_without_invalidation (invalidation : Invalidation.t) access_trace =
  let res =
    match invalidation with
    | ConstantDereference _ ->
        not (Trace.exists_main access_trace ~f:(function Invalidated _ -> true | _ -> false))
    | CFree
    | CppDelete
    | CppDeleteArray
    | EndIterator
    | GoneOutOfScope _
    | OptionalEmpty
    | StdVector _ ->
        false
  in
  if res then
    L.d_printfln "no invalidation in acces trace %a"
      (Trace.pp ~pp_immediate:(fun fmt -> F.fprintf fmt "immediate"))
      access_trace ;
  res


let is_constant_deref_without_invalidation_diagnostic (diagnostic : Diagnostic.t) =
  match diagnostic with
  | ConfigUsage _
  | ConstRefableParameter _
  | CSharpResourceLeak _
  | ErlangError _
  | JavaResourceLeak _
  | HackUnawaitedAwaitable _
  | MemoryLeak _
  | ReadonlySharedPtrParameter _
  | ReadUninitializedValue _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | UnnecessaryCopy _ ->
      false
  | AccessToInvalidAddress {invalidation; access_trace} ->
      is_constant_deref_without_invalidation invalidation access_trace


let is_suppressed tenv proc_desc ~is_nullptr_dereference ~is_constant_deref_without_invalidation
    summary =
  if is_constant_deref_without_invalidation then (
    L.d_printfln ~color:Red
      "Dropping error: constant dereference with no invalidation in the access trace" ;
    true )
  else
    match Procdesc.get_proc_name proc_desc with
    | Procname.Java jn when is_nullptr_dereference ->
        let b = is_nullsafe_error tenv ~is_nullptr_dereference jn in
        if b then (
          L.d_printfln ~color:Red "Dropping error: conflicting with nullsafe" ;
          b )
        else
          let b = not (AbductiveDomain.Summary.skipped_calls_match_pattern summary) in
          if b then
            L.d_printfln ~color:Red
              "Dropping error: skipped an unknown function not in the allow list" ;
          b
    | _ ->
        false


let summary_of_error_post tenv proc_desc location mk_error astate =
  match
    AbductiveDomain.Summary.of_post tenv
      (Procdesc.get_proc_name proc_desc)
      (Procdesc.get_attributes proc_desc)
      location astate
  with
  | Sat (Ok summary)
  | Sat
      ( Error (`MemoryLeak (summary, _, _, _, _))
      | Error (`JavaResourceLeak (summary, _, _, _, _))
      | Error (`HackUnawaitedAwaitable (summary, _, _, _))
      | Error (`CSharpResourceLeak (summary, _, _, _, _)) )
  | Sat (Error (`RetainCycle (summary, _, _, _, _, _))) ->
      (* ignore potential memory leaks: error'ing in the middle of a function will typically produce
         spurious leaks *)
      Sat (mk_error summary)
  | Sat (Error (`PotentialInvalidAccessSummary (summary, astate, addr, trace))) ->
      (* ignore the error we wanted to report (with [mk_error]): the abstract state contained a
         potential error already so report [error] instead *)
      Sat
        (AccessResult.of_abductive_summary_error
           (`PotentialInvalidAccessSummary (summary, astate, addr, trace)) )
  | Unsat ->
      Unsat


let summary_error_of_error tenv proc_desc location (error : AccessResult.error) : _ SatUnsat.t =
  match error with
  | WithSummary (error, summary) ->
      Sat (error, summary)
  | PotentialInvalidAccess {astate} | ReportableError {astate} ->
      summary_of_error_post tenv proc_desc location (fun summary -> (error, summary)) astate


(* the access error and summary must come from [summary_error_of_error] *)
let report_summary_error tenv proc_desc err_log ((access_error : AccessResult.error), summary) :
    _ ExecutionDomain.base_t option =
  match access_error with
  | PotentialInvalidAccess {address; must_be_valid} ->
      let invalidation = Invalidation.ConstantDereference IntLit.zero in
      let access_trace = fst must_be_valid in
      let is_constant_deref_without_invalidation =
        is_constant_deref_without_invalidation invalidation access_trace
      in
      let is_suppressed =
        is_suppressed tenv proc_desc ~is_nullptr_dereference:true
          ~is_constant_deref_without_invalidation summary
      in
      if is_suppressed then L.d_printfln "suppressed error" ;
      if Config.pulse_report_latent_issues then
        report ~latent:true ~is_suppressed proc_desc err_log
          (AccessToInvalidAddress
             { calling_context= []
             ; invalid_address= address
             ; invalidation= ConstantDereference IntLit.zero
             ; invalidation_trace=
                 Immediate {location= Procdesc.get_loc proc_desc; history= ValueHistory.epoch}
             ; access_trace
             ; must_be_valid_reason= snd must_be_valid } ) ;
      Some (LatentInvalidAccess {astate= summary; address; must_be_valid; calling_context= []})
  | ReportableError {diagnostic} -> (
      let is_nullptr_dereference =
        match diagnostic with AccessToInvalidAddress _ -> true | _ -> false
      in
      let is_constant_deref_without_invalidation =
        is_constant_deref_without_invalidation_diagnostic diagnostic
      in
      let is_suppressed =
        is_suppressed tenv proc_desc ~is_nullptr_dereference ~is_constant_deref_without_invalidation
          summary
      in
      match LatentIssue.should_report summary diagnostic with
      | `ReportNow ->
          if is_suppressed then L.d_printfln "ReportNow suppressed error" ;
          report ~latent:false ~is_suppressed proc_desc err_log diagnostic ;
          if Diagnostic.aborts_execution diagnostic then Some (AbortProgram summary) else None
      | `DelayReport latent_issue ->
          if is_suppressed then L.d_printfln "DelayReport suppressed error" ;
          if Config.pulse_report_latent_issues then
            report_latent_issue ~is_suppressed proc_desc err_log latent_issue ;
          Some (LatentAbortProgram {astate= summary; latent_issue}) )
  | WithSummary _ ->
      (* impossible thanks to prior application of [summary_error_of_error] *)
      assert false


let report_error tenv proc_desc err_log location access_error =
  let open SatUnsat.Import in
  summary_error_of_error tenv proc_desc location access_error
  >>| report_summary_error tenv proc_desc err_log


let report_errors tenv proc_desc err_log location errors =
  let open SatUnsat.Import in
  List.rev errors
  |> List.fold ~init:(Sat None) ~f:(fun sat_result error ->
         match sat_result with
         | Unsat | Sat (Some _) ->
             sat_result
         | Sat None ->
             report_error tenv proc_desc err_log location error )


let report_exec_results tenv proc_desc err_log location results =
  List.filter_map results ~f:(fun exec_result ->
      match PulseResult.to_result exec_result with
      | Ok post ->
          Some post
      | Error errors -> (
        match report_errors tenv proc_desc err_log location errors with
        | Unsat ->
            L.d_printfln "UNSAT discovered during error reporting" ;
            None
        | Sat None -> (
          match exec_result with
          | Ok _ | FatalError _ ->
              L.die InternalError
                "report_errors returned None but the result was not a recoverable error"
          | Recoverable (exec_state, _) ->
              Some exec_state )
        | Sat (Some exec_state) ->
            Some exec_state ) )


let report_results tenv proc_desc err_log location results =
  let open PulseResult.Let_syntax in
  List.map results ~f:(fun result ->
      let+ astate = result in
      ExecutionDomain.ContinueProgram astate )
  |> report_exec_results tenv proc_desc err_log location


let report_result tenv proc_desc err_log location result =
  report_results tenv proc_desc err_log location [result]
