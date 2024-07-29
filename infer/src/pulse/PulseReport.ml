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

(* Is nullptr dereference issue in Java class annotated with [@Nullsafe] *)
let is_nullptr_dereference_in_nullsafe_class tenv ~is_nullptr_dereference jn =
  is_nullptr_dereference
  && match NullsafeMode.of_java_procname tenv jn with Default -> false | Local | Strict -> true


let report {InterproceduralAnalysis.tenv; proc_desc; err_log} ~is_suppressed ~latent
    (diagnostic : Diagnostic.t) =
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
      let transitive_callees, transitive_missed_captures =
        let to_jsonbug_missed_capture class_name =
          {Jsonbug_t.class_name= Typ.Name.name class_name}
        in
        match diagnostic with
        | TransitiveAccess {transitive_callees; transitive_missed_captures} ->
            ( TransitiveInfo.Callees.to_jsonbug_transitive_callees transitive_callees
            , Typ.Name.Set.elements transitive_missed_captures
              |> List.map ~f:to_jsonbug_missed_capture )
        | _ ->
            ([], [])
      in
      let copy_type = get_copy_type diagnostic |> Option.map ~f:Typ.to_string in
      let taint_source, taint_sink =
        let proc_name_of_taint taint_item =
          Format.asprintf "%a" TaintItem.pp_value_plain (TaintItem.value_of_taint taint_item)
        in
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
      let tainted_expression =
        match diagnostic with
        | TaintFlow {expr} ->
            Some (F.asprintf "%a" DecompilerExpr.pp expr)
        | _ ->
            None
      in
      let taint_policy_privacy_effect =
        match diagnostic with
        | TaintFlow {flow_kind= TaintedFlow; policy_privacy_effect; _} ->
            policy_privacy_effect
        | _ ->
            None
      in
      let taint_report_as_issue_type, taint_report_as_category =
        match diagnostic with
        | TaintFlow {report_as_issue_type; report_as_category} ->
            (report_as_issue_type, report_as_category)
        | _ ->
            (None, None)
      in
      let taint_extra : Jsonbug_t.taint_extra option =
        match
          ( taint_source
          , taint_sink
          , taint_policy_privacy_effect
          , tainted_expression
          , taint_report_as_issue_type
          , taint_report_as_category )
        with
        | None, None, None, None, None, None ->
            None
        | _, _, _, _, _, _ ->
            Some
              { taint_source
              ; taint_sink
              ; taint_policy_privacy_effect
              ; tainted_expression
              ; report_as_issue_type= taint_report_as_issue_type
              ; report_as_category= taint_report_as_category }
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
      { Jsonbug_t.cost_polynomial= None
      ; cost_degree= None
      ; copy_type
      ; config_usage_extra
      ; taint_extra
      ; transitive_callees
      ; transitive_missed_captures }
    in
    (* [Diagnostic.get_issue_type] wrapper to report different type of issue for
       nullptr dereferences in Java classes annotated with @Nullsafe if requested *)
    let get_issue_type tenv ~latent diagnostic proc_desc =
      let original_issue_type = Diagnostic.get_issue_type diagnostic ~latent in
      if IssueType.equal original_issue_type (IssueType.nullptr_dereference ~latent) then
        match Procdesc.get_proc_name proc_desc with
        | Procname.Java jn
          when is_nullptr_dereference_in_nullsafe_class tenv ~is_nullptr_dereference:true jn
               && Config.pulse_nullsafe_report_npe_as_separate_issue_type ->
            IssueType.nullptr_dereference_in_nullsafe_class ~latent
        | _ ->
            original_issue_type
      else original_issue_type
    in
    let issue_type = get_issue_type tenv ~latent diagnostic proc_desc in
    let message, suggestion = get_message_and_suggestion diagnostic in
    L.d_printfln ~color:Red "Reporting issue: %a: %s" IssueType.pp issue_type message ;
    Reporting.log_issue proc_desc err_log ~loc:(get_location diagnostic) ?loc_instantiated
      ~ltr:(extra_trace @ get_trace diagnostic)
      ~extras ?suggestion Pulse issue_type message


let report_latent_issue analysis_data latent_issue ~is_suppressed =
  LatentIssue.to_diagnostic latent_issue |> report analysis_data ~latent:true ~is_suppressed


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
    | StdVector _
    | CppMap _ ->
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
  | DynamicTypeMismatch _
  | ErlangError _
  | TransitiveAccess _
  | JavaResourceLeak _
  | HackCannotInstantiateAbstractClass _
  | HackUnawaitedAwaitable _
  | HackUnfinishedBuilder _
  | MemoryLeak _
  | MutualRecursionCycle _
  | ReadonlySharedPtrParameter _
  | ReadUninitialized _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | UnnecessaryCopy _ ->
      false
  | AccessToInvalidAddress {invalidation; access_trace} ->
      is_constant_deref_without_invalidation invalidation access_trace


let is_optional_empty diagnostic =
  match (diagnostic : Diagnostic.t) with
  | AccessToInvalidAddress {invalidation= OptionalEmpty} ->
      true
  | _ ->
      false


(* Skip reporting nullptr dereferences in Java classes annotated with [@Nullsafe] if requested *)
let should_skip_reporting_nullptr_dereference_in_nullsafe_class tenv ~is_nullptr_dereference jn =
  (not Config.pulse_nullsafe_report_npe)
  && is_nullptr_dereference_in_nullsafe_class tenv ~is_nullptr_dereference jn


let is_suppressed tenv proc_desc ~is_nullptr_dereference ~is_constant_deref_without_invalidation
    ~is_optional_empty =
  if is_constant_deref_without_invalidation then (
    L.d_printfln ~color:Red
      "Dropping error: constant dereference with no invalidation in the access trace" ;
    true )
  else
    match Procdesc.get_proc_name proc_desc with
    | Procname.Java jn when is_nullptr_dereference ->
        should_skip_reporting_nullptr_dereference_in_nullsafe_class tenv ~is_nullptr_dereference jn
    | pname ->
        Procname.is_cpp_lambda pname && is_optional_empty


let summary_of_error_post proc_desc location mk_error astate =
  match
    AbductiveDomain.Summary.of_post
      (Procdesc.get_proc_name proc_desc)
      (Procdesc.get_attributes proc_desc)
      location astate
  with
  | Sat (Ok summary)
  | Sat
      ( Error (`MemoryLeak (summary, _, _, _, _))
      | Error (`JavaResourceLeak (summary, _, _, _, _))
      | Error (`HackUnawaitedAwaitable (summary, _, _, _))
      | Error (`HackUnfinishedBuilder (summary, _, _, _, _))
      | Error (`CSharpResourceLeak (summary, _, _, _, _)) ) ->
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


let summary_error_of_error proc_desc location (error : AccessResult.error) : _ SatUnsat.t =
  match error with
  | WithSummary (error, summary) ->
      Sat (error, summary)
  | PotentialInvalidAccess {astate}
  | PotentialInvalidSpecializedCall {astate}
  | ReportableError {astate} ->
      summary_of_error_post proc_desc location (fun summary -> (error, summary)) astate


(* the access error and summary must come from [summary_error_of_error] *)
let report_summary_error ({InterproceduralAnalysis.tenv; proc_desc} as analysis_data)
    ((access_error : AccessResult.error), summary) : _ ExecutionDomain.base_t option =
  match access_error with
  | PotentialInvalidAccess {address; must_be_valid} ->
      let invalidation = Invalidation.ConstantDereference IntLit.zero in
      let access_trace = fst must_be_valid in
      let is_constant_deref_without_invalidation =
        is_constant_deref_without_invalidation invalidation access_trace
      in
      let is_suppressed =
        is_suppressed tenv proc_desc ~is_nullptr_dereference:true
          ~is_constant_deref_without_invalidation ~is_optional_empty:false
      in
      if is_suppressed then L.d_printfln "suppressed error" ;
      if Config.pulse_report_latent_issues then
        report analysis_data ~latent:true ~is_suppressed
          (AccessToInvalidAddress
             { calling_context= []
             ; invalid_address= address
             ; invalidation= ConstantDereference IntLit.zero
             ; invalidation_trace=
                 Immediate {location= Procdesc.get_loc proc_desc; history= ValueHistory.epoch}
             ; access_trace
             ; must_be_valid_reason= snd must_be_valid } ) ;
      Some (LatentInvalidAccess {astate= summary; address; must_be_valid; calling_context= []})
  | PotentialInvalidSpecializedCall {specialized_type; trace} ->
      Some (LatentSpecializedTypeIssue {astate= summary; specialized_type; trace})
  | ReportableError {diagnostic} -> (
      let is_nullptr_dereference =
        match diagnostic with AccessToInvalidAddress _ -> true | _ -> false
      in
      let is_constant_deref_without_invalidation =
        is_constant_deref_without_invalidation_diagnostic diagnostic
      in
      let is_optional_empty = is_optional_empty diagnostic in
      let is_suppressed =
        is_suppressed tenv proc_desc ~is_nullptr_dereference ~is_constant_deref_without_invalidation
          ~is_optional_empty
      in
      match LatentIssue.should_report summary diagnostic with
      | `ReportNow ->
          if is_suppressed then L.d_printfln "ReportNow suppressed error" ;
          report analysis_data ~latent:false ~is_suppressed diagnostic ;
          if Diagnostic.aborts_execution diagnostic then Some (AbortProgram summary) else None
      | `DelayReport latent_issue ->
          if is_suppressed then L.d_printfln "DelayReport suppressed error" ;
          if Config.pulse_report_latent_issues then
            report_latent_issue analysis_data ~is_suppressed latent_issue ;
          Some (LatentAbortProgram {astate= summary; latent_issue}) )
  | WithSummary _ ->
      (* impossible thanks to prior application of [summary_error_of_error] *)
      assert false


let report_error ({InterproceduralAnalysis.proc_desc} as analysis_data) location access_error =
  let open SatUnsat.Import in
  summary_error_of_error proc_desc location access_error >>| report_summary_error analysis_data


let report_errors analysis_data location errors =
  let open SatUnsat.Import in
  List.rev errors
  |> List.fold ~init:(Sat None) ~f:(fun sat_result error ->
         match sat_result with
         | Unsat | Sat (Some _) ->
             sat_result
         | Sat None ->
             report_error analysis_data location error )


let report_exec_results analysis_data location results =
  let results = PulseTaintOperations.dedup_reports results in
  List.filter_map results ~f:(fun exec_result ->
      match PulseResult.to_result exec_result with
      | Ok post ->
          Some post
      | Error errors -> (
        match report_errors analysis_data location errors with
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


let report_results analysis_data location results =
  let open PulseResult.Let_syntax in
  List.map results ~f:(fun result ->
      let+ astate = result in
      ExecutionDomain.ContinueProgram astate )
  |> report_exec_results analysis_data location


let report_result analysis_data location result = report_results analysis_data location [result]
