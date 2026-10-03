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


let do_report {InterproceduralAnalysis.tenv; proc_desc; err_log} ~is_suppressed ~latent
    ?location_override ?extra_trace ?reachable_from (diagnostic : Diagnostic.t) =
  let open Diagnostic in
  if is_suppressed && not Config.pulse_report_issues_for_tests then ()
  else
    (* Report suppressed issues with a message to distinguish them from non-suppressed issues.
       Useful for infer's tests. *)
    let loc_instantiated = Diagnostic.get_location_instantiated diagnostic in
    let suppressed_extra_trace =
      if is_suppressed && Config.pulse_report_issues_for_tests then
        let depth = 0 in
        let tags = [] in
        let location = Diagnostic.get_location diagnostic in
        [Errlog.make_trace_element depth location "*** SUPPRESSED ***" tags]
      else []
    in
    let extras =
      let may_depend_on_an_unknown_value =
        match diagnostic with
        | AccessToInvalidAddress {may_depend_on_an_unknown_value= true} ->
            let elt : Jsonbug_t.may_depend_on_an_unknown_value = {value= true} in
            Some elt
        | _ ->
            None
      in
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
      ; may_depend_on_an_unknown_value
      ; reachable_from
      ; taint_extra
      ; transitive_callees
      ; transitive_missed_captures }
    in
    (* Remap to a different issue type for nullptr derefs in @Nullsafe Java classes,
       and for Swift unwraps of a [.none] Optional (-> SWIFT_NPE). *)
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
      else if IssueType.equal original_issue_type (IssueType.optional_empty_access ~latent) then
        match Procdesc.get_proc_name proc_desc with
        | Procname.Swift _ ->
            IssueType.swift_npe ~latent
        | _ ->
            original_issue_type
      else original_issue_type
    in
    let issue_type = get_issue_type tenv ~latent diagnostic proc_desc in
    let message, suggestion = get_message_and_suggestion diagnostic in
    let message =
      Option.fold reachable_from ~init:message ~f:(fun message reachable_from ->
          F.sprintf "(reachable from %s) %s" reachable_from message )
    in
    let autofix = PulseAutofix.get_autofix proc_desc diagnostic in
    let loc =
      match location_override with
      | Some location ->
          location
      | None ->
          Diagnostic.get_location diagnostic
    in
    let ltr =
      Option.fold extra_trace
        ~init:(suppressed_extra_trace @ get_trace diagnostic)
        ~f:(fun ltr extra_trace ->
          Trace.add_to_errlog ~nesting:0
            ~pp_immediate:(fun fmt -> F.fprintf fmt "original issue trace starts here")
            extra_trace ltr )
    in
    L.d_printfln ~color:Red "Reporting issue: %a: %s" IssueType.pp issue_type message ;
    Reporting.log_issue proc_desc err_log ~loc ?loc_instantiated ~ltr ~extras ~autofix ?suggestion
      Pulse issue_type message


let report analysis_data ~is_suppressed ~latent diagnostic =
  do_report analysis_data ~is_suppressed ~latent diagnostic


let report_if_entry_point ({InterproceduralAnalysis.proc_desc} as analysis_data) trace_to_error
    diagnostic =
  let proc_name_s = Procdesc.get_proc_name proc_desc |> Procname.to_string in
  if
    List.exists Config.pulse_report_issues_reachable_from ~f:(fun regex ->
        Str.string_match regex proc_name_s 0 )
  then
    let location_override = Trace.get_outer_location trace_to_error in
    do_report analysis_data ~is_suppressed:false ~latent:false ~location_override
      ~extra_trace:trace_to_error ~reachable_from:proc_name_s diagnostic


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
    | ConstantDereference _ | ComparedToNullInThisProcedure _ ->
        not
          (Trace.exists access_trace ~f:(function
            | Invalidated (trace_invalidation, _, _)
              when Invalidation.is_same_type trace_invalidation invalidation ->
                true
            | _ ->
                false ))
    | CFree
    | CppDelete
    | CppDeleteArray
    | EndIterator
    | FClose
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
  | AssertionError _
  | ConfigUsage _
  | ConstRefableParameter _
  | DynamicTypeMismatch _
  | ErlangError _
  | InfiniteLoopError _
  | HackCannotInstantiateAbstractClass _
  | MissingNullabilityAnnotation _
  | MutualRecursionCycle _
  | ReadonlySharedPtrParameter _
  | ReadUninitialized _
  | ResourceLeak _
  | RetainCycle _
  | StackVariableAddressEscape _
  | TaintFlow _
  | TransitiveAccess _
  | UninitMethod _
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


let is_suppressed_diagnostic tenv proc_desc (diagnostic : Diagnostic.t) =
  is_suppressed tenv proc_desc
    ~is_nullptr_dereference:(match diagnostic with AccessToInvalidAddress _ -> true | _ -> false)
    ~is_constant_deref_without_invalidation:
      (is_constant_deref_without_invalidation_diagnostic diagnostic)
    ~is_optional_empty:(is_optional_empty diagnostic)


(** latent issues found during the analysis of the current procedure; see
    [promote_and_report_latent_issues] *)
let delayed_latent_issues =
  AnalysisGlobalState.make_dls ~init:(fun () : (LatentIssue.t * bool) list -> [])


let summary_of_error_post proc_desc location mk_error astate =
  match AbductiveDomain.Summary.of_post (Procdesc.get_attributes proc_desc) location astate with
  | Sat (Ok summary)
  | Sat
      ( Error (`MemoryLeak (summary, _, _, _, _))
      | Error (`JavaResourceLeak (summary, _, _, _, _))
      | Error (`UnawaitedAwaitable (summary, _, _, _))
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
  | Unsat _ as unsat ->
      unsat


let summary_error_of_error proc_desc location (error : AccessResult.error) : _ SatUnsat.t =
  match error with
  | WithSummary (error, summary) ->
      Sat (error, summary)
  | PotentialInvalidAccess {astate}
  | PotentialInvalidSpecializedCall {astate}
  | ReportableError {astate} ->
      summary_of_error_post proc_desc location (fun summary -> (error, summary)) astate


(* the access error and summary must come from [summary_error_of_error] *)
let report_summary_error ({InterproceduralAnalysis.tenv; proc_desc} as analysis_data) path
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
             ; may_depend_on_an_unknown_value=
                 AbductiveDomain.Summary.contains_unknown_values summary
             ; must_be_valid_reason= snd must_be_valid } ) ;
      Some
        (Stopped (LatentInvalidAccess {astate= summary; address; must_be_valid; calling_context= []})
        )
  | PotentialInvalidSpecializedCall {specialized_type; trace} ->
      Some (Stopped (LatentSpecializedTypeIssue {astate= summary; specialized_type; trace}))
  | ReportableError {diagnostic} -> (
      let is_suppressed = is_suppressed_diagnostic tenv proc_desc diagnostic in
      match LatentIssue.should_report summary diagnostic with
      | `ReportNow ->
          if is_suppressed then L.d_printfln "ReportNow suppressed error" ;
          report analysis_data ~latent:false ~is_suppressed diagnostic ;
          if Diagnostic.aborts_execution path diagnostic then
            let trace_to_issue =
              Trace.Immediate {location= Procdesc.get_loc proc_desc; history= ValueHistory.epoch}
            in
            Some (Stopped (AbortProgram {astate= summary; diagnostic; trace_to_issue}))
          else None
      | `DelayReport latent_issue ->
          if is_suppressed then L.d_printfln "DelayReport suppressed error" ;
          if Config.pulse_report_latent_issues then
            Utils.with_dls delayed_latent_issues ~f:(fun latent_issues ->
                (latent_issue, is_suppressed) :: latent_issues ) ;
          Some (Stopped (LatentAbortProgram {astate= summary; latent_issue})) )
  | WithSummary _ ->
      (* impossible thanks to prior application of [summary_error_of_error] *)
      assert false


(* not the message, which can differ between states, for instance when the invalid value is also the
   value of a variable in one of them. The locations of the calls leading to the issue are part of
   the key so that states that fail at different places inside the same call are not grouped. *)
module IssueKey = struct
  type t = string * Location.t list [@@deriving compare]

  let of_latent_issue latent_issue =
    let diagnostic = LatentIssue.to_diagnostic latent_issue in
    let rec trace_locations (trace : Trace.t) =
      match trace with
      | Immediate {location} ->
          [location]
      | ViaCall {location; in_call} ->
          location :: trace_locations in_call
    in
    let locations =
      match (latent_issue : LatentIssue.t) with
      | AccessToInvalidAddress {access_trace} ->
          trace_locations access_trace
      | ErlangError _ ->
          [Diagnostic.get_location diagnostic]
    in
    ((Diagnostic.get_issue_type ~latent:false diagnostic).IssueType.unique_id, locations)
end

module IssueKeyMap = Stdlib.Map.Make (IssueKey)

let promote_and_report_latent_issues ({InterproceduralAnalysis.tenv; proc_desc} as analysis_data)
    (pre_post_list : AbductiveDomain.Summary.t ExecutionDomain.base_t list) =
  let latent_states_by_issue =
    (* tests in the current procedure make Erlang issues latent too (see
       [PulseFormula.is_manifest]), keep that stricter behaviour *)
    if Language.curr_language_is Erlang then IssueKeyMap.empty
    else
      List.fold pre_post_list ~init:IssueKeyMap.empty ~f:(fun latent_states exec_state ->
          match (exec_state : _ ExecutionDomain.base_t) with
          | Stopped (LatentAbortProgram {astate; latent_issue}) ->
              IssueKeyMap.update
                (IssueKey.of_latent_issue latent_issue)
                (fun states -> Some ((astate, latent_issue) :: Option.value states ~default:[]))
                latent_states
          | _ ->
              latent_states )
  in
  (* in a state where a global variable is equal to the invalid value, for instance 0, the issue can
     be blamed on that variable *)
  let blames_global_variable (diagnostic : Diagnostic.t) =
    match diagnostic with
    | AccessToInvalidAddress {invalid_address= SourceExpr ((PVar pvar, _), _)} ->
        Pvar.is_global pvar
    | _ ->
        false
  in
  let manifest_issues =
    IssueKeyMap.filter_map
      (fun _ states ->
        (* suppressed issues stay latent so that callers, where they may not be suppressed, still
           get to report them *)
        let diagnostics =
          List.rev_map states ~f:(fun (_, latent_issue) -> LatentIssue.to_diagnostic latent_issue)
          |> List.filter ~f:(fun diagnostic ->
              not (is_suppressed_diagnostic tenv proc_desc diagnostic) )
        in
        if
          List.length states > 1
          && (not (List.is_empty diagnostics))
          && PulseArithmetic.is_manifest_disjunction (List.map states ~f:fst)
        then
          Option.first_some
            (List.find diagnostics ~f:(fun diagnostic -> not (blames_global_variable diagnostic)))
            (List.hd diagnostics)
        else None )
      latent_states_by_issue
  in
  let trace_to_issue =
    Trace.Immediate {location= Procdesc.get_loc proc_desc; history= ValueHistory.epoch}
  in
  IssueKeyMap.iter
    (fun _ diagnostic ->
      L.d_printfln ~color:Red "latent issue is manifest in the disjunction of its states" ;
      report analysis_data ~latent:false ~is_suppressed:false diagnostic ;
      report_if_entry_point analysis_data trace_to_issue diagnostic )
    manifest_issues ;
  List.rev (DLS.get delayed_latent_issues)
  |> List.iter ~f:(fun (latent_issue, is_suppressed) ->
      if not (IssueKeyMap.mem (IssueKey.of_latent_issue latent_issue) manifest_issues) then
        report_latent_issue analysis_data ~is_suppressed latent_issue ) ;
  DLS.set delayed_latent_issues [] ;
  List.map pre_post_list ~f:(fun (exec_state : _ ExecutionDomain.base_t) ->
      match exec_state with
      | Stopped (LatentAbortProgram {astate; latent_issue})
        when IssueKeyMap.mem (IssueKey.of_latent_issue latent_issue) manifest_issues ->
          let diagnostic = LatentIssue.to_diagnostic latent_issue in
          ExecutionDomain.Stopped (AbortProgram {astate; diagnostic; trace_to_issue})
      | _ ->
          exec_state )


let report_error ({InterproceduralAnalysis.proc_desc} as analysis_data) path location access_error =
  let open SatUnsat.Import in
  summary_error_of_error proc_desc location access_error >>| report_summary_error analysis_data path


let report_errors analysis_data path location errors =
  let open SatUnsat.Import in
  List.rev errors
  |> List.fold ~init:(Sat None) ~f:(fun sat_result error ->
      match sat_result with
      | Unsat _ | Sat (Some _) ->
          sat_result
      | Sat None ->
          report_error analysis_data path location error )


let report_exec_results analysis_data path location results =
  let results = PulseTaintOperations.dedup_reports results in
  List.filter_map results ~f:(fun exec_result ->
      match PulseResult.to_result exec_result with
      | Ok post ->
          Some post
      | Error errors -> (
        match report_errors analysis_data path location errors with
        | Unsat unsat_info ->
            L.d_printfln "UNSAT discovered during error reporting" ;
            SatUnsat.log_unsat unsat_info ;
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


let report_results analysis_data path location results =
  let open PulseResult.Let_syntax in
  List.map results ~f:(fun result ->
      let+ astate = result in
      ExecutionDomain.ContinueProgram astate )
  |> report_exec_results analysis_data path location


let report_result analysis_data path location result =
  report_results analysis_data path location [result]
