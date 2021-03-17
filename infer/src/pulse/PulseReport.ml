(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

let report ?(extra_trace = []) proc_desc err_log diagnostic =
  let open Diagnostic in
  Reporting.log_issue proc_desc err_log ~loc:(get_location diagnostic)
    ~ltr:(extra_trace @ get_trace diagnostic)
    Pulse (get_issue_type diagnostic) (get_message diagnostic)


let report_latent_issue proc_desc err_log latent_issue =
  (* HACK: report latent issues with a prominent message to distinguish them from
     non-latent. Useful for infer's own tests. *)
  let diagnostic = LatentIssue.to_diagnostic latent_issue in
  let extra_trace =
    let depth = 0 in
    let tags = [] in
    let location = Diagnostic.get_location diagnostic in
    [Errlog.make_trace_element depth location "*** LATENT ***" tags]
  in
  report ~extra_trace proc_desc err_log diagnostic


let is_nullsafe_error tenv diagnostic jn =
  (not Config.pulse_nullsafe_report_npe)
  && IssueType.equal (Diagnostic.get_issue_type diagnostic) IssueType.nullptr_dereference
  && match NullsafeMode.of_java_procname tenv jn with Default -> false | Local _ | Strict -> true


let is_suppressed tenv proc_desc diagnostic astate =
  match Procdesc.get_proc_name proc_desc with
  | Procname.Java jn ->
      is_nullsafe_error tenv diagnostic jn
      || not (AbductiveDomain.skipped_calls_match_pattern astate)
  | _ ->
      false


let summary_of_error_post tenv proc_desc mk_error astate =
  match AbductiveDomain.summary_of_post tenv proc_desc astate with
  | Sat astate ->
      Sat (mk_error astate)
  | Unsat ->
      Unsat


let summary_error_of_error tenv proc_desc (error : AbductiveDomain.t AccessResult.error) :
    AbductiveDomain.summary AccessResult.error SatUnsat.t =
  match error with
  | ReportableError {astate; diagnostic} ->
      summary_of_error_post tenv proc_desc
        (fun astate -> AccessResult.ReportableError {astate; diagnostic})
        astate
  | ISLError astate ->
      summary_of_error_post tenv proc_desc (fun astate -> AccessResult.ISLError astate) astate


let report_summary_error tenv proc_desc err_log
    (access_error : AbductiveDomain.summary AccessResult.error) : _ ExecutionDomain.base_t =
  match access_error with
  | ISLError astate ->
      ISLLatentMemoryError astate
  | ReportableError {astate; diagnostic} -> (
    match LatentIssue.should_report astate diagnostic with
    | `ReportNow ->
        if not (is_suppressed tenv proc_desc diagnostic astate) then
          report proc_desc err_log diagnostic ;
        AbortProgram astate
    | `DelayReport latent_issue ->
        if Config.pulse_report_latent_issues then report_latent_issue proc_desc err_log latent_issue ;
        LatentAbortProgram {astate; latent_issue} )


let report_error tenv proc_desc err_log (access_error : AbductiveDomain.t AccessResult.error) =
  let open SatUnsat.Import in
  summary_error_of_error tenv proc_desc access_error >>| report_summary_error tenv proc_desc err_log


let report_exec_results tenv proc_desc err_log results =
  List.filter_map results ~f:(fun exec_result ->
      match exec_result with
      | Ok post ->
          Some post
      | Error error -> (
        match report_error tenv proc_desc err_log error with
        | Unsat ->
            None
        | Sat exec_state ->
            Some exec_state ) )


let report_results tenv proc_desc err_log results =
  let open IResult.Let_syntax in
  List.map results ~f:(fun result ->
      let+ astate = result in
      ExecutionDomain.ContinueProgram astate )
  |> report_exec_results tenv proc_desc err_log


let report_result tenv proc_desc err_log result = report_results tenv proc_desc err_log [result]
