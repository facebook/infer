(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

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


let is_suppressed tenv proc_desc diagnostic =
  match Procdesc.get_proc_name proc_desc with
  | Procname.Java jn
    when (not Config.pulse_nullsafe_report_npe)
         && IssueType.equal (Diagnostic.get_issue_type diagnostic) IssueType.nullptr_dereference
    -> (
    match NullsafeMode.of_java_procname tenv jn with Default -> false | Local _ | Strict -> true )
  | _ ->
      false


let report_error tenv proc_desc err_log access_error =
  match LatentIssue.should_report access_error with
  | `ReportNow (astate_summary, diagnostic) ->
      if not (is_suppressed tenv proc_desc diagnostic) then report proc_desc err_log diagnostic ;
      AbortProgram astate_summary
  | `DelayReport (astate, latent_issue) ->
      if Config.pulse_report_latent_issues then report_latent_issue proc_desc err_log latent_issue ;
      LatentAbortProgram {astate; latent_issue}


let report_exec_results {InterproceduralAnalysis.proc_desc; tenv; err_log} results =
  List.filter_map results ~f:(fun exec_result ->
      match exec_result with
      | Ok post ->
          Some post
      | Error error -> (
        match AccessResult.to_summary tenv proc_desc error with
        | Unsat ->
            None
        | Sat error ->
            Some (report_error tenv proc_desc err_log error) ) )


let report_results analysis_data results =
  List.map results ~f:(fun result ->
      let+ astate = result in
      ContinueProgram astate )
  |> report_exec_results analysis_data


let report_result analysis_data result = report_results analysis_data [result]
