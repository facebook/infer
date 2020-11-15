(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type 'a access_result = ('a, Diagnostic.t * AbductiveDomain.t) result

type 'a path_feasibility = InfeasiblePath | FeasiblePath of 'a

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


let report_error proc_desc err_log access_result =
  Result.map_error access_result ~f:(fun (diagnostic, astate) ->
      let astate_summary = AbductiveDomain.summary_of_post proc_desc astate in
      if PulseArithmetic.is_unsat_cheap (astate_summary :> AbductiveDomain.t) then InfeasiblePath
      else
        match LatentIssue.should_report_diagnostic astate_summary diagnostic with
        | `ReportNow ->
            report proc_desc err_log diagnostic ;
            FeasiblePath (ExecutionDomain.AbortProgram astate_summary)
        | `DelayReport latent_issue ->
            if Config.pulse_report_latent_issues then
              report_latent_issue proc_desc err_log latent_issue ;
            FeasiblePath (ExecutionDomain.LatentAbortProgram {astate= astate_summary; latent_issue}) )
