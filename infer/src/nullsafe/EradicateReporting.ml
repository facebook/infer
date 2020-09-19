(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let get_proc_name proc_attrs =
  match ProcAttributes.get_proc_name proc_attrs with
  | Procname.Java java_pname ->
      java_pname
  | _ ->
      Logging.die InternalError "Unexpected attempt to report a nullsafe error on a non-java method"


let report_error {IntraproceduralAnalysis.proc_desc; tenv; err_log} checker ?(field_name = None)
    nullsafe_issue =
  let proc_attrs = Procdesc.get_attributes proc_desc in
  let issue_type = NullsafeIssue.get_issue_type nullsafe_issue in
  let description = NullsafeIssue.get_description nullsafe_issue in
  let severity = NullsafeIssue.get_severity nullsafe_issue in
  let loc = NullsafeIssue.get_loc nullsafe_issue in
  let proc_name = get_proc_name proc_attrs in
  let nullsafe_extra = Some (NullsafeIssue.get_nullsafe_extra nullsafe_issue proc_name) in
  let extras = Jsonbug_t.{nullsafe_extra; cost_degree= None; cost_polynomial= None} in
  let suppressed = Reporting.is_suppressed tenv proc_attrs issue_type ~field_name in
  if suppressed then Logging.debug Analysis Medium "Reporting is suppressed!@\n"
  else
    let localized_description = Localise.verbatim_desc description in
    let issue_to_report =
      {IssueToReport.issue_type; description= localized_description; ocaml_pos= None}
    in
    let trace = [Errlog.make_trace_element 0 loc description []] in
    let node = AnalysisState.get_node_exn () in
    let session = AnalysisState.get_session () in
    Reporting.log_issue_from_summary ~severity_override:severity proc_desc err_log
      ~node:(BackendNode {node})
      ~session ~loc ~ltr:trace checker issue_to_report ~extras
