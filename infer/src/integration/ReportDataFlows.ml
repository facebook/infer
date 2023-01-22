(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type flow_type = FromSource | ToSink

let issue_type_of_flow_type flow_type =
  match flow_type with
  | FromSource ->
      IssueType.sensitive_data_flow
  | ToSink ->
      IssueType.data_flow_to_sink


let jsonbug_filter (jsonbug : Jsonbug_t.jsonbug) ~procname ~flow_type =
  let procname_from_jsonbug =
    let extract_field (taint_extra : Jsonbug_t.taint_extra) =
      match flow_type with
      | FromSource ->
          taint_extra.taint_source
      | ToSink ->
          taint_extra.taint_sink
    in
    let taint_extra = match jsonbug.extras with Some extra -> extra.taint_extra | None -> None in
    Option.bind taint_extra ~f:extract_field
  in
  let procname_matches = Option.exists procname_from_jsonbug ~f:(String.equal procname) in
  let issue_type_matches =
    let expected_issue_type = issue_type_of_flow_type flow_type in
    String.equal jsonbug.bug_type expected_issue_type.unique_id
  in
  procname_matches && issue_type_matches


let report_data_flows_of_procname procname ~flow_type =
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report Config.from_json_report in
  let filtered_report = List.filter report ~f:(jsonbug_filter ~procname ~flow_type) in
  IssuesTest.pp_custom_of_report Format.std_formatter filtered_report Config.issues_tests_fields
