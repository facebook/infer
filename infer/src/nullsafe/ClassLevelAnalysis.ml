(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let log_issue ?proc_name ~issue_log ~loc ~trace issue_type error_message =
  let proc_name = Option.value proc_name ~default:Procname.Linters_dummy_method in
  Reporting.log_issue_external proc_name Exceptions.Info ~issue_log ~loc ~ltr:trace issue_type
    error_message


(* Meta issues are those related to null-safety of the class in general, not concrete nullability violations *)
let report_meta_issues source_file class_name class_info issue_log =
  (* For purposes of aggregation, we consider all nested anonymous summaries as belonging to this class *)
  let summaries = AggregatedSummaries.ClassInfo.get_all_summaries class_info in
  let loc = Location.{file= source_file; line= 1; col= 0} in
  let total_count =
    List.map summaries ~f:(fun Summary.{payloads= {nullsafe}} ->
        Option.value_map nullsafe
          ~f:(fun NullsafeSummary.{type_violation_count} -> type_violation_count)
          ~default:0 )
    |> List.fold ~init:0 ~f:( + )
  in
  let description =
    Format.asprintf "Class %a: Total type violations: %d" JavaClassName.pp class_name total_count
  in
  let trace = [Errlog.make_trace_element 0 loc description []] in
  let issue_type =
    if total_count > 0 then IssueType.eradicate_meta_class_needs_fixing
    else IssueType.eradicate_meta_class_is_clean
  in
  log_issue ~issue_log ~loc ~trace issue_type description


(* Optimization - if issues are disabled, don't bother analyzing them *)
let should_analyze_meta_issues () =
  (not Config.filtering) || IssueType.eradicate_meta_class_is_clean.IssueType.enabled
  || IssueType.eradicate_meta_class_needs_fixing.IssueType.enabled


let analyze_class source_file class_name class_info issue_log =
  if should_analyze_meta_issues () then
    report_meta_issues source_file class_name class_info issue_log
  else issue_log
