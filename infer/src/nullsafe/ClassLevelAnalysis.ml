(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let log_issue ?proc_name ~issue_log ~loc ~trace ~severity issue_type error_message =
  let proc_name = Option.value proc_name ~default:Procname.Linters_dummy_method in
  Reporting.log_issue_external proc_name severity ~issue_log ~loc ~ltr:trace issue_type
    error_message


(* If the issue is related to violation of nullability type system rules *)
let is_typing_rules_violation = function
  | TypeErr.Condition_redundant _ | TypeErr.Over_annotation _ ->
      (* Those are not nullability type system violations per se *)
      false
  | TypeErr.Inconsistent_subclass _
  | TypeErr.Nullable_dereference _
  | TypeErr.Field_not_initialized _
  | TypeErr.Bad_assignment _ ->
      true


(* Yes, if the issue is a) "type violation" issue b) reportable to the user in a given mode *)
let is_reportable_typing_rules_violation ~nullsafe_mode issue =
  is_typing_rules_violation issue && TypeErr.is_reportable ~nullsafe_mode issue


let get_reportable_typing_rules_violations ~nullsafe_mode issues =
  List.filter issues ~f:(is_reportable_typing_rules_violation ~nullsafe_mode)


(* analyze all isses for the current class and classify them into one meta-issue.
   return meta issue, description, and severity
 *)
let make_meta_issue all_issues current_mode class_name =
  (* If current mode is Default, we want to see what would it take to make it @Nullsafe.
  *)
  let target_mode =
    if NullsafeMode.equal current_mode Default then NullsafeMode.Local NullsafeMode.Trust.All
    else current_mode
  in
  let reportable_issue_count =
    (* NOTE: This is a tricky place. There are issues that are not surfaced in Default mode, but
       would be surfaced if mode becomes @Nullsafe.
       We want to take those issues into account, hence we evaluate against target_mode, and not current mode!
       With that logic, we will classify class as [eradicate_meta_class_can_be_nullsafe] only
       if it indeed can be made @Nullsafe without any (including currently hidden!) issues occurred.
    *)
    get_reportable_typing_rules_violations ~nullsafe_mode:target_mode all_issues |> List.length
  in
  if Int.equal reportable_issue_count 0 then
    (* Good news. No issues in target mode! *)
    if NullsafeMode.equal current_mode Default then
      (* This class is not @Nullsafe yet, but can become such! *)
      ( IssueType.eradicate_meta_class_can_be_nullsafe
      , Format.asprintf
          "Congrats! Class %a is free of nullability issues. Mark it \
           `@Nullsafe(Nullsafe.Mode.Local)` to prevent regressions."
          JavaClassName.pp class_name
      , Exceptions.Advice )
    else
      (* This class is already nullsafe *)
      ( IssueType.eradicate_meta_class_is_nullsafe
      , Format.asprintf "Class %a is free of nullability issues." JavaClassName.pp class_name
      , Exceptions.Info )
  else
    (* At least one nullability issue. *)
    let description =
      if NullsafeMode.equal current_mode Default then
        Format.asprintf "Class %a needs %d issues to be fixed in order to be marked @Nullsafe."
          JavaClassName.pp class_name reportable_issue_count
      else
        Format.asprintf
          "@Nullsafe classes should have exactly zero nullability issues. Class %a has %d."
          JavaClassName.pp class_name reportable_issue_count
    in
    (IssueType.eradicate_meta_class_needs_improvement, description, Exceptions.Info)


(* Meta issues are those related to null-safety of the class in general, not concrete nullability violations *)
let report_meta_issues tenv source_file class_name class_info issue_log =
  (* For purposes of aggregation, we consider all nested anonymous summaries as belonging to this class *)
  let current_mode = NullsafeMode.of_class tenv (Typ.JavaClass class_name) in
  let summaries = AggregatedSummaries.ClassInfo.get_all_summaries class_info in
  let loc = Location.{file= source_file; line= 1; col= 0} in
  let all_issues =
    List.map summaries ~f:(fun Summary.{payloads= {nullsafe}} ->
        Option.value_map nullsafe ~f:(fun NullsafeSummary.{issues} -> issues) ~default:[] )
    |> List.fold ~init:[] ~f:( @ )
  in
  let issue_type, description, severity = make_meta_issue all_issues current_mode class_name in
  let trace = [Errlog.make_trace_element 0 loc description []] in
  log_issue ~issue_log ~loc ~trace ~severity issue_type description


(* Optimization - if issues are disabled, don't bother analyzing them *)
let should_analyze_meta_issues () =
  (not Config.filtering) || IssueType.eradicate_meta_class_can_be_nullsafe.enabled
  || IssueType.eradicate_meta_class_needs_improvement.enabled
  || IssueType.eradicate_meta_class_is_nullsafe.enabled


let analyze_class tenv source_file class_name class_info issue_log =
  if should_analyze_meta_issues () then
    report_meta_issues tenv source_file class_name class_info issue_log
  else issue_log
