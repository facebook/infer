(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let log_issue ?proc_name ~issue_log ~loc ~severity ~nullsafe_extra issue_type error_message =
  let extras =
    Jsonbug_t.{nullsafe_extra= Some nullsafe_extra; cost_polynomial= None; cost_degree= None}
  in
  let proc_name = Option.value proc_name ~default:Procname.Linters_dummy_method in
  let trace = [Errlog.make_trace_element 0 loc error_message []] in
  Reporting.log_issue_external proc_name ~severity_override:severity ~issue_log ~loc ~extras
    ~ltr:trace Eradicate issue_type error_message


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


let get_reportable_typing_rules_violations modes_with_issues =
  List.filter modes_with_issues ~f:(fun (nullsafe_mode, issue) ->
      is_reportable_typing_rules_violation ~nullsafe_mode issue )
  |> List.map ~f:(fun (_, a) -> a)


let get_reportable_typing_rules_violations_for_mode ~nullsafe_mode issues =
  List.map issues ~f:(fun issue -> (nullsafe_mode, issue)) |> get_reportable_typing_rules_violations


type meta_issue =
  { issue_type: IssueType.t
  ; description: string
  ; severity: IssueType.severity
  ; meta_issue_info: Jsonbug_t.nullsafe_meta_issue_info }

let mode_to_json mode =
  let open NullsafeMode in
  match mode with
  | Default ->
      `Default
  | Local Trust.All ->
      `LocalTrustAll
  | Local (Trust.Only trust_list) when Trust.is_trust_none trust_list ->
      `LocalTrustNone
  | Local (Trust.Only _) ->
      `LocalTrustSome
  | Strict ->
      `Strict


let is_clean_in_mode nullsafe_mode all_issues =
  get_reportable_typing_rules_violations_for_mode ~nullsafe_mode all_issues |> List.is_empty


(* Return the maximum mode where we still have zero issues, or None if no such mode exists.
*)
let calc_strictest_mode_with_zero_issues all_issues =
  let modes_to_try = NullsafeMode.[Strict; Local Trust.none; Local Trust.All; Default] in
  List.find modes_to_try ~f:(fun mode -> is_clean_in_mode mode all_issues)


(* The maximum strict mode this mode can be promoted to with still having zero issues, if exists *)
let calc_mode_to_promote_to curr_mode all_issues =
  let open IOption.Let_syntax in
  let* strictest_mode = calc_strictest_mode_with_zero_issues all_issues in
  if NullsafeMode.is_stricter_than ~stricter:strictest_mode ~weaker:curr_mode then
    Some strictest_mode
  else None


(* analyze all issues for the current class (including all nested and anonymous classes recursively)
   and classify them into one meta-issue.
 *)
let make_meta_issue modes_and_issues top_level_class_mode top_level_class_name =
  let currently_reportable_issues = get_reportable_typing_rules_violations modes_and_issues in
  List.iter currently_reportable_issues ~f:(fun issue ->
      L.debug Analysis Medium "Issue: %a@\n" TypeErr.pp_err_instance issue ) ;
  let currently_reportable_issue_count = List.length currently_reportable_issues in
  let all_issues = List.map modes_and_issues ~f:(fun (_, a) -> a) in
  let mode_to_promote_to =
    if currently_reportable_issue_count > 0 then
      (* This is not an optimization - consider a class with a nested class that is in the stricter mode than top level mode,
         and has issues in this mode, but not in the top level mode.
         This class is already broken now, so mode to promote to should be None.
         But [calc_mode_to_promote_to] can return some mode for this case, which would be wrong. *)
      None
    else calc_mode_to_promote_to top_level_class_mode all_issues
  in
  let meta_issue_info =
    Jsonbug_t.
      { num_issues= currently_reportable_issue_count
      ; curr_nullsafe_mode= mode_to_json top_level_class_mode
      ; can_be_promoted_to= Option.map mode_to_promote_to ~f:mode_to_json }
  in
  let issue_type, description, severity =
    if NullsafeMode.equal top_level_class_mode Default then
      match mode_to_promote_to with
      | Some _ ->
          let message =
            Format.sprintf
              "Congrats! `%s` is free of nullability issues. Mark it \
               `@Nullsafe(Nullsafe.Mode.LOCAL)` to prevent regressions."
              (JavaClassName.classname top_level_class_name)
          in
          (IssueType.eradicate_meta_class_can_be_nullsafe, message, IssueType.Advice)
      | None ->
          (* This class can not be made @Nullsafe without extra work *)
          let issue_count_to_make_nullsafe =
            get_reportable_typing_rules_violations_for_mode
              ~nullsafe_mode:(NullsafeMode.Local NullsafeMode.Trust.All) all_issues
            |> List.length
          in
          ( IssueType.eradicate_meta_class_needs_improvement
          , Format.asprintf "`%s` needs %d issues to be fixed in order to be marked @Nullsafe."
              (JavaClassName.classname top_level_class_name)
              issue_count_to_make_nullsafe
          , IssueType.Info )
    else if currently_reportable_issue_count > 0 then
      (* This class is @Nullsafe, but broken. This should not happen often if there is enforcement for
         @Nullsafe mode error in the target codebase. *)
      ( IssueType.eradicate_meta_class_needs_improvement
      , Format.asprintf
          "@Nullsafe classes should have exactly zero nullability issues. `%s` has %d."
          (JavaClassName.classname top_level_class_name)
          currently_reportable_issue_count
      , IssueType.Info )
    else
      ( IssueType.eradicate_meta_class_is_nullsafe
      , Format.asprintf "Class %a is free of nullability issues." JavaClassName.pp
          top_level_class_name
      , IssueType.Info )
  in
  {issue_type; description; severity; meta_issue_info}


let get_class_loc source_file Struct.{java_class_info} =
  let default = {Location.file= source_file; line= 1; col= 0} in
  match java_class_info with
  | Some {loc} ->
      (* In rare cases location is not present, fall back to the first line of the file *)
      Option.value loc ~default
  | None ->
      L.internal_error "java_class_info should be present for Java classes" ;
      default


(* Meta issues are those related to null-safety of the class in general, not concrete nullability violations *)
let report_meta_issue_for_top_level_class tenv source_file class_name class_struct class_info
    issue_log =
  if Option.is_some (JavaClassName.get_outer_class_name class_name) then
    (* We record meta-issues only for top-level classes *) issue_log
  else
    let current_mode = NullsafeMode.of_class tenv class_name in
    (* For purposes of aggregation, we consider all nested summaries as belonging to this class *)
    let class_names_and_summaries =
      AggregatedSummaries.ClassInfo.get_recursive_summaries class_info
    in
    let class_loc = get_class_loc source_file class_struct in
    let all_issues =
      List.map class_names_and_summaries ~f:(fun (class_name, NullsafeSummary.{issues}) ->
          let mode_for_class_name = NullsafeMode.of_class tenv class_name in
          List.map issues ~f:(fun issue -> (mode_for_class_name, issue)) )
      |> List.fold ~init:[] ~f:( @ )
    in
    let {issue_type; description; severity; meta_issue_info} =
      make_meta_issue all_issues current_mode class_name
    in
    let package = JavaClassName.package class_name in
    let class_name = JavaClassName.classname class_name in
    let nullsafe_extra =
      Jsonbug_t.
        { class_name
        ; package
        ; method_info= None
        ; inconsistent_param_index= None
        ; parameter_not_nullable_info= None
        ; meta_issue_info= Some meta_issue_info
        ; unvetted_3rd_party= None
        ; nullable_methods= None
        ; field= None
        ; annotation_graph= None }
    in
    log_issue ~issue_log ~loc:class_loc ~severity ~nullsafe_extra issue_type description


(* Optimization - if issues are disabled, don't bother analyzing them *)
let should_analyze_meta_issues () =
  (not Config.filtering) || IssueType.eradicate_meta_class_can_be_nullsafe.enabled
  || IssueType.eradicate_meta_class_needs_improvement.enabled
  || IssueType.eradicate_meta_class_is_nullsafe.enabled


let analyze_meta_issue_for_top_level_class tenv source_file class_name class_struct class_info
    issue_log =
  if should_analyze_meta_issues () then
    report_meta_issue_for_top_level_class tenv source_file class_name class_struct class_info
      issue_log
  else issue_log


let analyze_nullsafe_annotations tenv source_file class_name class_struct issue_log =
  let loc = get_class_loc source_file class_struct in
  let nullsafe_extra =
    let package = JavaClassName.package class_name in
    let class_name = JavaClassName.classname class_name in
    Jsonbug_t.
      { class_name
      ; package
      ; method_info= None
      ; inconsistent_param_index= None
      ; parameter_not_nullable_info= None
      ; meta_issue_info= None
      ; unvetted_3rd_party= None
      ; nullable_methods= None
      ; field= None
      ; annotation_graph= None }
  in
  match NullsafeMode.check_problematic_class_annotation tenv class_name with
  | Ok () ->
      issue_log
  | Error NullsafeMode.RedundantNestedClassAnnotation ->
      let description =
        Format.sprintf
          "`%s`: the same @Nullsafe mode is already specified in the outer class, so this \
           annotation can be removed."
          (JavaClassName.classname class_name)
      in
      log_issue ~issue_log ~loc ~nullsafe_extra ~severity:Advice
        IssueType.eradicate_redundant_nested_class_annotation description
  | Error (NullsafeMode.NestedModeIsWeaker (ExtraTrustClass wrongly_trusted_classes)) ->
      (* The list can not be empty *)
      let example_of_wrongly_trusted_class = List.nth_exn wrongly_trusted_classes 0 in
      let description =
        Format.sprintf
          "Nested classes cannot add classes to trust list if they are not in the outer class \
           trust list. Remove `%s` from trust list."
          (JavaClassName.classname example_of_wrongly_trusted_class)
      in
      log_issue ~issue_log ~loc ~nullsafe_extra ~severity:Warning
        IssueType.eradicate_bad_nested_class_annotation description
  | Error (NullsafeMode.NestedModeIsWeaker Other) ->
      let description =
        Format.sprintf
          "`%s`: nested classes are disallowed to weaken @Nullsafe mode specified in the outer \
           class. This annotation will be ignored."
          (JavaClassName.classname class_name)
      in
      log_issue ~issue_log ~loc ~nullsafe_extra ~severity:Warning
        IssueType.eradicate_bad_nested_class_annotation description


let report_annotation_graph source_file class_name class_struct annotation_graph issue_log =
  let class_loc = get_class_loc source_file class_struct in
  let package = JavaClassName.package class_name in
  let class_name = JavaClassName.classname class_name in
  let nullsafe_extra =
    Jsonbug_t.
      { class_name
      ; package
      ; method_info= None
      ; inconsistent_param_index= None
      ; parameter_not_nullable_info= None
      ; meta_issue_info= None
      ; unvetted_3rd_party= None
      ; nullable_methods= None
      ; field= None
      ; annotation_graph= Some annotation_graph }
  in
  log_issue ~issue_log ~loc:class_loc ~severity:IssueType.Info ~nullsafe_extra
    IssueType.eradicate_annotation_graph ""


let build_and_report_annotation_graph tenv source_file class_name class_struct class_info issue_log
    =
  if not Config.nullsafe_annotation_graph then issue_log
  else
    let class_typ_name = Typ.JavaClass class_name in
    let provisional_violations =
      AggregatedSummaries.ClassInfo.get_summaries class_info
      |> List.map ~f:(fun NullsafeSummary.{issues} -> issues)
      |> List.concat
      |> List.filter_map ~f:ProvisionalViolation.of_issue
    in
    let annotation_graph =
      AnnotationGraph.build_graph tenv class_struct class_typ_name provisional_violations
    in
    report_annotation_graph source_file class_name class_struct annotation_graph issue_log


let analyze_class_impl tenv source_file class_name class_struct class_info issue_log =
  issue_log
  |> analyze_meta_issue_for_top_level_class tenv source_file class_name class_struct class_info
  |> analyze_nullsafe_annotations tenv source_file class_name class_struct
  |> build_and_report_annotation_graph tenv source_file class_name class_struct class_info


let analyze_class tenv source_file class_info issue_log =
  let class_name = AggregatedSummaries.ClassInfo.get_class_name class_info in
  match Tenv.lookup tenv (Typ.JavaClass class_name) with
  | Some class_struct ->
      analyze_class_impl tenv source_file class_name class_struct class_info issue_log
  | None ->
      L.debug Analysis Medium
        "%a: could not load class info in environment: skipping class analysis@\n" JavaClassName.pp
        class_name ;
      issue_log
