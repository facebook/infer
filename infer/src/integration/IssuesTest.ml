(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_method_info fmt Jsonbug_t.{class_name; package; method_name; call_line} =
  F.fprintf fmt "%s.%s.%s at %d" package class_name method_name call_line


let pp_nullsafe_extra fmt
    Jsonbug_t.
      { class_name
      ; package
      ; inconsistent_param_index
      ; meta_issue_info
      ; unvetted_3rd_party
      ; nullable_methods
      ; annotation_graph
      ; field } =
  F.fprintf fmt "%s, %s" class_name (Option.value package ~default:"<no package>") ;
  Option.iter unvetted_3rd_party ~f:(fun unvetted_3rd_party ->
      let third_party_str = String.concat unvetted_3rd_party ~sep:"," in
      F.fprintf fmt ", unvetted_3rd_party:[%s]" third_party_str ) ;
  Option.iter nullable_methods ~f:(fun nullable_methods ->
      F.fprintf fmt ", nullable_methods:%a" (Pp.seq pp_method_info) nullable_methods ) ;
  Option.iter field ~f:(fun Jsonbug_t.{class_name; package_name; field} ->
      F.fprintf fmt ", field:%s.%s.%s" (Option.value package_name ~default:"") class_name field ) ;
  Option.iter inconsistent_param_index ~f:(fun index ->
      F.fprintf fmt ", inconsistent_param_index:%d" index ) ;
  Option.iter meta_issue_info
    ~f:(fun Jsonbug_t.{num_issues; curr_nullsafe_mode; can_be_promoted_to} ->
      let can_be_promoted_to_str =
        Option.value_map can_be_promoted_to
          ~f:(fun mode -> F.sprintf ", promote_mode: %s" (Jsonbug_j.string_of_nullsafe_mode mode))
          ~default:""
      in
      F.fprintf fmt ", issues: %d, curr_mode: %s%s" num_issues
        (Jsonbug_j.string_of_nullsafe_mode curr_nullsafe_mode)
        can_be_promoted_to_str ) ;
  Option.iter annotation_graph ~f:(fun annotation_graph ->
      F.fprintf fmt "\nAnnotationGraph:@\n  @[%a@]" NullsafeAnnotationGraphUtils.pp_annotation_graph
        annotation_graph )


let pp_trace fmt trace comma =
  let pp_trace_elem fmt Jsonbug_t.{description} = F.pp_print_string fmt description in
  let trace_without_empty_descs =
    List.filter ~f:(fun Jsonbug_t.{description} -> not (String.is_empty description)) trace
  in
  F.fprintf fmt "%s[%a]" comma (Pp.comma_seq pp_trace_elem) trace_without_empty_descs


let pp_custom_of_report fmt report fields =
  let pp_custom_of_issue fmt (issue : Jsonbug_t.jsonbug) =
    let open Jsonbug_t in
    let comma_separator index = if index > 0 then ", " else "" in
    let pp_field index field =
      match (field : IssuesTestField.t) with
      | BugType ->
          F.fprintf fmt "%s%s" (comma_separator index) issue.bug_type
      | Bucket ->
          let bucket =
            match
              String.lsplit2 issue.qualifier ~on:']'
              |> Option.map ~f:fst
              |> Option.bind ~f:(String.chop_prefix ~prefix:"[")
            with
            | Some bucket ->
                bucket
            | None ->
                "no_bucket"
          in
          F.fprintf fmt "%s%s" (comma_separator index) bucket
      | Qualifier ->
          F.fprintf fmt "%s%s" (comma_separator index) issue.qualifier
      | Severity ->
          F.fprintf fmt "%s%s" (comma_separator index) issue.severity
      | Line ->
          F.fprintf fmt "%s%d" (comma_separator index) issue.line
      | Column ->
          F.fprintf fmt "%s%d" (comma_separator index) issue.column
      | Procedure ->
          F.fprintf fmt "%s%s" (comma_separator index) issue.procedure
      | ProcedureStartLine ->
          F.fprintf fmt "%s%d" (comma_separator index) issue.procedure_start_line
      | File ->
          F.fprintf fmt "%s%s" (comma_separator index) issue.file
      | BugTrace ->
          pp_trace fmt issue.bug_trace (comma_separator index)
      | Key ->
          F.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.key)
      | Hash ->
          F.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.hash)
      | LineOffset ->
          F.fprintf fmt "%s%d" (comma_separator index) (issue.line - issue.procedure_start_line)
      | QualifierContainsPotentialExceptionNote ->
          F.pp_print_bool fmt
            (String.is_substring issue.qualifier ~substring:JsonReports.potential_exception_message)
      | NullsafeExtra ->
          let nullsafe_extra = Option.bind issue.extras ~f:(fun extras -> extras.nullsafe_extra) in
          Option.iter nullsafe_extra ~f:(fun nullsafe_extra ->
              F.fprintf fmt "%s%a" (comma_separator index) pp_nullsafe_extra nullsafe_extra )
    in
    List.iteri ~f:pp_field fields ;
    F.fprintf fmt "@."
  in
  List.iter ~f:(pp_custom_of_issue fmt) report


let tests_jsonbug_compare (bug1 : Jsonbug_t.jsonbug) (bug2 : Jsonbug_t.jsonbug) =
  let open Jsonbug_t in
  [%compare: string * string * int * string * Caml.Digest.t]
    (bug1.file, bug1.procedure, bug1.line - bug1.procedure_start_line, bug1.bug_type, bug1.hash)
    (bug2.file, bug2.procedure, bug2.line - bug2.procedure_start_line, bug2.bug_type, bug2.hash)


let write_from_json ~json_path ~out_path issues_tests_fields =
  Utils.with_file_out out_path ~f:(fun outf ->
      let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report json_path in
      let sorted_report = List.sort ~compare:tests_jsonbug_compare report in
      pp_custom_of_report (F.formatter_of_out_channel outf) sorted_report issues_tests_fields )
