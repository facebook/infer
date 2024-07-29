(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_taint_extra fmt
    Jsonbug_t.{taint_source; taint_sink; taint_policy_privacy_effect; tainted_expression} =
  (* taint_source is expected to be always present *)
  Option.iter taint_source ~f:(fun source -> F.fprintf fmt "source: %s" source) ;
  Option.iter taint_sink ~f:(fun sink -> F.fprintf fmt ", sink: %s" sink) ;
  Option.iter tainted_expression ~f:(fun expr -> F.fprintf fmt ", tainted expression: %s" expr) ;
  Option.iter taint_policy_privacy_effect ~f:(fun effect ->
      F.fprintf fmt ", privacy effect: %s" effect )


let pp_trace fmt trace comma =
  let pp_trace_elem fmt Jsonbug_t.{description} = F.pp_print_string fmt description in
  let trace_without_empty_descs =
    List.filter ~f:(fun Jsonbug_t.{description} -> not (String.is_empty description)) trace
  in
  F.fprintf fmt "%s[%a]" comma (Pp.comma_seq pp_trace_elem) trace_without_empty_descs


let pp_bug_type fmt issue comma_separator index =
  let open Jsonbug_t in
  let issue_type_override =
    let open IOption.Let_syntax in
    let* extras = issue.extras in
    let* taint_extras = extras.taint_extra in
    taint_extras.report_as_issue_type
  in
  let issue_type = Option.value issue_type_override ~default:issue.bug_type in
  F.fprintf fmt "%s%s" (comma_separator index) issue_type


let pp_custom_of_report fmt report fields =
  let pp_custom_of_issue fmt (issue : Jsonbug_t.jsonbug) =
    let open Jsonbug_t in
    let comma_separator index = if index > 0 then ", " else "" in
    let pp_field index field =
      match (field : IssuesTestField.t) with
      | BugType ->
          pp_bug_type fmt issue comma_separator index
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
      | Suggestion ->
          F.fprintf fmt "%s%s" (comma_separator index) (Option.value ~default:"" issue.suggestion)
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
      | TaintExtra ->
          let taint_extra = Option.bind issue.extras ~f:(fun extras -> extras.taint_extra) in
          Option.iter taint_extra ~f:(fun taint_extra ->
              F.fprintf fmt "%s%a" (comma_separator index) pp_taint_extra taint_extra )
      | TransitiveCalleesExtra ->
          let pp_item fmt
              {Jsonbug_t.caller_name; callsite_relative_position_in_caller; kind; resolution} =
            F.fprintf fmt "%s:%d:%a:%a" caller_name callsite_relative_position_in_caller
              (Pp.of_string ~f:Jsonbug_j.string_of_transitive_callee_kind)
              kind
              (Pp.of_string ~f:Jsonbug_j.string_of_transitive_callee_resolution)
              resolution
          in
          let pp_missed_capture fmt {Jsonbug_t.class_name} = F.pp_print_string fmt class_name in
          Option.iter issue.extras ~f:(fun extras ->
              let transitive_callees = extras.transitive_callees in
              if not (List.is_empty transitive_callees) then
                F.fprintf fmt "%s{%a}" (comma_separator index) (Pp.seq ~sep:"," pp_item)
                  transitive_callees ;
              let transitive_missed_captures = extras.transitive_missed_captures in
              if not (List.is_empty transitive_missed_captures) then
                F.fprintf fmt "%s{%a}" (comma_separator index)
                  (Pp.seq ~sep:"," pp_missed_capture)
                  transitive_missed_captures )
    in
    List.iteri ~f:pp_field fields ;
    F.fprintf fmt "@."
  in
  List.iter ~f:(pp_custom_of_issue fmt) report


let write_from_json ~json_path ~out_path issues_tests_fields =
  Utils.with_file_out out_path ~f:(fun outf ->
      let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report json_path in
      pp_custom_of_report (F.formatter_of_out_channel outf) report issues_tests_fields )
