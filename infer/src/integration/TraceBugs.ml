(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

[@@@warning "+9"]

let is_past_limit limit =
  match limit with None -> fun _ -> false | Some limit -> fun n -> n >= limit


let pp_trace_item ~show_source_context fmt
    Jsonbug_t.{level; filename; line_number; column_number; description} =
  let pp_col_number fmt c = if c >= 0 then F.fprintf fmt ":%d" c in
  F.fprintf fmt "%s:%d%a: %s@\n" filename line_number pp_col_number column_number description ;
  if show_source_context then
    TextReport.pp_source_context ~indent:(2 * level) fmt
      {Jsonbug_t.file= filename; lnum= line_number; cnum= column_number; enum= -1}


let show_issue_with_trace ~show_source_context ~max_nested_level
    (n_issue, (issue : Jsonbug_t.jsonbug)) =
  L.result "#%d@\n%a@\n" n_issue TextReport.pp_jsonbug issue ;
  if List.is_empty issue.bug_trace then L.result "@\nEmpty trace@\n%!"
  else
    List.iter issue.bug_trace ~f:(fun trace_item ->
        (* subtract 1 to get inclusive limits on the nesting level *)
        if not (is_past_limit max_nested_level (trace_item.Jsonbug_t.level - 1)) then
          L.result "@\n%a" (pp_trace_item ~show_source_context) trace_item )


let user_select_issue ~selector_limit report =
  List.fold_until report
    ~finish:(fun _ -> ())
    ~init:0
    ~f:(fun n issue ->
      if is_past_limit selector_limit n then Stop ()
      else (
        L.result "#%d@\n%a@\n" n TextReport.pp_jsonbug issue ;
        Continue (n + 1) ) ) ;
  let rec ask_until_valid_input max_report =
    L.result "@\nSelect report to display (0-%d) (default: 0): %!" max_report ;
    let input = In_channel.input_line_exn In_channel.stdin in
    if String.is_empty input then 0
    else
      match int_of_string_opt input with
      | Some n when n >= 0 && n <= max_report ->
          n
      | Some n ->
          L.progress "Error: %d is not between 0 and %d, try again.@\n%!" n max_report ;
          ask_until_valid_input max_report
      | None ->
          L.progress "Error: please input a number, not '%s'.@\n%!" input ;
          ask_until_valid_input max_report
  in
  let n = ask_until_valid_input (List.length report - 1) in
  (n, List.nth_exn report n)


let explore ~selector_limit ~report_txt:_ ~report_json ~show_source_context ~selected
    ~max_nested_level =
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report report_json in
  let issue_to_display =
    match (selected, report) with
    | Some n, _ -> (
      (* an issue number has been pre-selected, use that *)
      match List.nth report n with
      | None ->
          L.die UserError "Cannot select issues #%d: only %d issues in '%s'" n (List.length report)
            report_json
      | Some issue ->
          Some (n, issue) )
    | None, [] ->
        (* empty report, can't print anything *)
        L.progress "No issues found in '%s', exiting.@\n" report_json ;
        None
    | None, [issue] ->
        (* single-issue report: no need to prompt the user to select which issue to display *)
        L.progress "Auto-selecting the only issue in '%s'@\n%!" report_json ;
        Some (0, issue)
    | None, _ :: _ :: _ ->
        (* user prompt *)
        Some (user_select_issue ~selector_limit report)
  in
  Option.iter issue_to_display
    ~f:
      ( L.result "@\n" ;
        show_issue_with_trace ~show_source_context ~max_nested_level )
