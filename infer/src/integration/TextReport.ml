(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

[@@@warning "+9"]

(* how many lines of context around each report *)
let source_context = 2

module IssueHash = Caml.Hashtbl.Make (String)

let pp_n_spaces n fmt =
  for _ = 1 to n do
    F.pp_print_char fmt ' '
  done


module ReportSummary = struct
  type t = {mutable n_issues: int; issue_type_counts: int IssueHash.t}

  let mk_empty () = {n_issues= 0; issue_type_counts= IssueHash.create 64}

  let pp fmt {n_issues= _; issue_type_counts} =
    let max_issue_length, issue_counts =
      IssueHash.to_seq issue_type_counts
      |> Seq.fold_left
           (fun (max_issue_length, issue_counts) ((issue_type, _) as issue_with_count) ->
             let l = String.length issue_type in
             (Int.max max_issue_length l, issue_with_count :: issue_counts) )
           (0, [])
    in
    List.sort issue_counts ~compare:(fun (issue_type1, count1) (issue_type2, count2) ->
        (* reverse lexicographic order on (count * issue_type) *)
        if Int.equal count2 count1 then String.compare issue_type2 issue_type1
        else Int.compare count2 count1 )
    |> List.iter ~f:(fun (bug_type, count) ->
           pp_n_spaces (max_issue_length - String.length bug_type) fmt ;
           F.fprintf fmt "  %s: %d@\n" bug_type count )


  let add_issue summary (jsonbug : Jsonbug_t.jsonbug) =
    let bug_count =
      IssueHash.find_opt summary.issue_type_counts jsonbug.bug_type |> Option.value ~default:0
    in
    IssueHash.replace summary.issue_type_counts jsonbug.bug_type (bug_count + 1) ;
    summary.n_issues <- summary.n_issues + 1 ;
    (* chain for convenience/pretending it's a functional data structure *)
    summary
end

let pp_jsonbug fmt {Jsonbug_t.file; severity; line; bug_type; qualifier; _} =
  F.fprintf fmt "%s:%d: %s: %s@\n  %s" file line (String.lowercase severity) bug_type qualifier


let pp_source_context ~indent fmt
    {Jsonbug_t.file= source_name; lnum= report_line; cnum= report_col; enum= _} =
  let source_name =
    if Filename.is_absolute source_name then source_name else Config.project_root ^/ source_name
  in
  match Sys.is_file source_name with
  | `No | `Unknown ->
      ()
  | `Yes ->
      let start_line = max 1 (report_line - source_context) in
      (* could go beyond last line *)
      let end_line = report_line + source_context in
      let n_length = String.length (string_of_int end_line) in
      Utils.with_file_in source_name ~f:(fun in_chan ->
          Container.fold_until in_chan
            ~fold:(In_channel.fold_lines ~fix_win_eol:false)
            ~finish:(fun _final_line_number -> ())
            ~init:1
            ~f:(fun line_number line ->
              if start_line <= line_number && line_number <= end_line then (
                (* we are inside the context to print *)
                F.fprintf fmt "%t%*d. " (pp_n_spaces indent) n_length line_number ;
                if report_col < 0 then
                  (* no column number, print caret next to the line of the report *)
                  if Int.equal line_number report_line then F.pp_print_string fmt "> "
                  else F.pp_print_string fmt "  " ;
                F.pp_print_string fmt line ;
                F.pp_print_newline fmt () ;
                if Int.equal line_number report_line && report_col >= 0 then (
                  pp_n_spaces (indent + n_length + 1 + report_col) fmt ;
                  F.pp_print_char fmt '^' ;
                  F.pp_print_newline fmt () ) ) ;
              if line_number < end_line then Continue (line_number + 1) else Stop () ) )


let create_from_json ~quiet ~console_limit ~report_txt ~report_json =
  (* TOOD: possible optimisation: stream reading report.json to process each issue one by one *)
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report report_json in
  let one_issue_to_report_txt fmt (jsonbug : Jsonbug_t.jsonbug) =
    F.fprintf fmt "%a@\n%a@\n" pp_jsonbug jsonbug (pp_source_context ~indent:2)
      {Jsonbug_t.file= jsonbug.file; lnum= jsonbug.line; cnum= jsonbug.column; enum= -1}
  in
  let one_issue_to_console ~console_limit i (jsonbug : Jsonbug_t.jsonbug) =
    match console_limit with
    | Some limit when i >= limit ->
        ()
    | _ ->
        let style =
          match jsonbug.severity with
          | "ERROR" ->
              ANSITerminal.[Foreground Red]
          | "WARNING" ->
              ANSITerminal.[Foreground Yellow]
          | _ ->
              []
        in
        F.printf "%!" ;
        ANSITerminal.print_string style (F.asprintf "%a" pp_jsonbug jsonbug) ;
        F.printf "%!" ;
        F.printf "@\n%a@\n" (pp_source_context ~indent:2)
          {Jsonbug_t.file= jsonbug.file; lnum= jsonbug.line; cnum= jsonbug.column; enum= -1}
  in
  Utils.with_file_out report_txt ~f:(fun report_txt_out ->
      let report_txt_fmt = F.formatter_of_out_channel report_txt_out in
      if not quiet then F.printf "@\n@[" ;
      let summary =
        List.foldi report ~init:(ReportSummary.mk_empty ()) ~f:(fun i summary jsonbug ->
            let summary' = ReportSummary.add_issue summary jsonbug in
            one_issue_to_report_txt report_txt_fmt jsonbug ;
            if not quiet then one_issue_to_console ~console_limit i jsonbug ;
            summary' )
      in
      let n_issues = summary.n_issues in
      if Int.equal n_issues 0 then (
        if not quiet then (
          F.printf "%!" ;
          ANSITerminal.(printf [Background Magenta; Bold; Foreground White]) "  No issues found  " ;
          F.printf "@\n%!" ) ;
        F.pp_print_string report_txt_fmt "@\nNo issues found@\n" )
      else
        let s_of_issues = if n_issues > 1 then "s" else "" in
        if not quiet then (
          F.printf "@\n%!" ;
          ANSITerminal.(printf [Bold]) "Found %d issue%s" n_issues s_of_issues ;
          ( match console_limit with
          | Some limit when n_issues >= limit ->
              F.printf " (console output truncated to %d, see '%s' for the full list)" limit
                report_txt
          | _ ->
              () ) ;
          F.printf "@\n%a@]%!" ReportSummary.pp summary ) ;
        F.fprintf report_txt_fmt "Found %d issue%s@\n%a%!" n_issues s_of_issues ReportSummary.pp
          summary )
