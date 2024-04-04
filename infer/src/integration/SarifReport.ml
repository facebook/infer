(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module IssueHash = Caml.Hashtbl.Make (String)

module ReportSummary = struct
  type t = {mutable n_issues: int; issue_type_counts: (int * string) IssueHash.t}

  let mk_empty () = {n_issues= 0; issue_type_counts= IssueHash.create 64}

  let is_first_rule_item = ref true

  let pp fmt {n_issues= _; issue_type_counts} =
    let string_of_issue ~issue_type ~issue_type_hum =
      let shortDescription = {Sarifbug_j.text= issue_type_hum} in
      let help_uri = "https://fbinfer.com" ^ Help.abs_url_of_issue_type issue_type in
      let rule = {Sarifbug_j.id= issue_type; shortDescription; helpUri= help_uri} in
      Sarifbug_j.string_of_rule rule
    in
    IssueHash.to_seq issue_type_counts
    |> Seq.iter (fun (issue_type, (_, issue_type_hum)) ->
           if !is_first_rule_item then is_first_rule_item := false else F.pp_print_char fmt ',' ;
           let issue_string = string_of_issue ~issue_type ~issue_type_hum in
           F.pp_print_string fmt issue_string )


  let add_issue summary (jsonbug : Jsonbug_t.jsonbug) =
    let bug_count =
      IssueHash.find_opt summary.issue_type_counts jsonbug.bug_type
      |> Option.value_map ~default:0 ~f:fst
    in
    IssueHash.replace summary.issue_type_counts jsonbug.bug_type
      (bug_count + 1, jsonbug.bug_type_hum) ;
    summary.n_issues <- summary.n_issues + 1 ;
    (* chain for convenience/pretending it's a functional data structure *)
    summary
end

let is_first_item = ref true

let pp_open fmt =
  is_first_item := true ;
  F.fprintf fmt "{"


let pp_close fmt = F.fprintf fmt "]}]}@\n@?"

let pp_schema_version fmt =
  F.fprintf fmt {|"$schema":"%s","version":"%s",|} "http://json.schemastore.org/sarif-2.1.0-rtm.5"
    "2.1.0"


let pp_runs fmt =
  F.fprintf fmt {|"runs":[{"tool":{"driver":{"name":"%s","informationUri":"%s",|} "Infer"
    "https://github.com/facebook/infer" ;
  F.fprintf fmt {|"version":"%d.%d.%d","rules":[|} Version.major Version.minor Version.patch


let pp_results_header fmt =
  is_first_item := true ;
  F.fprintf fmt {|]}},"results":[|}


let loc_trace_to_sarifbug_record trace_list =
  let file_loc filename =
    let absolute_source_name = Config.project_root ^/ filename in
    let file_path = "file:" ^ filename in
    {Sarifbug_j.uri= file_path; Sarifbug_j.uriBaseId= absolute_source_name}
  in
  let message description = {Sarifbug_j.text= description} in
  let region line_number column_number =
    let line_num = match line_number with -1 | 0 -> 1 | _ -> line_number in
    let column_num = match column_number with -1 | 0 -> 1 | _ -> column_number in
    {Sarifbug_j.startLine= line_num; startColumn= column_num}
  in
  let physical_location filename line_number column_number =
    {Sarifbug_j.artifactLocation= file_loc filename; region= region line_number column_number}
  in
  let file_location_to_record filename line_number column_number description =
    { Sarifbug_j.message= message description
    ; physicalLocation= physical_location filename line_number column_number }
  in
  let trace_item_to_record {Jsonbug_t.level; filename; line_number; column_number; description} =
    { Sarifbug_j.nestingLevel= level
    ; location= file_location_to_record filename line_number column_number description }
  in
  List.map ~f:trace_item_to_record trace_list


let pp_jsonbug fmt
    {Jsonbug_t.file; severity; bug_type; qualifier; suggestion; line; column; bug_trace; hash; key}
    =
  let message =
    { Sarifbug_j.text=
        qualifier ^ Option.value_map ~default:"" ~f:(fun sugg -> " " ^ sugg) suggestion }
  in
  let level = String.lowercase severity in
  let ruleId = bug_type in
  let absolute_source_name = Config.project_root ^/ file in
  let file_path = "file:" ^ file in
  let file_loc = {Sarifbug_j.uri= file_path; uriBaseId= absolute_source_name} in
  let region =
    match column with
    | -1 ->
        {Sarifbug_j.startLine= line; startColumn= 1}
    | _ ->
        {Sarifbug_j.startLine= line; startColumn= column}
  in
  let physical_location = {Sarifbug_j.artifactLocation= file_loc; region} in
  let file_location_to_record = [{Sarifbug_j.physicalLocation= physical_location}] in
  let thread_flow_locs = [{Sarifbug_j.locations= loc_trace_to_sarifbug_record bug_trace}] in
  let trace_list_length = List.length bug_trace in
  let thread_flow =
    if trace_list_length > 0 then Some [{Sarifbug_j.threadFlows= thread_flow_locs}] else None
  in
  let fingerprints = {Sarifbug_j.hashV1= hash; key} in
  let result =
    { Sarifbug_j.message
    ; level
    ; ruleId
    ; codeFlows= thread_flow
    ; locations= file_location_to_record
    ; fingerprints }
  in
  F.pp_print_string fmt (Sarifbug_j.string_of_sarifbug result)


let create_from_json ~report_sarif ~report_json =
  let report = Atdgen_runtime.Util.Json.from_file Jsonbug_j.read_report report_json in
  let one_issue_to_report_sarif fmt (jsonbug : Jsonbug_t.jsonbug) =
    if !is_first_item then is_first_item := false else F.pp_print_char fmt ',' ;
    F.fprintf fmt "%a" pp_jsonbug jsonbug
  in
  Utils.with_file_out report_sarif ~f:(fun report_sarif_out ->
      let report_sarif_fmt = F.formatter_of_out_channel report_sarif_out in
      pp_open report_sarif_fmt ;
      pp_schema_version report_sarif_fmt ;
      pp_runs report_sarif_fmt ;
      let summary =
        List.fold report ~init:(ReportSummary.mk_empty ()) ~f:(fun summary jsonbug ->
            let summary' = ReportSummary.add_issue summary jsonbug in
            summary' )
      in
      F.fprintf report_sarif_fmt "%a" ReportSummary.pp summary ;
      pp_results_header report_sarif_fmt ;
      List.iter ~f:(fun jsonbug -> one_issue_to_report_sarif report_sarif_fmt jsonbug) report ;
      pp_close report_sarif_fmt )
