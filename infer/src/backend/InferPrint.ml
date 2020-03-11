(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let error_desc_to_plain_string error_desc =
  let pp fmt = Localise.pp_error_desc fmt error_desc in
  let s = F.asprintf "%t" pp in
  let s = String.strip s in
  let s =
    (* end error description with a dot *)
    if String.is_suffix ~suffix:"." s then s else s ^ "."
  in
  s


let error_desc_to_dotty_string error_desc = Localise.error_desc_get_dotty error_desc

let compute_key (bug_type : string) (proc_name : Procname.t) (filename : string) =
  let base_filename = Filename.basename filename
  and simple_procedure_name = Procname.get_method proc_name in
  String.concat ~sep:"|" [base_filename; simple_procedure_name; bug_type]


let compute_hash ~(severity : string) ~(bug_type : string) ~(proc_name : Procname.t)
    ~(file : string) ~(qualifier : string) =
  let base_filename = Filename.basename file in
  let hashable_procedure_name = Procname.hashable_name proc_name in
  let location_independent_qualifier =
    (* Removing the line,column, and infer temporary variable (e.g., n$67) information from the
       error message as well as the index of the annonymmous class to make the hash invariant
       when moving the source code in the file *)
    Str.global_replace (Str.regexp "\\(line \\|column \\|parameter \\|\\$\\)[0-9]+") "$_" qualifier
  in
  Utils.better_hash
    (severity, bug_type, hashable_procedure_name, base_filename, location_independent_qualifier)
  |> Caml.Digest.to_hex


let loc_trace_to_jsonbug_record trace_list ekind =
  match ekind with
  | Exceptions.Info ->
      []
  | _ ->
      let trace_item_to_record trace_item =
        { Jsonbug_j.level= trace_item.Errlog.lt_level
        ; filename= SourceFile.to_string trace_item.Errlog.lt_loc.Location.file
        ; line_number= trace_item.Errlog.lt_loc.Location.line
        ; column_number= trace_item.Errlog.lt_loc.Location.col
        ; description= trace_item.Errlog.lt_description }
      in
      let record_list = List.rev (List.rev_map ~f:trace_item_to_record trace_list) in
      record_list


let should_report (issue_kind : Exceptions.severity) issue_type error_desc eclass =
  if (not Config.filtering) || Exceptions.equal_err_class eclass Exceptions.Linters then true
  else
    let issue_kind_is_blacklisted =
      match issue_kind with Info -> true | Advice | Error | Like | Warning -> false
    in
    if issue_kind_is_blacklisted then false
    else
      let issue_type_is_null_deref =
        let null_deref_issue_types =
          let open IssueType in
          [ field_not_null_checked
          ; null_dereference
          ; parameter_not_null_checked
          ; premature_nil_termination
          ; empty_vector_access
          ; biabd_use_after_free ]
        in
        List.mem ~equal:IssueType.equal null_deref_issue_types issue_type
      in
      if issue_type_is_null_deref then Localise.error_desc_is_reportable_bucket error_desc else true


(* The reason an issue should be censored (that is, not reported). The empty
   string (that is "no reason") means that the issue should be reported. *)
let censored_reason (issue_type : IssueType.t) source_file =
  let filename = SourceFile.to_rel_path source_file in
  let rejected_by ((issue_type_polarity, issue_type_re), (filename_polarity, filename_re), reason) =
    let accepted =
      (* matches issue_type_re implies matches filename_re *)
      (not (Bool.equal issue_type_polarity (Str.string_match issue_type_re issue_type.unique_id 0)))
      || Bool.equal filename_polarity (Str.string_match filename_re filename 0)
    in
    Option.some_if (not accepted) reason
  in
  List.find_map Config.censor_report ~f:rejected_by


let potential_exception_message = "potential exception at line"

module type Printer = sig
  type elt

  val pp_open : F.formatter -> unit -> unit

  val pp_close : F.formatter -> unit -> unit

  val pp : F.formatter -> elt -> unit
end

module MakeJsonListPrinter (P : sig
  type elt

  val to_string : elt -> string option
end) : Printer with type elt = P.elt = struct
  include P

  let is_first_item = ref true

  let pp_open fmt () =
    is_first_item := true ;
    F.fprintf fmt "[@?"


  let pp_close fmt () = F.fprintf fmt "]@\n@?"

  let pp fmt elt =
    match to_string elt with
    | Some s ->
        if !is_first_item then is_first_item := false else F.pp_print_char fmt ',' ;
        F.fprintf fmt "%s@?" s
    | None ->
        ()
end

type json_issue_printer_typ =
  { error_filter: SourceFile.t -> IssueType.t -> bool
  ; proc_name: Procname.t
  ; proc_loc_opt: Location.t option
  ; err_key: Errlog.err_key
  ; err_data: Errlog.err_data }

let procedure_id_of_procname proc_name =
  match Procname.get_language proc_name with
  | Language.Java ->
      Procname.to_unique_id proc_name
  | _ ->
      Procname.to_string proc_name


module JsonIssuePrinter = MakeJsonListPrinter (struct
  type elt = json_issue_printer_typ

  let to_string ({error_filter; proc_name; proc_loc_opt; err_key; err_data} : elt) =
    let source_file, procedure_start_line =
      match proc_loc_opt with
      | Some proc_loc ->
          (proc_loc.Location.file, proc_loc.Location.line)
      | None ->
          (err_data.loc.Location.file, 0)
    in
    if SourceFile.is_invalid source_file then
      L.(die InternalError)
        "Invalid source file for %a %a@.Trace: %a@." IssueType.pp err_key.err_name
        Localise.pp_error_desc err_key.err_desc Errlog.pp_loc_trace err_data.loc_trace ;
    let should_report_source_file =
      (not (SourceFile.is_biabduction_model source_file))
      || Config.debug_mode || Config.debug_exceptions
    in
    if
      error_filter source_file err_key.err_name
      && should_report_source_file
      && should_report err_key.severity err_key.err_name err_key.err_desc err_data.err_class
    then
      let severity = Exceptions.severity_string err_key.severity in
      let bug_type = err_key.err_name.IssueType.unique_id in
      let file =
        SourceFile.to_string ~force_relative:Config.report_force_relative_path source_file
      in
      let json_ml_loc =
        match err_data.loc_in_ml_source with
        | Some (file, lnum, cnum, enum) when Config.reports_include_ml_loc ->
            Some Jsonbug_j.{file; lnum; cnum; enum}
        | _ ->
            None
      in
      let qualifier =
        let base_qualifier = error_desc_to_plain_string err_key.err_desc in
        if IssueType.(equal resource_leak) err_key.err_name then
          match Errlog.compute_local_exception_line err_data.loc_trace with
          | None ->
              base_qualifier
          | Some line ->
              let potential_exception_message =
                Format.asprintf "%a: %s %d" MarkupFormatter.pp_bold "Note"
                  potential_exception_message line
              in
              Format.sprintf "%s@\n%s" base_qualifier potential_exception_message
        else base_qualifier
      in
      let bug =
        { Jsonbug_j.bug_type
        ; qualifier
        ; severity
        ; line= err_data.loc.Location.line
        ; column= err_data.loc.Location.col
        ; procedure= procedure_id_of_procname proc_name
        ; procedure_start_line
        ; file
        ; bug_trace= loc_trace_to_jsonbug_record err_data.loc_trace err_key.severity
        ; node_key= Option.map ~f:Procdesc.NodeKey.to_string err_data.node_key
        ; key= compute_key bug_type proc_name file
        ; hash= compute_hash ~severity ~bug_type ~proc_name ~file ~qualifier
        ; dotty= error_desc_to_dotty_string err_key.err_desc
        ; infer_source_loc= json_ml_loc
        ; bug_type_hum= err_key.err_name.IssueType.hum
        ; linters_def_file= err_data.linters_def_file
        ; doc_url= err_data.doc_url
        ; traceview_id= None
        ; censored_reason= censored_reason err_key.err_name source_file
        ; access= err_data.access
        ; extras= err_data.extras }
      in
      Some (Jsonbug_j.string_of_jsonbug bug)
    else None
end)

module IssuesJson = struct
  include JsonIssuePrinter

  (** Write bug report in JSON format *)
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt proc_name err_log =
    Errlog.iter
      (fun err_key err_data -> pp fmt {error_filter; proc_name; proc_loc_opt; err_key; err_data})
      err_log
end

type json_costs_printer_typ =
  {loc: Location.t; proc_name: Procname.t; cost_opt: CostDomain.summary option}

module JsonCostsPrinter = MakeJsonListPrinter (struct
  type elt = json_costs_printer_typ

  let to_string {loc; proc_name; cost_opt} =
    match cost_opt with
    | Some {post; is_on_ui_thread} when not (Procname.is_java_access_method proc_name) ->
        let hum cost =
          let degree_with_term = CostDomain.BasicCost.get_degree_with_term cost in
          { Jsonbug_t.hum_polynomial= Format.asprintf "%a" CostDomain.BasicCost.pp_hum cost
          ; hum_degree=
              Format.asprintf "%a"
                (CostDomain.BasicCost.pp_degree ~only_bigO:false)
                degree_with_term
          ; big_o=
              Format.asprintf "%a" (CostDomain.BasicCost.pp_degree ~only_bigO:true) degree_with_term
          }
        in
        let cost_info cost =
          { Jsonbug_t.polynomial_version= CostDomain.BasicCost.version
          ; polynomial= CostDomain.BasicCost.encode cost
          ; degree=
              Option.map (CostDomain.BasicCost.degree cost) ~f:Polynomials.Degree.encode_to_int
          ; hum= hum cost }
        in
        let cost_item =
          let file = SourceFile.to_rel_path loc.Location.file in
          { Jsonbug_t.hash= compute_hash ~severity:"" ~bug_type:"" ~proc_name ~file ~qualifier:""
          ; loc= {file; lnum= loc.Location.line; cnum= loc.Location.col; enum= -1}
          ; procedure_name= Procname.get_method proc_name
          ; procedure_id= procedure_id_of_procname proc_name
          ; is_on_ui_thread
          ; exec_cost= cost_info (CostDomain.get_cost_kind CostKind.OperationCost post) }
        in
        Some (Jsonbug_j.string_of_cost_item cost_item)
    | _ ->
        None
end)

let pp_custom_of_report fmt report fields =
  let pp_custom_of_issue fmt (issue : Jsonbug_t.jsonbug) =
    let open Jsonbug_t in
    let comma_separator index = if index > 0 then ", " else "" in
    let pp_trace fmt trace comma =
      let pp_trace_elem fmt {description} = F.pp_print_string fmt description in
      let trace_without_empty_descs =
        List.filter ~f:(fun {description} -> not (String.is_empty description)) trace
      in
      F.fprintf fmt "%s[%a]" comma (Pp.comma_seq pp_trace_elem) trace_without_empty_descs
    in
    let pp_field index field =
      match field with
      | `Issue_field_bug_type ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.bug_type
      | `Issue_field_bucket ->
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
          Format.fprintf fmt "%s%s" (comma_separator index) bucket
      | `Issue_field_qualifier ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.qualifier
      | `Issue_field_severity ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.severity
      | `Issue_field_line ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.line
      | `Issue_field_column ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.column
      | `Issue_field_procedure ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.procedure
      | `Issue_field_procedure_start_line ->
          Format.fprintf fmt "%s%d" (comma_separator index) issue.procedure_start_line
      | `Issue_field_file ->
          Format.fprintf fmt "%s%s" (comma_separator index) issue.file
      | `Issue_field_bug_trace ->
          pp_trace fmt issue.bug_trace (comma_separator index)
      | `Issue_field_key ->
          Format.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.key)
      | `Issue_field_hash ->
          Format.fprintf fmt "%s%s" (comma_separator index) (Caml.Digest.to_hex issue.hash)
      | `Issue_field_line_offset ->
          Format.fprintf fmt "%s%d" (comma_separator index) (issue.line - issue.procedure_start_line)
      | `Issue_field_qualifier_contains_potential_exception_note ->
          Format.pp_print_bool fmt
            (String.is_substring issue.qualifier ~substring:potential_exception_message)
    in
    List.iteri ~f:pp_field fields ; Format.fprintf fmt "@."
  in
  List.iter ~f:(pp_custom_of_issue fmt) report


let tests_jsonbug_compare (bug1 : Jsonbug_t.jsonbug) (bug2 : Jsonbug_t.jsonbug) =
  let open Jsonbug_t in
  [%compare: string * string * int * string * Caml.Digest.t]
    (bug1.file, bug1.procedure, bug1.line - bug1.procedure_start_line, bug1.bug_type, bug1.hash)
    (bug2.file, bug2.procedure, bug2.line - bug2.procedure_start_line, bug2.bug_type, bug2.hash)


let error_filter filters proc_name file error_name =
  (Config.write_html || not (IssueType.(equal skip_function) error_name))
  && filters.Inferconfig.path_filter file
  && filters.Inferconfig.error_filter error_name
  && filters.Inferconfig.proc_filter proc_name


type report_kind = Costs | Issues | Summary [@@deriving compare]

type bug_format_kind = Json | Tests [@@deriving compare]

let get_outfile outfile =
  match outfile with
  | Some outfile ->
      outfile
  | None ->
      L.(die InternalError) "An outfile is require for this format."


let pp_issue_in_format (format_kind, (outfile_opt : Utils.outfile option)) error_filter
    {Issue.proc_name; proc_location; err_key; err_data} =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      IssuesJson.pp outf.fmt
        {error_filter; proc_name; proc_loc_opt= Some proc_location; err_key; err_data}
  | Tests ->
      L.die InternalError "Printing issues as tests is not implemented"


let pp_issues_in_format (format_kind, (outfile_opt : Utils.outfile option)) =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      IssuesJson.pp_issues_of_error_log outf.fmt
  | Tests ->
      L.die InternalError "Printing issues as tests is not implemented"


let pp_issues_of_error_log error_filter linereader proc_loc_opt procname err_log bug_format_list =
  let pp_issues_in_format format =
    pp_issues_in_format format error_filter linereader proc_loc_opt procname err_log
  in
  List.iter ~f:pp_issues_in_format bug_format_list


let collect_issues summary issues_acc =
  let err_log = Summary.get_err_log summary in
  let proc_name = Summary.get_proc_name summary in
  let proc_location = Summary.get_loc summary in
  Errlog.fold
    (fun err_key err_data acc -> {Issue.proc_name; proc_location; err_key; err_data} :: acc)
    err_log issues_acc


let pp_summary summary =
  L.result "Procedure: %a@\n%a@." Procname.pp (Summary.get_proc_name summary) Summary.pp_text
    summary


let pp_costs_in_format (format_kind, (outfile_opt : Utils.outfile option)) =
  match format_kind with
  | Json ->
      let outf = get_outfile outfile_opt in
      JsonCostsPrinter.pp outf.fmt
  | Tests ->
      L.die InternalError "Printing costs in tests is not implemented"


let pp_costs summary costs_format_list =
  let pp format =
    pp_costs_in_format format
      { loc= Summary.get_loc summary
      ; proc_name= Summary.get_proc_name summary
      ; cost_opt= summary.Summary.payloads.Payloads.cost }
  in
  List.iter ~f:pp costs_format_list


let pp_summary_by_report_kind formats_by_report_kind summary issues_acc =
  let pp_summary_by_report_kind (report_kind, format_list) =
    match (report_kind, format_list) with
    | Costs, _ ->
        pp_costs summary format_list
    | Summary, _ when InferCommand.equal Config.command Report && not Config.quiet ->
        pp_summary summary
    | _ ->
        ()
  in
  List.iter ~f:pp_summary_by_report_kind formats_by_report_kind ;
  collect_issues summary issues_acc


let pp_json_report_by_report_kind formats_by_report_kind fname =
  match Utils.read_file fname with
  | Ok report_lines ->
      let pp_json_issues format_list report =
        let pp_json_issue (format_kind, (outfile_opt : Utils.outfile option)) =
          match format_kind with
          | Tests ->
              let outf = get_outfile outfile_opt in
              pp_custom_of_report outf.fmt report Config.issues_tests_fields
          | Json ->
              L.die InternalError "Printing issues from json does not support json output"
        in
        List.iter ~f:pp_json_issue format_list
      in
      let sorted_report =
        let report = Jsonbug_j.report_of_string (String.concat ~sep:"\n" report_lines) in
        List.sort ~compare:tests_jsonbug_compare report
      in
      let pp_report_by_report_kind (report_kind, format_list) =
        match (report_kind, format_list) with
        | Issues, _ :: _ ->
            pp_json_issues format_list sorted_report
        | _ ->
            ()
      in
      List.iter ~f:pp_report_by_report_kind formats_by_report_kind
  | Error error ->
      L.(die UserError) "Error reading '%s': %s" fname error


let pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log
    =
  let pp_summary_by_report_kind (report_kind, format_list) =
    match (report_kind, format_list) with
    | Issues, _ :: _ ->
        pp_issues_of_error_log error_filter linereader None procname error_log format_list
    | _ ->
        ()
  in
  List.iter ~f:pp_summary_by_report_kind formats_by_report_kind


(** Process lint issues of a procedure *)
let pp_lint_issues filters formats_by_report_kind linereader procname error_log =
  let error_filter = error_filter filters procname in
  pp_lint_issues_by_report_kind formats_by_report_kind error_filter linereader procname error_log


(** Process a summary *)
let process_summary formats_by_report_kind summary issues_acc =
  pp_summary_by_report_kind formats_by_report_kind summary issues_acc


(** Although the out_file is an Option type, the None option is strictly meant for the logs
    format_kind, and all other formats should contain an outfile value. *)
let mk_format format_kind fname =
  Option.value_map
    ~f:(fun out_file -> [(format_kind, Some out_file)])
    ~default:[] (Utils.create_outfile fname)


let init_issues_format_list report_json =
  let json_format = Option.value_map ~f:(mk_format Json) ~default:[] report_json in
  let tests_format = Option.value_map ~f:(mk_format Tests) ~default:[] Config.issues_tests in
  json_format @ tests_format


let init_files format_list_by_kind =
  let init_files_of_report_kind (report_kind, format_list) =
    let init_files_of_format (format_kind, (outfile_opt : Utils.outfile option)) =
      match (format_kind, report_kind) with
      | Json, Costs ->
          let outfile = get_outfile outfile_opt in
          JsonCostsPrinter.pp_open outfile.fmt ()
      | Json, Issues ->
          let outfile = get_outfile outfile_opt in
          IssuesJson.pp_open outfile.fmt ()
      | Json, Summary | Tests, _ ->
          ()
    in
    List.iter ~f:init_files_of_format format_list
  in
  List.iter ~f:init_files_of_report_kind format_list_by_kind


let finalize_and_close_files format_list_by_kind =
  let close_files_of_report_kind (report_kind, format_list) =
    let close_files_of_format (format_kind, (outfile_opt : Utils.outfile option)) =
      ( match (format_kind, report_kind) with
      | Json, Costs ->
          let outfile = get_outfile outfile_opt in
          JsonCostsPrinter.pp_close outfile.fmt ()
      | Json, Issues ->
          let outfile = get_outfile outfile_opt in
          IssuesJson.pp_close outfile.fmt ()
      | Json, Summary | Tests, _ ->
          () ) ;
      match outfile_opt with Some outfile -> Utils.close_outf outfile | None -> ()
    in
    List.iter ~f:close_files_of_format format_list ;
    ()
  in
  List.iter ~f:close_files_of_report_kind format_list_by_kind


let pp_summary_and_issues formats_by_report_kind issue_formats =
  let linereader = Printer.LineReader.create () in
  let filters = Inferconfig.create_filters () in
  let all_issues = ref [] in
  SpecsFiles.iter_from_config ~f:(fun summary ->
      all_issues := process_summary formats_by_report_kind summary !all_issues ) ;
  all_issues := Issue.sort_filter_issues !all_issues ;
  if Config.is_checker_enabled QuandaryBO then all_issues := QuandaryBO.update_issues !all_issues ;
  List.iter
    ~f:(fun ({Issue.proc_name} as issue) ->
      let error_filter = error_filter filters proc_name in
      List.iter
        ~f:(fun issue_format -> pp_issue_in_format issue_format error_filter issue)
        issue_formats )
    !all_issues ;
  (* Issues that are generated and stored outside of summaries by linter and checkers *)
  List.iter (Config.lint_issues_dir_name :: FileLevelAnalysisIssueDirs.get_registered_dir_names ())
    ~f:(fun dir_name ->
      IssueLog.load dir_name
      |> IssueLog.iter ~f:(pp_lint_issues filters formats_by_report_kind linereader) ) ;
  finalize_and_close_files formats_by_report_kind


let main ~report_json =
  let issue_formats = init_issues_format_list report_json in
  let formats_by_report_kind =
    let costs_report_format_kind =
      match report_json with
      | Some _ ->
          let file = Config.(results_dir ^/ Config.costs_report_json) in
          [(Costs, mk_format Json file)]
      | None ->
          []
    in
    costs_report_format_kind @ [(Issues, issue_formats); (Summary, [])]
  in
  init_files formats_by_report_kind ;
  ( match Config.issues_tests with
  | Some _ ->
      pp_json_report_by_report_kind formats_by_report_kind Config.from_json_report
  | None ->
      pp_summary_and_issues formats_by_report_kind issue_formats ) ;
  if Config.test_determinator && Config.process_clang_ast then
    TestDeterminator.merge_test_determinator_results () ;
  ()
