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

let rules_list = ref []

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


let compute_hash =
  let num_regexp = Re.Str.regexp "\\(:\\)[0-9]+" in
  let qualifier_regexp = Re.Str.regexp "\\(line \\|column \\|:\\|parameter \\|\\$\\)[0-9]+" in
  fun ~(severity : string) ~(bug_type : string) ~(proc_name : Procname.t) ~(file : string)
      ~(qualifier : string) ->
    let base_filename = Filename.basename file in
    let hashable_procedure_name = Procname.hashable_name proc_name in
    let location_independent_proc_name =
      Re.Str.global_replace num_regexp "$_" hashable_procedure_name
    in
    let location_independent_qualifier =
      (* Removing the line,column, line and column in lambda's name
         (e.g. test::lambda.cpp:10:15::operator()),
         and infer temporary variable (e.g., n$67) information from the
         error message as well as the index of the annonymmous class to make the hash invariant
         when moving the source code in the file *)
      Re.Str.global_replace qualifier_regexp "$_" qualifier
    in
    Utils.better_hash
      ( severity
      , bug_type
      , location_independent_proc_name
      , base_filename
      , location_independent_qualifier )
    |> Caml.Digest.to_hex


let loc_trace_to_jsonbug_record trace_list ekind =
  match ekind with
  | IssueType.Info ->
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


let should_report issue_type error_desc =
  if (not Config.filtering) || Language.curr_language_is CIL then true
  else
    let issue_type_is_null_deref =
      let null_deref_issue_types =
        let open IssueType in
        [ field_not_null_checked
        ; null_dereference
        ; parameter_not_null_checked
        ; premature_nil_termination
        ; empty_vector_access ]
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

module type PrinterSarif = sig
  type elt

  val pp_open : F.formatter -> unit -> unit

  val pp_results : F.formatter -> unit -> unit

  val pp_close : F.formatter -> unit -> unit

  val pp_rules : F.formatter -> elt -> unit

  val pp : F.formatter -> elt -> unit
end

module MakeSarifListPrinter (P : sig
  type elt

  val rules_to_string : elt -> string option

  val to_string : elt -> string option
end) : PrinterSarif with type elt = P.elt = struct
  include P

  let is_first_item = ref true
  
  let pp_n_spaces n fmt =
    for _ = 1 to n do
      F.pp_print_char fmt ' '
    done

  let pp_open fmt () =
    is_first_item := true ;
    F.fprintf fmt "{@\n" ;
    F.fprintf fmt "%t%s: %s,@\n" (pp_n_spaces 2) "\"$schema\"" "\"http://json.schemastore.org/sarif-2.1.0\"" ;
    F.fprintf fmt "%t%s: %s,@\n" (pp_n_spaces 2) "\"version\"" "\"2.1.0\"" ;
    F.fprintf fmt "%t%s: [@\n"  (pp_n_spaces 2) "\"runs\"" ;
    F.fprintf fmt "%t{@\n"  (pp_n_spaces 4) ;
    F.fprintf fmt "%t%s: {@\n"  (pp_n_spaces 6) "\"tool\"" ;
    F.fprintf fmt "%t%s: {@\n"  (pp_n_spaces 8) "\"driver\"" ;
    F.fprintf fmt "%t%s: %s,@\n"  (pp_n_spaces 10) "\"name\"" "\"Infer\"" ;
    F.fprintf fmt "%t%s: %s,@\n"  (pp_n_spaces 10) "\"informationUri\"" "\"https://github.com/facebook/infer\"" ;
    F.fprintf fmt "%t%s: \"%d.%d.%d\",@\n"  (pp_n_spaces 10) "\"version\"" Version.major Version.minor Version.patch ;
    F.fprintf fmt "%t%s: [@?"  (pp_n_spaces 10) "\"rules\""

  let pp_results fmt () =
    is_first_item := true ;
    F.fprintf fmt "]@\n" ;
    F.fprintf fmt "%t%s@\n"  (pp_n_spaces 8) "}" ;
    F.fprintf fmt "%t%s,@\n"  (pp_n_spaces 6) "}" ;
    F.fprintf fmt "%t%s: [@?"  (pp_n_spaces 6) "\"results\""

  let pp_close fmt () = 
    F.fprintf fmt "]@\n" ;
    F.fprintf fmt "%t%s@\n" (pp_n_spaces 4) "}" ;
    F.fprintf fmt "%t%s@\n" (pp_n_spaces 2) "]" ;
    F.fprintf fmt "}@\n@?"
  
  let pp_rules fmt elt =
    match rules_to_string elt with
    | Some s ->
        if !is_first_item then is_first_item := false else F.pp_print_char fmt ',' ;
        F.fprintf fmt "%s@?" s
    | None ->
        ()

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
        "Invalid source file for %a %a@.Trace: %a@." IssueType.pp err_key.issue_type
        Localise.pp_error_desc err_key.err_desc Errlog.pp_loc_trace err_data.loc_trace ;
    let should_report_proc_name =
      Config.debug_mode || Config.debug_exceptions || not (BiabductionModels.mem proc_name)
    in
    if
      error_filter source_file err_key.issue_type
      && should_report_proc_name
      && should_report err_key.issue_type err_key.err_desc
    then
      let severity = IssueType.string_of_severity err_key.severity in
      let bug_type = err_key.issue_type.unique_id in
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
        if IssueType.(equal resource_leak) err_key.issue_type then
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
        ; bug_type_hum= err_key.issue_type.hum
        ; linters_def_file= err_data.linters_def_file
        ; doc_url= err_data.doc_url
        ; traceview_id= None
        ; censored_reason= censored_reason err_key.issue_type source_file
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

module SarifIssuePrinter = MakeSarifListPrinter (struct
  type elt = json_issue_printer_typ
  
  let rules_to_string ({error_filter; proc_name; proc_loc_opt; err_key; err_data} : elt) =
    let source_file =
      match proc_loc_opt with
      | Some proc_loc ->
          proc_loc.Location.file
      | None ->
          err_data.loc.Location.file
    in
    if SourceFile.is_invalid source_file then
      L.(die InternalError)
        "Invalid source file for %a %a@.Trace: %a@." IssueType.pp err_key.issue_type
        Localise.pp_error_desc err_key.err_desc Errlog.pp_loc_trace err_data.loc_trace ;
    let should_report_proc_name =
      Config.debug_mode || Config.debug_exceptions || not (BiabductionModels.mem proc_name)
    in
    let ruleId = err_key.issue_type.unique_id in
    let should_report_rule_id = not (List.mem ~equal:String.equal !rules_list ruleId) in
    if
      error_filter source_file err_key.issue_type
      && should_report_rule_id
      && should_report_proc_name
      && should_report err_key.issue_type err_key.err_desc
    then
      let shortDescription = {Sarifbug_j.text= err_key.issue_type.hum} in
      let rule =
        { Sarifbug_j.id= ruleId
        ; shortDescription }
      in
      rules_list := List.append !rules_list [ruleId] ;
      Some (Sarifbug_j.string_of_rule rule)
    else None

  let to_string ({error_filter; proc_name; proc_loc_opt; err_key; err_data} : elt) =
    let source_file =
      match proc_loc_opt with
      | Some proc_loc ->
          proc_loc.Location.file
      | None ->
          err_data.loc.Location.file
    in
    if SourceFile.is_invalid source_file then
      L.(die InternalError)
        "Invalid source file for %a %a@.Trace: %a@." IssueType.pp err_key.issue_type
        Localise.pp_error_desc err_key.err_desc Errlog.pp_loc_trace err_data.loc_trace ;
    let should_report_proc_name =
      Config.debug_mode || Config.debug_exceptions || not (BiabductionModels.mem proc_name)
    in
    if
      error_filter source_file err_key.issue_type
      && should_report_proc_name
      && should_report err_key.issue_type err_key.err_desc
    then
      let message_content =
        let base_qualifier = error_desc_to_plain_string err_key.err_desc in
        if IssueType.(equal resource_leak) err_key.issue_type then
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
      let message = 
        {Sarifbug_j.text= message_content}
      in
      let level = String.lowercase (IssueType.string_of_severity err_key.severity) in
      let ruleId = err_key.issue_type.unique_id in
      let file =
        SourceFile.to_string ~force_relative:Config.report_force_relative_path source_file
      in
      let file_loc = {Sarifbug_j.uri= file} in
      let column = err_data.loc.Location.col in
      let region = 
        match column with
        | -1 ->
          { Sarifbug_j.startLine= err_data.loc.Location.line
          ; startColumn= 1 }
        | _ ->
          { Sarifbug_j.startLine= err_data.loc.Location.line
          ; startColumn= column }
      in
      let physical_location = 
        {Sarifbug_j.artifactLocation= file_loc
        ; region } 
      in
      let file_location_to_record = [{ Sarifbug_j.physicalLocation=physical_location}] in
      let result =
        { Sarifbug_j.message
        ; level
        ; ruleId 
        ; locations= file_location_to_record }
      in
      Some (Sarifbug_j.string_of_sarifbug result)
    else None
end)

module IssuesSarif = struct
  include SarifIssuePrinter

  (** Write bug report in SARIF 2.1 format *)
  let pp_issues_of_error_log fmt error_filter _ proc_loc_opt proc_name err_log =
    Errlog.iter
      (fun err_key err_data -> pp fmt {error_filter; proc_name; proc_loc_opt; err_key; err_data})
      err_log
  
  (** Write bug rules in SARIF 2.1 format *)
  let pp_rules_of_error_log fmt error_filter _ proc_loc_opt proc_name err_log =
    Errlog.iter
      (fun err_key err_data -> pp_rules fmt {error_filter; proc_name; proc_loc_opt; err_key; err_data})
      err_log
end

module NoQualifierHashProcInfo = struct
  type t = {hash: string; loc: Jsonbug_t.loc; procedure_name: string; procedure_id: string}

  let get loc proc_name =
    let file =
      SourceFile.to_string ~force_relative:Config.report_force_relative_path loc.Location.file
    in
    let hash = compute_hash ~severity:"" ~bug_type:"" ~proc_name ~file ~qualifier:"" in
    let loc = {Jsonbug_t.file; lnum= loc.Location.line; cnum= loc.Location.col; enum= -1} in
    let procedure_name = Procname.get_method proc_name in
    let procedure_id = procedure_id_of_procname proc_name in
    {hash; loc; procedure_name; procedure_id}
end

module JsonCostsPrinterElt = struct
  type elt = {loc: Location.t; proc_name: Procname.t; cost_opt: CostDomain.summary option}

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
        let cost_info ?is_autoreleasepool_trace cost =
          { Jsonbug_t.polynomial_version= CostDomain.BasicCost.version
          ; polynomial= CostDomain.BasicCost.encode cost
          ; degree=
              Option.map (CostDomain.BasicCost.degree cost) ~f:Polynomials.Degree.encode_to_int
          ; hum= hum cost
          ; trace=
              loc_trace_to_jsonbug_record
                (CostDomain.BasicCost.polynomial_traces ?is_autoreleasepool_trace cost)
                Advice }
        in
        let cost_item =
          let {NoQualifierHashProcInfo.hash; loc; procedure_name; procedure_id} =
            NoQualifierHashProcInfo.get loc proc_name
          in
          { Jsonbug_t.hash
          ; loc
          ; procedure_name
          ; procedure_id
          ; is_on_ui_thread
          ; exec_cost= cost_info (CostDomain.get_cost_kind CostKind.OperationCost post).cost
          ; autoreleasepool_size=
              cost_info ~is_autoreleasepool_trace:true
                (CostDomain.get_cost_kind CostKind.AutoreleasepoolSize post).cost }
        in
        Some (Jsonbug_j.string_of_cost_item cost_item)
    | _ ->
        None
end

module JsonCostsPrinter = MakeJsonListPrinter (JsonCostsPrinterElt)

module JsonConfigImpactPrinterElt = struct
  type elt =
    { loc: Location.t
    ; proc_name: Procname.t
    ; config_impact_opt: ConfigImpactAnalysis.Summary.t option
    ; is_strict: bool }

  let to_string {loc; proc_name; config_impact_opt; is_strict} =
    Option.map config_impact_opt ~f:(fun config_impact ->
        let {NoQualifierHashProcInfo.hash; loc; procedure_name; procedure_id} =
          NoQualifierHashProcInfo.get loc proc_name
        in
        let unchecked_callees =
          ConfigImpactAnalysis.Summary.get_unchecked_callees config_impact
          |> ConfigImpactAnalysis.UncheckedCallees.encode
        in
        Jsonbug_j.string_of_config_impact_item
          {Jsonbug_t.hash; loc; procedure_name; procedure_id; unchecked_callees; is_strict} )
end

module JsonConfigImpactPrinter = MakeJsonListPrinter (JsonConfigImpactPrinterElt)

let is_in_changed_files {Location.file} =
  match SourceFile.read_config_changed_files () with
  | None ->
      (* when Config.changed_files_index is not given *)
      true
  | Some changed_files ->
      SourceFile.Set.mem file changed_files


let mk_error_filter filters proc_name file error_name =
  (Config.write_html || not (IssueType.(equal skip_function) error_name))
  && filters.Inferconfig.path_filter file
  && filters.Inferconfig.error_filter error_name
  && filters.Inferconfig.proc_filter proc_name


let collect_issues proc_name proc_location err_log issues_acc =
  Errlog.fold
    (fun err_key err_data acc -> {Issue.proc_name; proc_location; err_key; err_data} :: acc)
    err_log issues_acc


let write_costs proc_name loc cost_opt (outfile : Utils.outfile) =
  if (not (Cost.is_report_suppressed proc_name)) && is_in_changed_files loc then
    JsonCostsPrinter.pp outfile.fmt {loc; proc_name; cost_opt}


let get_all_config_fields () =
  lazy
    (let all_config_fields = ref ConfigImpactAnalysis.Fields.empty in
     Summary.OnDisk.iter_specs ~f:(fun summary ->
         Payloads.config_impact_analysis summary.payloads
         |> Option.iter ~f:(fun summary ->
                all_config_fields :=
                  ConfigImpactAnalysis.Fields.union !all_config_fields
                    (ConfigImpactAnalysis.Summary.get_config_fields summary) ) ) ;
     !all_config_fields )


let write_config_impact all_config_fields proc_name loc config_impact_opt (outfile : Utils.outfile)
    =
  if
    ( ExternalConfigImpactData.is_in_config_data_file proc_name
    || (Config.config_impact_strict_mode && List.is_empty Config.config_impact_strict_mode_paths)
    || ConfigImpactAnalysis.is_in_strict_mode_paths loc.Location.file )
    && is_in_changed_files loc
  then
    let config_impact_opt =
      Option.map config_impact_opt
        ~f:
          (ConfigImpactAnalysis.Summary.instantiate_unchecked_callees_cond
             ~all_config_fields:(Lazy.force all_config_fields) )
    in
    JsonConfigImpactPrinter.pp outfile.fmt
      {loc; proc_name; config_impact_opt; is_strict= ConfigImpactAnalysis.strict_mode}


(** Process lint issues of a procedure *)
let write_lint_issues filters (issues_outf : Utils.outfile) linereader procname error_log =
  let error_filter = mk_error_filter filters procname in
  IssuesJson.pp_issues_of_error_log issues_outf.fmt error_filter linereader None procname error_log

(** Process lint issues of a procedure *)
let write_sarif_lint_issues filters (issues_outf : Utils.outfile) linereader procname error_log =
  let error_filter = mk_error_filter filters procname in
  IssuesSarif.pp_issues_of_error_log issues_outf.fmt error_filter linereader None procname error_log

let write_sarif_lint_rules filters (issues_outf : Utils.outfile) linereader procname error_log =
  let error_filter = mk_error_filter filters procname in
  IssuesSarif.pp_rules_of_error_log issues_outf.fmt error_filter linereader None procname error_log

let process_summary proc_name loc ~cost:(cost_opt, costs_outf)
    ~config_impact:(config_impact_opt, config_impact_outf, all_config_fields) err_log issues_acc =
  write_costs proc_name loc cost_opt costs_outf ;
  write_config_impact all_config_fields proc_name loc config_impact_opt config_impact_outf ;
  collect_issues proc_name loc err_log issues_acc

let process_issue_summary proc_name loc err_log issues_acc =
  collect_issues proc_name loc err_log issues_acc


let process_all_summaries_and_issues ~issues_outf ~costs_outf ~config_impact_outf =
  let linereader = LineReader.create () in
  let filters = Inferconfig.create_filters () in
  let all_issues = ref [] in
  let all_config_fields = get_all_config_fields () in
  Summary.OnDisk.iter_report_summaries_from_config
    ~f:(fun proc_name loc cost_opt config_impact_opt err_log ->
      all_issues :=
        process_summary proc_name loc ~cost:(cost_opt, costs_outf)
          ~config_impact:(config_impact_opt, config_impact_outf, all_config_fields)
          err_log !all_issues ) ;
  all_issues := Issue.sort_filter_issues !all_issues ;
  List.iter
    ~f:(fun {Issue.proc_name; proc_location; err_key; err_data} ->
      let error_filter = mk_error_filter filters proc_name in
      IssuesJson.pp issues_outf.Utils.fmt
        {error_filter; proc_name; proc_loc_opt= Some proc_location; err_key; err_data} )
    !all_issues ;
  (* Issues that are generated and stored outside of summaries by linter and checkers *)
  List.iter (ResultsDirEntryName.get_issues_directories ()) ~f:(fun dir_name ->
      IssueLog.load dir_name |> IssueLog.iter ~f:(write_lint_issues filters issues_outf linereader) ) ;
  ()

let process_issues_for_sarif ~issues_outf =
  let linereader = LineReader.create () in
  let filters = Inferconfig.create_filters () in
  let all_issues = ref [] in
  Summary.OnDisk.iter_report_summaries_from_config
    ~f:(fun proc_name loc _ _ err_log ->
      all_issues :=
        process_issue_summary proc_name loc err_log !all_issues ) ;
  all_issues := Issue.sort_filter_issues !all_issues ;
  List.iter
    ~f:(fun {Issue.proc_name; proc_location; err_key; err_data} ->
      let error_filter = mk_error_filter filters proc_name in
      IssuesSarif.pp_rules issues_outf.Utils.fmt
        {error_filter; proc_name; proc_loc_opt= Some proc_location; err_key; err_data} )
    !all_issues ;
  (* Rules that are generated and stored outside of summaries by linter and checkers *)
  List.iter (ResultsDirEntryName.get_issues_directories ()) ~f:(fun dir_name ->
      IssueLog.load dir_name |> IssueLog.iter ~f:(write_sarif_lint_rules filters issues_outf linereader) ) ;
  IssuesSarif.pp_results issues_outf.Utils.fmt () ;
  List.iter
    ~f:(fun {Issue.proc_name; proc_location; err_key; err_data} ->
      let error_filter = mk_error_filter filters proc_name in
      IssuesSarif.pp issues_outf.Utils.fmt
        {error_filter; proc_name; proc_loc_opt= Some proc_location; err_key; err_data} )
    !all_issues ;
  (* Issues that are generated and stored outside of summaries by linter and checkers *)
  List.iter (ResultsDirEntryName.get_issues_directories ()) ~f:(fun dir_name ->
      IssueLog.load dir_name |> IssueLog.iter ~f:(write_sarif_lint_issues filters issues_outf linereader) ) ;
  ()

let write_reports ~issues_json ~costs_json ~config_impact_json =
  let mk_outfile fname =
    match Utils.create_outfile fname with
    | None ->
        L.die InternalError "Could not create '%s'." fname
    | Some outf ->
        outf
  in
  let open_outfile_and_fmt json =
    let outf = mk_outfile json in
    IssuesJson.pp_open outf.fmt () ;
    outf
  in
  let close_fmt_and_outfile outf =
    IssuesJson.pp_close outf.Utils.fmt () ;
    Utils.close_outf outf
  in
  let issues_outf = open_outfile_and_fmt issues_json in
  let costs_outf = open_outfile_and_fmt costs_json in
  let config_impact_outf = open_outfile_and_fmt config_impact_json in
  process_all_summaries_and_issues ~issues_outf ~costs_outf ~config_impact_outf ;
  close_fmt_and_outfile config_impact_outf ;
  close_fmt_and_outfile costs_outf ;
  close_fmt_and_outfile issues_outf


let write_sarif_reports ~issues_sarif =
  let mk_outfile fname =
    match Utils.create_outfile fname with
    | None ->
        L.die InternalError "Could not create '%s'." fname
    | Some outf ->
        outf
  in
  let open_outfile_and_fmt sarif =
    let outf = mk_outfile sarif in
    IssuesSarif.pp_open outf.fmt () ;
    outf
  in
  let close_fmt_and_outfile outf =
    IssuesSarif.pp_close outf.Utils.fmt () ;
    Utils.close_outf outf
  in
  let issues_outf = open_outfile_and_fmt issues_sarif in
  process_issues_for_sarif ~issues_outf ;
  close_fmt_and_outfile issues_outf