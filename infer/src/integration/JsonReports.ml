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

let error_desc_to_qualifier_string error_desc =
  let pp fmt = Localise.pp_error_qualifier fmt error_desc in
  let s = F.asprintf "%t" pp in
  let s = String.strip s in
  (* end error description with a dot *)
  if String.is_suffix ~suffix:"." s then s else s ^ "."


let error_desc_to_suggestion_string error_desc =
  Option.map error_desc.Localise.suggestion ~f:(fun suggestion ->
      let s = F.asprintf "%s" suggestion in
      (* end error suggestion with a dot *)
      if String.is_suffix ~suffix:"." s then s else s ^ "." )


let error_desc_to_dotty_string error_desc = Localise.error_desc_get_dotty error_desc

let compute_key (bug_type : string) (proc_name : Procname.t) (filename : string) =
  let base_filename = Filename.basename filename
  and simple_procedure_name = Procname.get_method proc_name in
  String.concat ~sep:"|" [base_filename; simple_procedure_name; bug_type]


let sanitize_qualifier qualifier =
  (* Removing the line,column, line and column in lambda's name
     (e.g. test::lambda.cpp:10:15::operator()),
     and infer temporary variable (e.g., n$67) information from the
     error message as well as the index of the annonymmous class to make the hash invariant
     when moving the source code in the file *)
  let qualifier_regexp =
    Re.(
      seq
        [ alt (["line"; "column"; "parameter"; "argument"; "$"; ":"] |> List.map ~f:str)
        ; str " " |> rep
        ; opt @@ char '`'
        ; opt @@ char '#'
        ; rep1 digit
        ; opt @@ char '`' ]
      |> no_case |> compile )
  in
  Re.replace_string qualifier_regexp ~by:"$_" qualifier


let compute_hash =
  let num_regexp = Str.regexp ":[0-9]+\\(_[0-9a-f]+\\)?" in
  let hack_closure_num_regexp = Str.regexp "[0-9]*\\.__invoke$" in
  fun ~(severity : string) ~(bug_type : string) ~(proc_name : Procname.t) ~(file : string)
      ~(qualifier : string) ->
    let base_filename = Filename.basename file in
    let hashable_procedure_name = Procname.hashable_name proc_name in
    let location_independent_proc_name =
      Str.global_replace num_regexp "$_" hashable_procedure_name
      |> Str.replace_first hack_closure_num_regexp ".__invoke"
    in
    let location_independent_qualifier = sanitize_qualifier qualifier in
    Utils.better_hash
      ( severity
      , bug_type
      , location_independent_proc_name
      , base_filename
      , location_independent_qualifier )
    |> Caml.Digest.to_hex


let loc_trace_to_jsonbug_record trace_list =
  let trace_item_to_record trace_item =
    let loc : Location.t = trace_item.Errlog.lt_loc in
    let trace =
      { Jsonbug_j.level= trace_item.Errlog.lt_level
      ; filename= SourceFile.to_string ~force_relative:Config.report_force_relative_path loc.file
      ; line_number= loc.line
      ; column_number= loc.col
      ; description= trace_item.Errlog.lt_description }
    in
    Location.get_macro_file_line_opt trace_item.Errlog.lt_loc
    |> Option.value_map ~default:[trace] ~f:(fun (macro_source, macro_line) ->
           let trace = {trace with Jsonbug_j.description= "macro expanded here"} in
           let macro_trace =
             { Jsonbug_j.level= trace_item.Errlog.lt_level
             ; filename=
                 SourceFile.to_string ~force_relative:Config.report_force_relative_path macro_source
             ; line_number= macro_line
             ; column_number= -1
             ; description= trace_item.Errlog.lt_description }
           in
           [trace; macro_trace] )
  in
  let record_list = List.concat_map ~f:trace_item_to_record trace_list in
  record_list


let should_report proc_name issue_type error_desc =
  if (not Config.filtering) || Language.equal (Procname.get_language proc_name) CIL then true
  else
    let issue_type_is_null_deref =
      let null_deref_issue_types =
        let open IssueType in
        [null_dereference; premature_nil_termination; empty_vector_access]
      in
      List.mem ~equal:IssueType.equal null_deref_issue_types issue_type
    in
    if issue_type_is_null_deref then Localise.error_desc_is_reportable_bucket error_desc else true


let should_not_censor (issue_type : IssueType.t) =
  List.exists Config.no_censor_report ~f:(fun issue_type_re ->
      Str.string_match issue_type_re issue_type.unique_id 0 )


(* The reason an issue should be censored (that is, not reported). The empty
   string (that is "no reason") means that the issue should be reported. *)
let censored_reason (issue_type : IssueType.t) source_file =
  if should_not_censor issue_type then None
  else
    let filename = SourceFile.to_rel_path source_file in
    let rejected_by ((issue_type_polarity, issue_type_re), (filename_polarity, filename_re), reason)
        =
      let accepted =
        (* matches issue_type_re implies matches filename_re *)
        (not
           (Bool.equal issue_type_polarity (Str.string_match issue_type_re issue_type.unique_id 0)) )
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
  ; proc_location_opt: Location.t option
  ; err_key: Errlog.err_key
  ; err_data: Errlog.err_data }

let procedure_id_of_procname proc_name =
  match Procname.get_language proc_name with
  | Language.Java ->
      Procname.to_unique_id proc_name
  | _ ->
      Procname.to_string proc_name


let is_in_clang_header source_file =
  String.is_substring
    (SourceFile.to_string source_file)
    ~substring:"/facebook-clang-plugins/clang/install/include/"


let issue_in_report_block_list_specs ~file ~issue ~proc =
  let is_in_report_block_list_spec ~file ~issue ~proc report_block_list_spec =
    let filter_class =
      match
        (Procname.get_class_name proc, report_block_list_spec.Report_block_list_spec_t.class_name)
      with
      | Some class_name, Some fp_class_name ->
          String.is_substring ~substring:fp_class_name class_name
      | _ ->
          true
    in
    let filter_proc =
      let proc_name = Procname.get_method proc in
      String.is_substring ~substring:report_block_list_spec.Report_block_list_spec_t.procedure_name
        proc_name
    in
    let filter_file =
      String.is_substring ~substring:report_block_list_spec.Report_block_list_spec_t.file
        (SourceFile.to_rel_path file)
    in
    let filter_error =
      String.equal issue.IssueType.unique_id
        report_block_list_spec.Report_block_list_spec_t.bug_type
    in
    filter_class && filter_proc && filter_file && filter_error
  in
  List.exists ~f:(is_in_report_block_list_spec ~file ~issue ~proc) Config.report_block_list_spec


module JsonIssuePrinter = MakeJsonListPrinter (struct
  type elt = json_issue_printer_typ

  let to_string ({error_filter; proc_name; proc_location_opt; err_key; err_data} : elt) =
    let source_file, procedure_start_line =
      match proc_location_opt with
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
      && should_report proc_name err_key.issue_type err_key.err_desc
      && (not (is_in_clang_header source_file))
      && should_report proc_name err_key.issue_type err_key.err_desc
      && not
           (issue_in_report_block_list_specs ~file:source_file ~issue:err_key.issue_type
              ~proc:proc_name )
    then
      let severity = IssueType.string_of_severity err_key.severity in
      let category =
        Option.value
          (Errlog.category_override err_data)
          ~default:(IssueType.string_of_category err_key.issue_type.category)
      in
      let bug_type =
        Option.value (Errlog.issue_type_override err_data) ~default:err_key.issue_type.unique_id
      in
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
        let base_qualifier = error_desc_to_qualifier_string err_key.err_desc in
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
      let suggestion = error_desc_to_suggestion_string err_key.err_desc in
      let bug =
        { Jsonbug_j.bug_type
        ; qualifier
        ; severity
        ; category
        ; suggestion
        ; line= err_data.loc.Location.line
        ; column= err_data.loc.Location.col
        ; procedure= procedure_id_of_procname proc_name
        ; procedure_start_line
        ; file
        ; bug_trace= loc_trace_to_jsonbug_record err_data.loc_trace
        ; bug_trace_length= Errlog.loc_trace_length err_data.loc_trace
        ; bug_trace_max_depth= Errlog.loc_trace_max_depth err_data.loc_trace
        ; node_key= Option.map ~f:Procdesc.NodeKey.to_string err_data.node_key
        ; key= compute_key bug_type proc_name file
        ; hash= compute_hash ~severity ~bug_type ~proc_name ~file ~qualifier
        ; dotty= error_desc_to_dotty_string err_key.err_desc
        ; infer_source_loc= json_ml_loc
        ; bug_type_hum= err_key.issue_type.hum
        ; traceview_id= None
        ; censored_reason= censored_reason err_key.issue_type source_file
        ; access= err_data.access
        ; extras= err_data.extras }
      in
      Some (Jsonbug_j.string_of_jsonbug bug)
    else None
end)

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
    | Some {post; is_on_ui_thread}
      when (not (Procname.is_java_access_method proc_name)) && not (is_in_clang_header loc.file) ->
        let hum cost =
          let degree_with_term = CostDomain.BasicCost.get_degree_with_term cost in
          { Jsoncost_t.hum_polynomial= Format.asprintf "%a" CostDomain.BasicCost.pp_hum cost
          ; hum_degree=
              Format.asprintf "%a"
                (CostDomain.BasicCost.pp_degree ~only_bigO:false)
                degree_with_term
          ; big_o=
              Format.asprintf "%a" (CostDomain.BasicCost.pp_degree ~only_bigO:true) degree_with_term
          }
        in
        let cost_info cost =
          { Jsoncost_t.polynomial_version= CostDomain.BasicCost.version
          ; polynomial= CostDomain.BasicCost.encode cost
          ; degree=
              Option.map (CostDomain.BasicCost.degree cost) ~f:Polynomials.Degree.encode_to_int
          ; hum= hum cost
          ; trace= loc_trace_to_jsonbug_record (CostDomain.BasicCost.polynomial_traces cost) }
        in
        let cost_item =
          let {NoQualifierHashProcInfo.hash; loc; procedure_name; procedure_id} =
            NoQualifierHashProcInfo.get loc proc_name
          in
          { Jsoncost_t.hash
          ; loc
          ; procedure_name
          ; procedure_id
          ; is_on_ui_thread
          ; exec_cost= cost_info (CostDomain.get_cost_kind CostKind.OperationCost post).cost }
        in
        Some (Jsoncost_j.string_of_item cost_item)
    | _ ->
        None
end

module JsonCostsPrinter = MakeJsonListPrinter (JsonCostsPrinterElt)

module JsonConfigImpactPrinterElt = struct
  type elt =
    { loc: Location.t
    ; proc_name: Procname.t
    ; config_impact_opt: ConfigImpactAnalysis.Summary.t option
    ; mode: Jsonconfigimpact_t.config_impact_mode }

  let to_string {loc; proc_name; config_impact_opt; mode} =
    if is_in_clang_header loc.file then None
    else
      Option.map config_impact_opt ~f:(fun config_impact ->
          let {NoQualifierHashProcInfo.hash; loc; procedure_name; procedure_id} =
            NoQualifierHashProcInfo.get loc proc_name
          in
          let unchecked_callees =
            ConfigImpactAnalysis.Summary.get_unchecked_callees config_impact
            |> ConfigImpactAnalysis.UncheckedCallees.encode
          in
          Jsonconfigimpact_j.string_of_item
            {Jsonconfigimpact_t.hash; loc; procedure_name; procedure_id; unchecked_callees; mode} )
end

module JsonConfigImpactPrinter = MakeJsonListPrinter (JsonConfigImpactPrinterElt)

let is_in_files_to_analyze {Location.file} =
  match SourceFile.read_config_files_to_analyze () with
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


let collect_issues proc_name proc_location_opt err_log issues_acc =
  Errlog.fold
    (fun err_key err_data acc -> {Issue.proc_name; proc_location_opt; err_key; err_data} :: acc)
    err_log issues_acc


let write_costs =
  let sil_regexp = lazy (Str.regexp ".*.sil.*") in
  fun proc_name loc cost_opt (outfile : Utils.outfile) ->
    if
      (not (Cost.is_report_suppressed proc_name))
      && is_in_files_to_analyze loc
      && (not (Procname.is_hack_builtins proc_name))
      && not (SourceFile.is_matching [Lazy.force sil_regexp] loc.Location.file)
    then JsonCostsPrinter.pp outfile.fmt {loc; proc_name; cost_opt}


let write_config_impact proc_name loc config_impact_opt (outfile : Utils.outfile) =
  if
    Config.is_checker_enabled Checker.ConfigImpactAnalysis
    && ( (Config.config_impact_strict_mode && List.is_empty Config.config_impact_strict_mode_paths)
       || ConfigImpactAnalysis.is_in_strict_mode_paths loc.Location.file
       || ExternalConfigImpactData.is_in_config_data_file proc_name )
    && is_in_files_to_analyze loc
  then
    if
      (* Ignore reporting methods of gated classes or anonymous classes *)
      ConfigImpactPostProcess.is_in_gated_classes proc_name
      || Procname.is_java_anonymous_inner_class_method proc_name
    then ()
    else
      let config_impact_opt =
        Option.map config_impact_opt ~f:ConfigImpactPostProcess.instantiate_unchecked_callees_cond
      in
      JsonConfigImpactPrinter.pp outfile.fmt
        {loc; proc_name; config_impact_opt; mode= ConfigImpactAnalysis.mode}


let process_summary proc_name loc ~cost:(cost_opt, costs_outf)
    ~config_impact:(config_impact_opt, config_impact_outf) err_log issues_acc =
  write_costs proc_name loc cost_opt costs_outf ;
  write_config_impact proc_name loc config_impact_opt config_impact_outf ;
  collect_issues proc_name (Some loc) err_log issues_acc


let process_all_summaries_and_issues ~issues_outf ~costs_outf ~config_impact_outf =
  let filters = Inferconfig.create_filters () in
  let all_issues = ref [] in
  Summary.OnDisk.iter_report_summaries_from_config
    ~f:(fun proc_name loc cost_opt config_impact_opt err_log ->
      all_issues :=
        process_summary proc_name loc ~cost:(cost_opt, costs_outf)
          ~config_impact:(config_impact_opt, config_impact_outf)
          err_log !all_issues ) ;
  (* Issues that are generated and stored outside of summaries, eg file or class-level issues *)
  IssueLog.iter_all_issues ~f:(fun _checker proc_name errlog ->
      all_issues := collect_issues proc_name None errlog !all_issues ) ;
  let all_issues = Issue.sort_filter_issues !all_issues in
  let n_issues = List.length all_issues in
  ScubaLogging.log_count ~label:"reports_unfiltered" ~value:n_issues ;
  List.iter all_issues ~f:(fun {Issue.proc_name; proc_location_opt; err_key; err_data} ->
      let error_filter = mk_error_filter filters proc_name in
      JsonIssuePrinter.pp issues_outf.Utils.fmt
        {error_filter; proc_name; proc_location_opt; err_key; err_data} ) ;
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
    JsonIssuePrinter.pp_open outf.fmt () ;
    outf
  in
  let close_fmt_and_outfile outf =
    JsonIssuePrinter.pp_close outf.Utils.fmt () ;
    Utils.close_outf outf
  in
  let issues_outf = open_outfile_and_fmt issues_json in
  let costs_outf = open_outfile_and_fmt costs_json in
  let config_impact_outf = open_outfile_and_fmt config_impact_json in
  process_all_summaries_and_issues ~issues_outf ~costs_outf ~config_impact_outf ;
  close_fmt_and_outfile config_impact_outf ;
  close_fmt_and_outfile costs_outf ;
  close_fmt_and_outfile issues_outf
