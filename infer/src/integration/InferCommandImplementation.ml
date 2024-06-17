(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let debug () =
  if not Config.(global_tenv || procedures || source_files) then
    L.die UserError
      "Expected at least one of '--global-tenv', '--procedures' or '--source_files'.@\n"
  else (
    ( if Config.global_tenv then
        match Tenv.load_global () with
        | None ->
            L.result "No global type environment was found.@."
        | Some tenv ->
            L.result "Global type environment:@\n@[<v>%a@]" Tenv.pp tenv ) ;
    ( if Config.procedures then
        let procedures_filter = Lazy.force Filtering.procedures_filter in
        let summary_of proc_name =
          Summary.OnDisk.get ~lazy_payloads:false AnalysisRequest.all proc_name
        in
        let filter source_file proc_name =
          procedures_filter source_file proc_name
          &&
          if Config.procedures_summary_skip_empty then Option.is_some (summary_of proc_name)
          else true
        in
        if Config.procedures_summary || Config.procedures_summary_json then
          let f_console_output proc_names =
            let pp_summary fmt proc_name =
              match summary_of proc_name with
              | None ->
                  F.fprintf fmt "No summary found: %a@\n" Procname.pp proc_name
              | Some summary ->
                  Summary.pp_text fmt summary
            in
            L.result "%t" (fun fmt -> List.iter proc_names ~f:(pp_summary fmt))
          in
          let json_of_summary proc_name =
            Summary.OnDisk.get ~lazy_payloads:false AnalysisRequest.all proc_name
            |> Option.map ~f:Summary.yojson_of_t
          in
          let f_json proc_names =
            Yojson.Safe.to_channel stdout (`List (List.filter_map ~f:json_of_summary proc_names)) ;
            Out_channel.newline stdout ;
            Out_channel.flush stdout
          in
          Option.iter
            (Procedures.select_proc_names_interactive ~filter)
            ~f:(if Config.procedures_summary_json then f_json else f_console_output)
        else if Config.procedures_call_graph then
          let files_to_graph =
            match SourceFile.read_config_files_to_analyze () with
            | Some file_set ->
                SourceFile.Set.elements file_set
            | None ->
                SourceFiles.get_all ~filter:(fun _ -> true) ()
          in
          SyntacticCallGraph.(build_from_sources files_to_graph |> to_dotty)
        else
          L.result "%a"
            Config.(
              Procedures.pp_all ~filter ~proc_name:procedures_name ~defined:procedures_definedness
                ~source_file:procedures_source_file ~proc_attributes:procedures_attributes
                ~proc_cfg:procedures_cfg ~callees:procedures_callees )
            () ) ;
    if Config.source_files then (
      if Config.source_files_call_graph then SourceFileGraph.to_dotty "file-call-graph.dot"
      else if Option.is_some Config.source_files_call_graph_partition then
        Option.iter Config.source_files_call_graph_partition ~f:(fun n_workers ->
            SourceFileGraph.partition_source_file_call_graph ~n_workers )
      else if Option.is_some Config.extract_capture_from then
        match SourceFile.read_config_changed_files () with
        | None ->
            L.die UserError
              "When extracting a capture database, --changed-files-index must be specified."
        | Some files ->
            CaptureManipulation.extract ~files
              ~input_capture_path:(Option.value_exn Config.extract_capture_from)
      else if Option.is_some Config.complete_capture_from then
        let changes_made =
          CaptureManipulation.complete
            ~input_capture_path:(Option.value_exn Config.complete_capture_from)
        in
        L.result "%s" (if changes_made then "MODIFIED" else "UNMODIFIED")
      else
        let filter = Lazy.force Filtering.source_files_filter in
        L.result "%a"
          (SourceFiles.pp_all ~filter ~type_environment:Config.source_files_type_environment
             ~procedure_names:Config.source_files_procedure_names
             ~freshly_captured:Config.source_files_freshly_captured )
          () ;
        if Config.source_files_cfg then (
          let source_files = SourceFiles.get_all ~filter () in
          List.iter source_files ~f:(fun source_file ->
              (* create directory in captured/ *)
              DB.Results_dir.init ~debug:true source_file ;
              (* collect the CFGs for all the procedures in [source_file] *)
              let proc_names = SourceFiles.proc_names_of_source source_file in
              let cfgs = Procname.Hash.create (List.length proc_names) in
              List.iter proc_names ~f:(fun proc_name ->
                  Procdesc.load proc_name
                  |> Option.iter ~f:(fun cfg -> Procname.Hash.add cfgs proc_name cfg) ) ;
              (* emit the dot file in captured/... *)
              DotCfg.emit_frontend_cfg source_file cfgs ) ;
          L.result "CFGs written in %s/*/%s@." (ResultsDir.get_path Debug)
            Config.dotty_frontend_output ) ) )


let explore () =
  if (* explore bug traces *)
     Config.html then
    TraceBugs.gen_html_report ~report_json:(ResultsDir.get_path ReportJson)
      ~show_source_context:Config.source_preview ~max_nested_level:Config.max_nesting
      ~report_html_dir:(ResultsDir.get_path ReportHtml)
  else
    TraceBugs.explore ~selector_limit:None ~report_json:(ResultsDir.get_path ReportJson)
      ~report_txt:(ResultsDir.get_path ReportText) ~selected:Config.select
      ~show_source_context:Config.source_preview ~max_nested_level:Config.max_nesting


let help () =
  if
    Config.(
      list_checkers || list_categories || list_issue_types || Option.is_some write_website
      || (not (List.is_empty help_checker))
      || not (List.is_empty help_issue_type) )
  then (
    if Config.list_checkers then Help.list_checkers () ;
    if Config.list_categories then Help.list_categories () ;
    if Config.list_issue_types then Help.list_issue_types () ;
    if not (List.is_empty Config.help_checker) then Help.show_checkers Config.help_checker ;
    if not (List.is_empty Config.help_issue_type) then Help.show_issue_types Config.help_issue_type ;
    Option.iter Config.write_website ~f:(fun website_root -> Help.write_website ~website_root) ;
    () )
  else
    L.result
      "To see Infer's manual, run `infer --help`.@\n\
       To see help about the \"help\" command itself, run `infer help --help`.@\n"


module ReportSet = struct
  module type JsonReport = sig
    include Caml.Hashtbl.HashedType

    val json_loader : Yojson.lexer_state -> Lexing.lexbuf -> t list

    val string_of_reports : ?len:int -> t list -> string

    val results_dir_entry_name : ResultsDirEntryName.id
  end

  (** functor producing a json report set module (using the provided equality relation) *)
  module MakeSet (R : JsonReport) = struct
    module HT = Caml.Hashtbl.Make (R)

    type t = unit HT.t

    let create () = HT.create 1

    let load_into set results_dir =
      let filename = ResultsDirEntryName.get_path ~results_dir R.results_dir_entry_name in
      let reports = Atdgen_runtime.Util.Json.from_file R.json_loader filename in
      List.iter reports ~f:(fun r -> if not (HT.mem set r) then HT.add set r ())


    let store set =
      let reports = HT.fold (fun r () acc -> r :: acc) set [] in
      let report_file = ResultsDir.get_path R.results_dir_entry_name in
      Out_channel.write_all report_file ~data:(R.string_of_reports reports)
  end

  module Jsonbug = struct
    type t = Jsonbug_j.jsonbug [@@deriving equal]

    let hash = Hashtbl.hash

    let json_loader = Jsonbug_j.read_report

    let results_dir_entry_name = ResultsDirEntryName.ReportJson

    let string_of_reports = Jsonbug_j.string_of_report
  end

  module Jsoncost = struct
    type t = Jsoncost_j.item [@@deriving equal]

    let hash = Hashtbl.hash

    let json_loader = Jsoncost_j.read_report

    let results_dir_entry_name = ResultsDirEntryName.ReportCostsJson

    let string_of_reports = Jsoncost_j.string_of_report
  end

  module JsonConfigimpact = struct
    type t = Jsonconfigimpact_j.item [@@deriving equal]

    let hash = Hashtbl.hash

    let json_loader = Jsonconfigimpact_j.read_report

    let results_dir_entry_name = ResultsDirEntryName.ReportConfigImpactJson

    let string_of_reports = Jsonconfigimpact_j.string_of_report
  end

  module JsonbugSet = MakeSet (Jsonbug)
  module JsoncostSet = MakeSet (Jsoncost)
  module JsonconfigimpactSet = MakeSet (JsonConfigimpact)

  type accumulator =
    {report_set: JsonbugSet.t; costs_set: JsoncostSet.t; config_impact_set: JsonconfigimpactSet.t}

  let create_accumulator () =
    { report_set= JsonbugSet.create ()
    ; costs_set= JsoncostSet.create ()
    ; config_impact_set= JsonconfigimpactSet.create () }


  let process_directory {report_set; costs_set; config_impact_set} results_dir =
    JsonbugSet.load_into report_set results_dir ;
    JsoncostSet.load_into costs_set results_dir ;
    JsonconfigimpactSet.load_into config_impact_set results_dir


  let store {report_set; costs_set; config_impact_set} =
    JsonbugSet.store report_set ;
    JsoncostSet.store costs_set ;
    JsonconfigimpactSet.store config_impact_set
end

let merge_reports () =
  let acc = ReportSet.create_accumulator () in
  Config.merge_report |> List.iter ~f:(ReportSet.process_directory acc) ;
  ReportSet.store acc


let merge_summaries () =
  let (), duration =
    Utils.timeit ~f:(fun () -> DBWriter.merge_summaries ~infer_outs:Config.merge_summaries)
  in
  L.debug Analysis Quiet "Merging summaries took %a.@\n" Mtime.Span.pp duration


let report () =
  let write_from_json out_path =
    IssuesTest.write_from_json ~json_path:Config.from_json_report ~out_path
      Config.issues_tests_fields
  in
  let write_from_cost_json out_path =
    CostIssuesTest.write_from_json ~json_path:Config.from_json_costs_report ~out_path
      CostIssuesTestField.all_fields
  in
  let write_from_config_impact_json out_path =
    ConfigImpactIssuesTest.write_from_json ~json_path:Config.from_json_config_impact_report
      ~out_path
  in
  let lineage_taint_config =
    let open Config in
    LineageTaint.TaintConfig.parse ~lineage_source ~lineage_sink ~lineage_sanitizers ~lineage_limit
  in
  match
    ( Config.issues_tests
    , Config.cost_issues_tests
    , Config.config_impact_issues_tests
    , Config.lineage_json_report
    , lineage_taint_config
    , Config.merge_report
    , Config.merge_summaries
    , Config.pulse_report_flows_from_taint_source
    , Config.pulse_report_flows_to_taint_sink )
  with
  | None, None, None, false, None, [], _, None, None ->
      if not (List.is_empty Config.merge_summaries) then merge_summaries () ;
      Driver.report ()
  | _, _, _, _, _, _, _, Some _, Some _ ->
      L.die UserError
        "Only one of '--pulse-report-flows-from-taint-source' and \
         '--pulse-report-flows-to-taint-sink' can be used.@\n"
  | ( out_path
    , cost_out_path
    , config_impact_out_path
    , report_lineage_json
    , lineage_taint_config
    , []
    , []
    , taint_source
    , taint_sink ) ->
      Option.iter out_path ~f:write_from_json ;
      Option.iter cost_out_path ~f:write_from_cost_json ;
      Option.iter config_impact_out_path ~f:write_from_config_impact_json ;
      if report_lineage_json then ReportLineage.report_json () ;
      Option.iter lineage_taint_config ~f:ReportLineage.report_taint ;
      Option.iter taint_source
        ~f:(ReportDataFlows.report_data_flows_of_procname ~flow_type:FromSource) ;
      Option.iter taint_sink ~f:(ReportDataFlows.report_data_flows_of_procname ~flow_type:ToSink)
  | None, None, None, false, None, _ :: _, [], None, None ->
      merge_reports ()
  | _, _, _, _, _, _ :: _, _, _, _ | _, _, _, _, _, _, _ :: _, _, _ ->
      L.die UserError
        "Options '--merge-report' or '--merge-summaries' or '--merge-report-sumamries' cannot be \
         used with '--issues-tests', '--cost-issues-tests', '--config-impact-issues-tests', \
         '--lineage-json-report', '--lineage-source', '--lineage-taint', \
         '--pulse-report-flows-from-taint-source', '--pulse-report-flows-to-taint-sink', or each \
         other.@\n"


let report_diff () =
  (* at least one pair of reports must be passed as input to compute a differential *)
  let open Config in
  match
    ( Option.both report_current report_previous
    , Option.both costs_current costs_previous
    , Option.both config_impact_current config_impact_previous
    , Option.both stats_dir_current stats_dir_previous )
  with
  | None, None, None, None ->
      L.die UserError
        "Expected at least one pair of arguments among '--report-current'/'--report-previous', \
         '--costs-current'/'--costs-previous', \
         '--config-impact-current'/'--config-impact-previous', or \
         '--stats-dir-current'/'--stats-dir-previous'"
  | _ ->
      if
        (is_some @@ Option.both report_current report_previous)
        || (is_some @@ Option.both costs_current costs_previous)
        || (is_some @@ Option.both config_impact_current config_impact_previous)
      then
        ReportDiff.reportdiff ~report_current ~report_previous ~costs_current ~costs_previous
          ~config_impact_current ~config_impact_previous ;
      Option.both stats_dir_previous stats_dir_current
      |> Option.iter ~f:(fun (previous, current) -> StatsDiff.diff ~previous ~current)
