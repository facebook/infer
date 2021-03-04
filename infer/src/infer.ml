(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CLOpt = CommandLineOption
module L = Logging

(** Top-level driver that orchestrates build system integration, frontends, backend, and reporting *)

let run driver_mode =
  let open Driver in
  run_prologue driver_mode ;
  let changed_files = read_config_changed_files () in
  InferAnalyze.invalidate_changed_procedures changed_files ;
  capture driver_mode ~changed_files ;
  analyze_and_report driver_mode ~changed_files ;
  run_epilogue ()


let run driver_mode = ScubaLogging.execute_with_time_logging "run" (fun () -> run driver_mode)

let setup () =
  let db_start =
    let already_started = ref false in
    fun () ->
      if (not !already_started) && CLOpt.is_originator && DBWriter.use_daemon then (
        DBWriter.start () ;
        Epilogues.register ~f:DBWriter.stop ~description:"Stop Sqlite write daemon" ;
        already_started := true )
  in
  ( match Config.command with
  | Analyze | AnalyzeJson ->
      ResultsDir.assert_results_dir "have you run capture before?"
  | Report | ReportDiff ->
      ResultsDir.create_results_dir ()
  | Capture | Compile | Run ->
      let driver_mode = Lazy.force Driver.mode_from_command_line in
      if
        Config.(
          (* In Buck mode, delete infer-out directories inside buck-out to start fresh and to
             avoid getting errors because some of their contents is missing (removed by
             [Driver.clean_results_dir ()]). *)
          (buck && Option.exists buck_mode ~f:BuckMode.is_clang_flavors) || genrule_mode)
        || not
             ( Driver.is_analyze_mode driver_mode
             || Config.(
                  continue_capture || infer_is_clang || infer_is_javac || reactive_mode
                  || incremental_analysis) )
      then ResultsDir.remove_results_dir () ;
      ResultsDir.create_results_dir () ;
      if
        CLOpt.is_originator && (not Config.continue_capture)
        && not (Driver.is_analyze_mode driver_mode)
      then (
        db_start () ;
        SourceFiles.mark_all_stale () )
  | Explore ->
      ResultsDir.assert_results_dir "please run an infer analysis first"
  | Debug ->
      ResultsDir.assert_results_dir "please run an infer analysis or capture first"
  | Help ->
      () ) ;
  let has_result_dir =
    match Config.command with
    | Analyze | AnalyzeJson | Capture | Compile | Debug | Explore | Report | ReportDiff | Run ->
        true
    | Help ->
        false
  in
  if has_result_dir then (
    db_start () ;
    if CLOpt.is_originator then ResultsDir.RunState.add_run_to_sequence () ) ;
  has_result_dir


let print_active_checkers () =
  (if Config.print_active_checkers && CLOpt.is_originator then L.result else L.environment_info)
    "Active checkers: %a@."
    (Pp.seq ~sep:", " RegisterCheckers.pp_checker)
    (RegisterCheckers.get_active_checkers ())


let print_scheduler () =
  L.environment_info "Scheduler: %s@\n"
    ( match Config.scheduler with
    | File ->
        "file"
    | Restart ->
        "restart"
    | SyntacticCallGraph ->
        "callgraph" )


let print_cores_used () = L.environment_info "Cores used: %d@\n" Config.jobs

let log_environment_info () =
  L.environment_info "CWD = %s@\n" (Sys.getcwd ()) ;
  ( match Config.inferconfig_file with
  | Some file ->
      L.environment_info "Read configuration in %s@\n" file
  | None ->
      L.environment_info "No .inferconfig file found@\n" ) ;
  L.environment_info "Project root = %s@\n" Config.project_root ;
  let infer_args =
    Sys.getenv CLOpt.args_env_var
    |> Option.map ~f:(String.split ~on:CLOpt.env_var_sep)
    |> Option.value ~default:["<not set>"]
  in
  L.environment_info "INFER_ARGS = %a@\n"
    (Pp.cli_args_with_verbosity ~verbose:Config.debug_mode)
    infer_args ;
  L.environment_info "command line arguments: %a@\n"
    (Pp.cli_args_with_verbosity ~verbose:Config.debug_mode)
    (Array.to_list Sys.(get_argv ())) ;
  ( match Utils.get_available_memory_MB () with
  | None ->
      L.environment_info "Could not retrieve available memory (possibly not on Linux)@\n"
  | Some available_memory ->
      L.environment_info "Available memory at startup: %d MB@\n" available_memory ;
      ScubaLogging.log_count ~label:"startup_mem_avail_MB" ~value:available_memory ) ;
  print_active_checkers () ;
  print_scheduler () ;
  print_cores_used ()


let () =
  (* We specifically want to collect samples only from the main process until
     we figure out what other entries and how we want to collect *)
  if CommandLineOption.is_originator then ScubaLogging.register_global_log_flushing_at_exit () ;
  ( if Config.linters_validate_syntax_only then
    match CTLParserHelper.validate_al_files () with
    | Ok () ->
        L.exit 0
    | Error e ->
        print_endline e ;
        L.exit 3 ) ;
  ( match Config.check_version with
  | Some check_version ->
      if not (String.equal check_version Version.versionString) then
        L.(die UserError)
          "Provided version '%s' does not match actual version '%s'" check_version
          Version.versionString
  | None ->
      () ) ;
  if Config.print_builtins then Builtin.print_and_exit () ;
  let has_results_dir = setup () in
  if has_results_dir then log_environment_info () ;
  if has_results_dir && Config.debug_mode && CLOpt.is_originator then (
    L.progress "Logs in %s@." (ResultsDir.get_path Logs) ;
    Option.iter Config.scuba_execution_id ~f:(fun id -> L.progress "Execution ID %Ld@." id) ) ;
  ( match Config.command with
  | _ when Config.test_determinator && not Config.process_clang_ast ->
      TestDeterminator.compute_and_emit_test_to_run ()
  | _ when Option.is_some Config.java_debug_source_file_info ->
      if Config.java_source_parser_experimental then
        JSourceLocations.debug_on_file (Option.value_exn Config.java_debug_source_file_info)
      else JSourceFileInfo.debug_on_file (Option.value_exn Config.java_debug_source_file_info)
  | Analyze ->
      run Driver.Analyze
  | AnalyzeJson ->
      run Driver.AnalyzeJson
  | Capture | Compile | Run ->
      run (Lazy.force Driver.mode_from_command_line)
  | Help ->
      if
        Config.(
          list_checkers || list_issue_types || Option.is_some write_website
          || (not (List.is_empty help_checker))
          || not (List.is_empty help_issue_type))
      then (
        if Config.list_checkers then Help.list_checkers () ;
        if Config.list_issue_types then Help.list_issue_types () ;
        if not (List.is_empty Config.help_checker) then Help.show_checkers Config.help_checker ;
        if not (List.is_empty Config.help_issue_type) then
          Help.show_issue_types Config.help_issue_type ;
        Option.iter Config.write_website ~f:(fun website_root -> Help.write_website ~website_root) ;
        () )
      else
        L.result
          "To see Infer's manual, run `infer --help`.@\n\
           To see help about the \"help\" command itself, run `infer help --help`.@\n"
  | Report -> (
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
      match (Config.issues_tests, Config.cost_issues_tests, Config.config_impact_issues_tests) with
      | None, None, None ->
          if not Config.quiet then L.result "%t" Summary.OnDisk.pp_specs_from_config
      | out_path, cost_out_path, config_impact_out_path ->
          Option.iter out_path ~f:write_from_json ;
          Option.iter cost_out_path ~f:write_from_cost_json ;
          Option.iter config_impact_out_path ~f:write_from_config_impact_json )
  | ReportDiff ->
      (* at least one report must be passed in input to compute differential *)
      ( match
          Config.
            ( report_current
            , report_previous
            , costs_current
            , costs_previous
            , config_impact_current
            , config_impact_previous )
        with
      | None, None, None, None, None, None ->
          L.die UserError
            "Expected at least one argument among '--report-current', '--report-previous', \
             '--costs-current', '--costs-previous', '--config-impact-current', and \
             '--config-impact-previous'\n"
      | _ ->
          () ) ;
      ReportDiff.reportdiff ~current_report:Config.report_current
        ~previous_report:Config.report_previous ~current_costs:Config.costs_current
        ~previous_costs:Config.costs_previous ~current_config_impact:Config.config_impact_current
        ~previous_config_impact:Config.config_impact_previous
  | Debug when not Config.(global_tenv || procedures || source_files) ->
      L.die UserError
        "Expected at least one of '--procedures', '--source_files', or '--global-tenv'"
  | Debug ->
      ( if Config.global_tenv then
        match Tenv.load_global () with
        | None ->
            L.result "No global type environment was found.@."
        | Some tenv ->
            L.result "Global type environment:@\n@[<v>%a@]" Tenv.pp tenv ) ;
      ( if Config.procedures then
        let filter = Lazy.force Filtering.procedures_filter in
        if Config.procedures_summary || Config.procedures_summary_json then
          let f_console_output proc_names =
            let pp_summary fmt proc_name =
              match Summary.OnDisk.get proc_name with
              | None ->
                  F.fprintf fmt "No summary found: %a@\n" Procname.pp proc_name
              | Some summary ->
                  Summary.pp_text fmt summary
            in
            L.result "%t" (fun fmt -> List.iter proc_names ~f:(pp_summary fmt))
          in
          let json_of_summary proc_name =
            Summary.OnDisk.get proc_name |> Option.map ~f:Summary.yojson_of_t
          in
          let f_json proc_names =
            Yojson.Safe.to_channel stdout (`List (List.filter_map ~f:json_of_summary proc_names)) ;
            Out_channel.newline stdout ;
            Out_channel.flush stdout
          in
          Option.iter
            (Procedures.select_proc_names_interactive ~filter)
            ~f:(if Config.procedures_summary_json then f_json else f_console_output)
        else
          L.result "%a"
            Config.(
              Procedures.pp_all ~filter ~proc_name:procedures_name ~attr_kind:procedures_definedness
                ~source_file:procedures_source_file ~proc_attributes:procedures_attributes
                ~proc_cfg:procedures_cfg)
            () ) ;
      if Config.source_files then (
        let filter = Lazy.force Filtering.source_files_filter in
        L.result "%a"
          (SourceFiles.pp_all ~filter ~type_environment:Config.source_files_type_environment
             ~procedure_names:Config.source_files_procedure_names
             ~freshly_captured:Config.source_files_freshly_captured)
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
            Config.dotty_frontend_output ) )
  | Explore ->
      if (* explore bug traces *)
         Config.html then
        TraceBugs.gen_html_report ~report_json:(ResultsDir.get_path ReportJson)
          ~show_source_context:Config.source_preview ~max_nested_level:Config.max_nesting
          ~report_html_dir:(ResultsDir.get_path ReportHtml)
      else
        TraceBugs.explore ~selector_limit:None ~report_json:(ResultsDir.get_path ReportJson)
          ~report_txt:(ResultsDir.get_path ReportText) ~selected:Config.select
          ~show_source_context:Config.source_preview ~max_nested_level:Config.max_nesting ) ;
  (* to make sure the exitcode=0 case is logged, explicitly invoke exit *)
  L.exit 0
