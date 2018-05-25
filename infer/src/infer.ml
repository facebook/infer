(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Top-level driver that orchestrates build system integration, frontends, backend, and
    reporting *)

module CLOpt = CommandLineOption
module L = Logging

let run driver_mode =
  let open Driver in
  run_prologue driver_mode ;
  let changed_files = read_config_changed_files () in
  capture driver_mode ~changed_files ;
  analyze_and_report driver_mode ~changed_files ;
  run_epilogue driver_mode


let setup () =
  ( match Config.command with
  | Analyze ->
      ResultsDir.assert_results_dir "have you run capture before?"
  | Report | ReportDiff ->
      ResultsDir.create_results_dir ()
  | Diff ->
      ResultsDir.remove_results_dir () ; ResultsDir.create_results_dir ()
  | Capture | Compile | Run ->
      let driver_mode = Lazy.force Driver.mode_from_command_line in
      if
        Config.(
          (* In Buck mode, delete infer-out directories inside buck-out to start fresh and to
              avoid getting errors because some of their contents is missing (removed by
              [Driver.clean_results_dir ()]). *)
          buck && flavors)
        || not
             ( Driver.(equal_mode driver_mode Analyze)
             || Config.(continue_capture || infer_is_clang || infer_is_javac || reactive_mode) )
      then ResultsDir.remove_results_dir () ;
      ResultsDir.create_results_dir () ;
      if
        CLOpt.is_originator && not Config.continue_capture
        && not Driver.(equal_mode driver_mode Analyze)
      then SourceFiles.mark_all_stale ()
  | Explore ->
      ResultsDir.assert_results_dir "please run an infer analysis first"
  | Events ->
      ResultsDir.assert_results_dir "have you run infer before?" ) ;
  if CLOpt.is_originator then ( RunState.add_run_to_sequence () ; RunState.store () ) ;
  ()


let print_active_checkers () =
  (if Config.print_active_checkers && CLOpt.is_originator then L.result else L.environment_info)
    "Analyzer: %s@."
    Config.(string_of_analyzer analyzer) ;
  (if Config.print_active_checkers && CLOpt.is_originator then L.result else L.environment_info)
    "Active checkers: %a@."
    (Pp.seq ~sep:", " RegisterCheckers.pp_checker)
    (RegisterCheckers.get_active_checkers ())


let log_environment_info () =
  L.environment_info "CWD = %s@\n" (Sys.getcwd ()) ;
  ( match Config.inferconfig_file with
  | Some file ->
      L.environment_info "Read configuration in %s@\n" file
  | None ->
      L.environment_info "No .inferconfig file found@\n" ) ;
  L.environment_info "Project root = %s@\n" Config.project_root ;
  let infer_args =
    Sys.getenv CLOpt.args_env_var |> Option.map ~f:(String.split ~on:CLOpt.env_var_sep)
    |> Option.value ~default:["<not set>"]
  in
  L.environment_info "INFER_ARGS = %a" Pp.cli_args infer_args ;
  L.environment_info "command line arguments: %a" Pp.cli_args (Array.to_list Sys.argv) ;
  print_active_checkers ()


let prepare_events_logging () =
  (* there's no point in logging data from the events command. To fetch them we'd need to run events again... *)
  if InferCommand.equal Config.command Events then ()
  else
    let log_identifier_msg =
      Printf.sprintf "Infer log identifier is %s\n" (EventLogger.get_log_identifier ())
    in
    L.environment_info "%s" log_identifier_msg ;
    if CLOpt.is_originator && Config.print_log_identifier then L.progress "%s" log_identifier_msg ;
    let log_uncaught_exn exn ~exitcode =
      EventLogger.log (EventLogger.UncaughtException (exn, exitcode))
    in
    L.set_log_uncaught_exception_callback log_uncaught_exn


let () =
  ( if Config.linters_validate_syntax_only then
      match CTLParserHelper.validate_al_files () with
      | Ok () ->
          L.exit 0
      | Error e ->
          print_endline e ; L.exit 3 ) ;
  ( match Config.check_version with
  | Some check_version ->
      if not (String.equal check_version Version.versionString) then
        L.(die UserError)
          "Provided version '%s' does not match actual version '%s'" check_version
          Version.versionString
  | None ->
      () ) ;
  if Config.print_builtins then Builtin.print_and_exit () ;
  setup () ;
  log_environment_info () ;
  prepare_events_logging () ;
  if Config.debug_mode && CLOpt.is_originator then
    L.progress "Logs in %s@." (Config.results_dir ^/ Config.log_file) ;
  ( if Config.test_determinator then (
      TestDeterminator.test_to_run_java Config.modified_lines Config.profiler_samples
        Config.method_decls_info ;
      TestDeterminator.emit_tests_to_run () )
  else
    match Config.command with
    | Analyze ->
        run Driver.Analyze
    | Capture | Compile | Run ->
        run (Lazy.force Driver.mode_from_command_line)
    | Report ->
        InferPrint.main ~report_json:None
    | ReportDiff ->
        (* at least one report must be passed in input to compute differential *)
        ( match (Config.report_current, Config.report_previous) with
        | None, None ->
            L.(die UserError)
              "Expected at least one argument among '--report-current' and '--report-previous'"
        | _ ->
            () ) ;
        ReportDiff.reportdiff ~current_report:Config.report_current
          ~previous_report:Config.report_previous
    | Diff ->
        Diff.diff (Lazy.force Driver.mode_from_command_line)
    | Explore when Config.procedures ->
        L.result "%a"
          Config.(
            Procedures.pp_all ?filter:procedures_filter ~proc_name:procedures_name
              ~attr_kind:procedures_definedness ~source_file:procedures_source_file
              ~proc_attributes:procedures_attributes)
          ()
    | Explore when Config.source_files ->
        L.result "%a"
          (SourceFiles.pp_all ?filter:Config.source_files_filter ~cfgs:Config.source_files_cfgs
             ~type_environment:Config.source_files_type_environment
             ~procedure_names:Config.source_files_procedure_names
             ~freshly_captured:Config.source_files_freshly_captured)
          ()
    | Explore ->
        let if_some key opt args =
          match opt with None -> args | Some arg -> key :: string_of_int arg :: args
        in
        let if_true key opt args = if not opt then args else key :: args in
        let if_false key opt args = if opt then args else key :: args in
        let args =
          if_some "--max-level" Config.max_nesting @@ if_true "--only-show" Config.only_show
          @@ if_false "--no-source" Config.source_preview @@ if_true "--html" Config.html
          @@ if_some "--select" Config.select ["-o"; Config.results_dir]
        in
        let prog = Config.lib_dir ^/ "python" ^/ "inferTraceBugs" in
        if is_error (Unix.waitpid (Unix.fork_exec ~prog ~argv:(prog :: args) ())) then
          L.external_error
            "** Error running the reporting script:@\n**   %s %s@\n** See error above@." prog
            (String.concat ~sep:" " args)
    | Events ->
        EventLogger.dump () ) ;
  (* to make sure the exitcode=0 case is logged, explicitly invoke exit *)
  L.exit 0
