(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CLOpt = CommandLineOption
module L = Logging
module Cmd = InferCommandImplementation

(** Top-level driver that orchestrates build system integration, frontends, backend, and reporting *)

let setup () =
  ( match Config.command with
  | Analyze ->
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
          (buck && Option.exists buck_mode ~f:BuckMode.is_clang) || genrule_mode )
        || not
             ( Driver.is_analyze_mode driver_mode
             || Config.(
                  continue_capture || infer_is_clang || infer_is_javac || reactive_mode
                  || incremental_analysis || mark_unchanged_procs || Option.is_some run_as_child )
             )
      then (
        AnalysisDependencyGraph.store_previous_schedule_if_needed () ;
        ResultsDir.remove_results_dir () ) ;
      ResultsDir.create_results_dir () ;
      if
        Config.is_originator && (not Config.continue_capture)
        && not (Driver.is_analyze_mode driver_mode)
      then (
        DBWriter.start () ;
        SourceFiles.mark_all_stale () )
  | Explore ->
      ResultsDir.assert_results_dir "please run an infer analysis first"
  | Debug when Option.is_some Config.extract_capture_from ->
      AnalysisDependencyGraph.store_previous_schedule_if_needed () ;
      ResultsDir.remove_results_dir () ;
      ResultsDir.create_results_dir ()
  | Debug ->
      ResultsDir.assert_results_dir "please run an infer analysis or capture first"
  | Help ->
      () ) ;
  let has_result_dir =
    match Config.command with
    | Analyze | Capture | Compile | Debug | Explore | Report | ReportDiff | Run ->
        true
    | Help ->
        false
  in
  if has_result_dir then (
    DBWriter.start () ;
    if Config.is_originator then ResultsDir.RunState.add_run_to_sequence () ) ;
  has_result_dir


let print_active_checkers () =
  ( if Config.print_active_checkers && Config.is_originator then L.result ~style:[]
    else L.environment_info )
    "Active checkers: %a@."
    (Pp.seq ~sep:", " RegisterCheckers.pp_checker)
    (RegisterCheckers.get_active_checkers ())


let print_scheduler () =
  L.environment_info "Scheduler: %s@\n" (Config.string_of_scheduler Config.scheduler)


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
  if Config.is_originator then ScubaLogging.register_global_log_flushing_at_exit () ;
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
  if has_results_dir && Config.is_originator then log_environment_info () ;
  if has_results_dir && Config.debug_mode && Config.is_originator then (
    L.progress "Logs in %s@." (ResultsDir.get_path Logs) ;
    Option.iter Config.scuba_execution_id ~f:(fun id -> L.progress "Execution ID %Ld@." id) ) ;
  ( match Config.command with
  | _ when Option.is_some Config.run_as_child ->
      InferAnalyze.register_active_checkers () ;
      never_returns (ProcessPool.run_as_child ())
  | _ when Option.is_some Config.java_debug_source_file_info ->
      if Config.java_source_parser_experimental then
        JSourceLocations.debug_on_file (Option.value_exn Config.java_debug_source_file_info)
      else JSourceFileInfo.debug_on_file (Option.value_exn Config.java_debug_source_file_info)
  | Analyze ->
      Driver.run Analyze
  | Capture | Compile | Run ->
      Driver.run (Lazy.force Driver.mode_from_command_line)
  | Help ->
      Cmd.help ()
  | Report ->
      Cmd.report ()
  | ReportDiff ->
      Cmd.report_diff ()
  | Debug ->
      Cmd.debug ()
  | Explore ->
      Cmd.explore () ) ;
  (* to make sure the exitcode=0 case is logged, explicitly invoke exit *)
  L.exit 0
