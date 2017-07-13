(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

(** Top-level driver that orchestrates build system integration, frontends, backend, and
    reporting *)

module CLOpt = CommandLineOption
module L = Logging
module F = Format

let read_config_changed_files () =
  match Config.changed_files_index with
  | None
   -> None
  | Some index ->
    match Utils.read_file index with
    | Ok lines
     -> Some (SourceFile.changed_sources_from_changed_files lines)
    | Error error
     -> L.external_error "Error reading the changed files index '%s': %s@." index error ; None

let run driver_mode =
  let open Driver in
  run_prologue driver_mode ;
  let changed_files = read_config_changed_files () in
  capture driver_mode ~changed_files ;
  analyze_and_report driver_mode ~changed_files ;
  run_epilogue driver_mode

let results_dir_dir_markers =
  List.map ~f:(Filename.concat Config.results_dir)
    [Config.attributes_dir_name; Config.captured_dir_name; Config.specs_dir_name]

let is_results_dir () =
  let not_found = ref "" in
  let has_all_markers =
    List.for_all results_dir_dir_markers ~f:(fun d ->
        Sys.is_directory d = `Yes
        ||
        (not_found := d ;
        false) )
  in
  Result.ok_if_true has_all_markers ~error:(Printf.sprintf "'%s/' not found" !not_found)

let create_results_dir () = List.iter ~f:Unix.mkdir_p results_dir_dir_markers ; L.setup_log_file ()

let assert_results_dir advice =
  Result.iter_error (is_results_dir ()) ~f:(fun err ->
      L.(die UserError)
        "ERROR: No results directory at '%s': %s@\nERROR: %s@." Config.results_dir err advice ) ;
  L.setup_log_file ()

let remove_results_dir () =
  if Sys.is_directory Config.results_dir = `Yes then (
    Result.iter_error (is_results_dir ()) ~f:(fun err ->
        L.(die UserError)
          "ERROR: '%s' exists but does not seem to be an infer results directory: %s@\nERROR: Please delete '%s' and try again@."
          Config.results_dir err Config.results_dir ) ;
    Utils.rmtree Config.results_dir )

let setup_results_dir () =
  match Config.command with
  | Analyze
   -> assert_results_dir "have you run capture before?"
  | Clang | Report | ReportDiff
   -> create_results_dir ()
  | Diff
   -> remove_results_dir () ; create_results_dir ()
  | Capture | Compile | Run
   -> let driver_mode = Lazy.force Driver.mode_from_command_line in
      if not
           ( Driver.(equal_driver_mode driver_mode Analyze)
           || Config.(buck || continue_capture || maven || reactive_mode) )
      then remove_results_dir () ;
      create_results_dir ()

let () =
  if Config.print_builtins then Builtin.print_and_exit () ;
  setup_results_dir () ;
  if Config.debug_mode then L.progress "Logs in %s@." (Config.results_dir ^/ Config.log_file) ;
  match Config.command with
  | Analyze
   -> let pp_cluster_opt fmt = function
        | None
         -> F.fprintf fmt "(no cluster)"
        | Some cluster
         -> F.fprintf fmt "of cluster %s" (Filename.basename cluster)
      in
      L.environment_info "Starting analysis %a" pp_cluster_opt Config.cluster_cmdline ;
      InferAnalyze.register_perf_stats_report () ;
      Driver.analyze_and_report Analyze ~changed_files:(read_config_changed_files ())
  | Clang
   -> let prog, args =
        match Array.to_list Sys.argv with prog :: args -> (prog, args) | [] -> assert false
        (* Sys.argv is never empty *)
      in
      ClangWrapper.exe ~prog ~args
  | Report
   -> let report_json =
        match Config.from_json_report with
        | None
         -> Some Config.(results_dir ^/ report_json)
        | Some _
         -> (* if we start from a json report instead of the specs, do not generate a json report
              again *)
            None
      in
      InferPrint.main ~report_csv:Config.bugs_csv ~report_json
  | ReportDiff
   -> (* at least one report must be passed in input to compute differential *)
      ( match (Config.report_current, Config.report_previous) with
      | None, None
       -> failwith "Expected at least one argument among 'report-current' and 'report-previous'\n"
      | _
       -> () ) ;
      ReportDiff.reportdiff ~current_report:Config.report_current
        ~previous_report:Config.report_previous
  | Capture | Compile | Run
   -> run (Lazy.force Driver.mode_from_command_line)
  | Diff
   -> Diff.diff (Lazy.force Driver.mode_from_command_line)
