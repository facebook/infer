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
  | None ->
      None
  | Some index -> match Utils.read_file index with
    | Ok lines ->
        Some (SourceFile.changed_sources_from_changed_files lines)
    | Error error ->
        L.external_error "Error reading the changed files index '%s': %s@." index error ;
        None

let run driver_mode =
  let open Driver in
  run_prologue driver_mode ;
  let changed_files = read_config_changed_files () in
  capture driver_mode ~changed_files ;
  analyze_and_report driver_mode ~changed_files ;
  run_epilogue driver_mode

let remove_results_dir () =
  Utils.rmtree Config.results_dir

let create_results_dir () =
  Unix.mkdir_p (Config.results_dir ^/ Config.attributes_dir_name) ;
  Unix.mkdir_p (Config.results_dir ^/ Config.captured_dir_name) ;
  Unix.mkdir_p (Config.results_dir ^/ Config.specs_dir_name);
  L.setup_log_file ()

let assert_results_dir advice =
  if Sys.file_exists Config.results_dir <> `Yes then (
    L.user_error "ERROR: results directory %s does not exist@\nERROR: %s@."
      Config.results_dir advice;
    exit 1
  );
  L.setup_log_file ()

let setup_results_dir () =
  match Config.command with
  | Analyze -> assert_results_dir "have you run capture before?"
  | Clang | Report | ReportDiff -> create_results_dir ()
  | Capture | Compile | Run ->
      let driver_mode = Lazy.force Driver.mode_from_command_line in
      if not (Driver.(equal_driver_mode driver_mode Analyze) ||
              Config.(buck || continue_capture || maven || reactive_mode)) then
        remove_results_dir ();
      create_results_dir ()

let () =
  if Config.print_builtins then Builtin.print_and_exit ();
  setup_results_dir ();
  if Config.debug_mode then
    L.progress "Logs in %s@." (Config.results_dir ^/ Config.log_file);
  match Config.command with
  | Analyze ->
      let pp_cluster_opt fmt = function
        | None -> F.fprintf fmt "(no cluster)"
        | Some cluster -> F.fprintf fmt "of cluster %s" (Filename.basename cluster) in
      L.environment_info "Starting analysis %a" pp_cluster_opt Config.cluster_cmdline;
      InferAnalyze.register_perf_stats_report ();
      Driver.analyze_and_report Analyze ~changed_files:(read_config_changed_files ())
  | Clang ->
      let prog, args = match Array.to_list Sys.argv with
        | prog::args -> prog, args
        | [] -> assert false (* Sys.argv is never empty *) in
      ClangWrapper.exe ~prog ~args
  | Report ->
      InferPrint.main_from_config ()
  | ReportDiff ->
      (* at least one report must be passed in input to compute differential *)
      (match Config.report_current, Config.report_previous with
       | None, None ->
           failwith "Expected at least one argument among 'report-current' and 'report-previous'\n"
       | _ -> ());
      ReportDiff.reportdiff ~current_report:Config.report_current
        ~previous_report:Config.report_previous
  | Capture | Compile | Run  ->
      run (Lazy.force Driver.mode_from_command_line)
