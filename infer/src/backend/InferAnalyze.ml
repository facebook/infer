(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Main module for the analysis after the capture phase *)
open! IStd
module L = Logging

(** Create tasks to analyze an execution environment *)
let create_exe_env_tasks source_file exe_env : Tasks.t =
  L.progressbar_file () ;
  Summary.clear_cache () ;
  Typ.Procname.SQLite.clear_cache () ;
  Random.self_init () ;
  Tasks.create
    [ (fun () ->
        Callbacks.iterate_callbacks exe_env ;
        if Config.write_html then Printer.write_all_html_files source_file ) ]


(** Create tasks to analyze a cluster *)
let create_source_file_tasks (source_file: SourceFile.t) : Tasks.t =
  let exe_env = Exe_env.mk source_file in
  L.(debug Analysis Medium) "@\nProcessing '%a'@." SourceFile.pp source_file ;
  create_exe_env_tasks source_file exe_env


let analyze_source_file source_file = Tasks.run (create_source_file_tasks source_file)

let output_json_makefile_stats clusters =
  let num_files = List.length clusters in
  let num_procs = 0 in
  (* can't compute it at this stage *)
  let num_lines = 0 in
  let file_stats =
    `Assoc [("files", `Int num_files); ("procedures", `Int num_procs); ("lines", `Int num_lines)]
  in
  (* write stats file to disk, intentionally overwriting old file if it already exists *)
  let f = Out_channel.create (Filename.concat Config.results_dir Config.proc_stats_filename) in
  Yojson.Basic.pretty_to_channel f file_stats


let print_legend () =
  L.progress "Starting analysis...@\n" ;
  L.progress "@\n" ;
  L.progress "legend:@." ;
  L.progress "  \"%s\" analyzing a file@\n" Config.log_analysis_file ;
  L.progress "  \"%s\" analyzing a procedure@\n" Config.log_analysis_procedure ;
  if Config.debug_mode then (
    L.progress "  \"%s\" analyzer crashed@\n" Config.log_analysis_crash ;
    L.progress "  \"%s\" timeout: procedure analysis took too much time@\n"
      Config.log_analysis_wallclock_timeout ;
    L.progress "  \"%s\" timeout: procedure analysis took too many symbolic execution steps@\n"
      Config.log_analysis_symops_timeout ;
    L.progress "  \"%s\" timeout: procedure analysis took too many recursive iterations@\n"
      Config.log_analysis_recursion_timeout ) ;
  L.progress "@\n@?"


let source_file_should_be_analyzed ~changed_files source_file =
  (* whether [fname] is one of the [changed_files] *)
  let is_changed_file = Option.map changed_files ~f:(SourceFile.Set.mem source_file) in
  let check_modified () =
    let modified = SourceFiles.is_freshly_captured source_file in
    if modified then L.debug Analysis Medium "Modified: %a@\n" SourceFile.pp source_file ;
    modified
  in
  match is_changed_file with
  | Some b ->
      b
  | None when Config.reactive_mode ->
      check_modified ()
  | None ->
      true


let register_active_checkers () =
  match Config.analyzer with
  | Checkers | Crashcontext ->
      RegisterCheckers.get_active_checkers () |> RegisterCheckers.register
  | CaptureOnly | CompileOnly | Linters ->
      ()


let main ~changed_files =
  ( match Config.modified_targets with
  | Some file ->
      MergeCapture.record_modified_targets_from_file file
  | None ->
      () ) ;
  register_active_checkers () ;
  (* delete all specs when doing a full analysis so that we do not report on procedures that do
         not exist anymore *)
  if not Config.reactive_mode then DB.Results_dir.clean_specs_dir () ;
  let all_source_files = SourceFiles.get_all () in
  let source_files_to_analyze =
    List.filter ~f:(source_file_should_be_analyzed ~changed_files) all_source_files
  in
  let n_source_files = List.length source_files_to_analyze in
  L.progress "Found %d%s source file%s to analyze in %s@." n_source_files
    ( if Config.reactive_mode || Option.is_some changed_files then
        " (out of " ^ string_of_int (List.length all_source_files) ^ ")"
    else "" )
    (if Int.equal n_source_files 1 then "" else "s")
    Config.results_dir ;
  print_legend () ;
  if Int.equal Config.jobs 1 then (
    List.iter ~f:analyze_source_file source_files_to_analyze ;
    L.progress "@\nAnalysis finished in %as@." Pp.elapsed_time () )
  else (
    L.environment_info "Parallel jobs: %d@." Config.jobs ;
    (* Prepare tasks one cluster at a time while executing in parallel *)
    let runner = Tasks.Runner.create ~jobs:Config.jobs in
    let analyze source_file =
      let tasks = create_source_file_tasks source_file in
      let aggregate_tasks = Tasks.aggregate ~size:Config.procedures_per_process tasks in
      Tasks.Runner.start runner ~tasks:aggregate_tasks
    in
    List.iter ~f:analyze source_files_to_analyze ;
    Tasks.Runner.complete runner ) ;
  output_json_makefile_stats source_files_to_analyze
