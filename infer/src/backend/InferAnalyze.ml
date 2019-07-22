(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Main module for the analysis after the capture phase *)
open! IStd

module L = Logging

let clear_caches () =
  Ondemand.clear_cache () ;
  Summary.OnDisk.clear_cache () ;
  Typ.Procname.SQLite.clear_cache ()


let analyze_target : TaskScheduler.target Tasks.doer =
  let analyze_source_file exe_env source_file =
    if Topl.is_active () then DB.Results_dir.init (Topl.sourcefile ()) ;
    DB.Results_dir.init source_file ;
    L.task_progress SourceFile.pp source_file ~f:(fun () ->
        Ondemand.analyze_file exe_env source_file ;
        if Topl.is_active () && Config.debug_mode then
          Dotty.print_icfg_dotty (Topl.sourcefile ()) (Topl.cfg ()) ;
        if Config.write_html then Printer.write_all_html_files source_file )
  in
  (* In call-graph scheduling, log progress every [per_procedure_logging_granularity] procedures.
     The default roughly reflects the average number of procedures in a C++ file. *)
  let per_procedure_logging_granularity = 200 in
  (* [procs_left] is set to 1 so that we log the first procedure sent to us. *)
  let procs_left = ref 1 in
  let analyze_proc_name exe_env proc_name =
    decr procs_left ;
    if Int.( <= ) !procs_left 0 then (
      L.log_task "Analysing block of %d procs, starting with %a@."
        per_procedure_logging_granularity Typ.Procname.pp proc_name ;
      procs_left := per_procedure_logging_granularity ) ;
    Ondemand.analyze_proc_name_toplevel exe_env proc_name
  in
  fun target ->
    if Config.memcached then Memcached.connect () ;
    let exe_env = Exe_env.mk () in
    (* clear cache for each source file to avoid it growing unboundedly *)
    clear_caches () ;
    ( match target with
    | Procname procname ->
        analyze_proc_name exe_env procname
    | File source_file ->
        analyze_source_file exe_env source_file ) ;
    if Config.memcached then Memcached.disconnect ()


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
  RegisterCheckers.get_active_checkers () |> RegisterCheckers.register


let main ~changed_files =
  register_active_checkers () ;
  if Config.reanalyze then (
    L.progress "Invalidating procedures to be reanalyzed@." ;
    Summary.OnDisk.reset_all ~filter:(Lazy.force Filtering.procedures_filter) () ;
    L.progress "Done@." )
  else DB.Results_dir.clean_specs_dir () ;
  let n_all_source_files = ref 0 in
  let n_source_files_to_analyze = ref 0 in
  let filter sourcefile =
    let result =
      (Lazy.force Filtering.source_files_filter) sourcefile
      && source_file_should_be_analyzed ~changed_files sourcefile
    in
    incr n_all_source_files ;
    if result then incr n_source_files_to_analyze ;
    result
  in
  ScubaLogging.log_count ~label:"source_files_to_analyze" ~value:!n_source_files_to_analyze ;
  let source_files_to_analyze = SourceFiles.get_all ~filter () in
  L.progress "Found %d%s source file%s to analyze in %s@." !n_source_files_to_analyze
    ( if Config.reactive_mode || Option.is_some changed_files then
      " (out of " ^ string_of_int !n_all_source_files ^ ")"
    else "" )
    (if Int.equal !n_source_files_to_analyze 1 then "" else "s")
    Config.results_dir ;
  (* empty all caches to minimize the process heap to have less work to do when forking *)
  clear_caches () ;
  if Int.equal Config.jobs 1 then (
    let target_files = List.rev_map source_files_to_analyze ~f:(fun sf -> TaskScheduler.File sf) in
    Tasks.run_sequentially ~f:analyze_target target_files ;
    L.progress "@\nAnalysis finished in %as@." Pp.elapsed_time () )
  else (
    L.environment_info "Parallel jobs: %d@." Config.jobs ;
    let tasks = TaskScheduler.schedule source_files_to_analyze in
    (* Prepare tasks one cluster at a time while executing in parallel *)
    let runner = Tasks.Runner.create ~jobs:Config.jobs ~f:analyze_target ~tasks in
    Tasks.Runner.run runner ) ;
  output_json_makefile_stats source_files_to_analyze
