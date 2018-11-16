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

let clear_caches () =
  Ondemand.clear_cache () ;
  Summary.clear_cache () ;
  Typ.Procname.SQLite.clear_cache ()


(** Create tasks to analyze an execution environment *)
let analyze_source_file : SourceFile.t Tasks.doer =
 fun source_file ->
  if Config.memcached then Memcached.connect () ;
  DB.Results_dir.init source_file ;
  let exe_env = Exe_env.mk () in
  L.task_progress SourceFile.pp source_file ~f:(fun () ->
      (* clear cache for each source file to avoid it growing unboundedly *)
      clear_caches () ;
      Callbacks.analyze_file exe_env source_file ;
      if Config.write_html then Printer.write_all_html_files source_file ) ;
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
  ( match Config.modified_targets with
  | Some file ->
      MergeCapture.record_modified_targets_from_file file
  | None ->
      () ) ;
  register_active_checkers () ;
  if Config.reanalyze then Summary.reset_all ~filter:(Lazy.force Filtering.procedures_filter) ()
  else DB.Results_dir.clean_specs_dir () ;
  let all_source_files =
    SourceFiles.get_all ~filter:(Lazy.force Filtering.source_files_filter) ()
  in
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
  (* empty all caches to minimize the process heap to have less work to do when forking *)
  clear_caches () ;
  if Int.equal Config.jobs 1 then (
    Tasks.run_sequentially ~f:analyze_source_file source_files_to_analyze ;
    L.progress "@\nAnalysis finished in %as@." Pp.elapsed_time () )
  else (
    L.environment_info "Parallel jobs: %d@." Config.jobs ;
    (* Prepare tasks one cluster at a time while executing in parallel *)
    let runner = Tasks.Runner.create ~jobs:Config.jobs ~f:analyze_source_file in
    Tasks.Runner.run runner ~tasks:source_files_to_analyze ) ;
  output_json_makefile_stats source_files_to_analyze
