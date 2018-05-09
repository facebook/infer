(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Main module for the analysis after the capture phase *)
open! IStd
module L = Logging

(** Create tasks to analyze an execution environment *)
let analyze_exe_env_tasks cluster exe_env : Tasks.t =
  L.progressbar_file () ;
  Specs.clear_spec_tbl () ;
  Typ.Procname.SQLite.clear_cache () ;
  Random.self_init () ;
  Tasks.create
    [ (fun () ->
        Callbacks.iterate_callbacks exe_env ;
        if Config.write_html then Printer.write_all_html_files cluster ) ]


(** Create tasks to analyze a cluster *)
let analyze_cluster_tasks cluster_num (cluster: Cluster.t) : Tasks.t =
  let exe_env = Exe_env.mk cluster in
  L.(debug Analysis Medium)
    "@\nProcessing cluster '%a' #%d@." SourceFile.pp cluster (cluster_num + 1) ;
  analyze_exe_env_tasks cluster exe_env


let analyze_cluster cluster_num cluster = Tasks.run (analyze_cluster_tasks cluster_num cluster)

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


let process_cluster_cmdline fname =
  match Cluster.load_from_file (DB.filename_from_string fname) with
  | None ->
      (if Config.keep_going then L.internal_error else L.die InternalError)
        "Cannot find cluster file %s@." fname
  | Some (nr, cluster) ->
      analyze_cluster (nr - 1) cluster


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


let cluster_should_be_analyzed ~changed_files cluster =
  (* whether [fname] is one of the [changed_files] *)
  let is_changed_file = Option.map changed_files ~f:(SourceFile.Set.mem cluster) in
  let check_modified () =
    let modified = SourceFiles.is_freshly_captured cluster in
    if modified then L.debug Analysis Medium "Modified: %a@\n" SourceFile.pp cluster ;
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


let main ~changed_files ~makefile =
  ( match Config.modified_targets with
  | Some file ->
      MergeCapture.record_modified_targets_from_file file
  | None ->
      () ) ;
  register_active_checkers () ;
  match Config.cluster_cmdline with
  | Some fname ->
      process_cluster_cmdline fname
  | None ->
      (* delete all specs when doing a full analysis so that we do not report on procedures that do
         not exist anymore *)
      if not Config.reactive_mode then DB.Results_dir.clean_specs_dir () ;
      let all_clusters = SourceFiles.get_all () in
      let clusters_to_analyze =
        List.filter ~f:(cluster_should_be_analyzed ~changed_files) all_clusters
      in
      let n_clusters_to_analyze = List.length clusters_to_analyze in
      L.progress "Found %d%s source file%s to analyze in %s@." n_clusters_to_analyze
        ( if Config.reactive_mode || Option.is_some changed_files then
            " (out of " ^ string_of_int (List.length all_clusters) ^ ")"
        else "" )
        (if Int.equal n_clusters_to_analyze 1 then "" else "s")
        Config.results_dir ;
      let is_java =
        lazy
          (List.exists
             ~f:(fun cl -> Filename.check_suffix ".java" (SourceFile.to_string cl))
             all_clusters)
      in
      print_legend () ;
      if not (Lazy.force is_java) then (
        (* Java uses ZipLib which is incompatible with forking *)
        (* per-procedure parallelism *)
        L.environment_info "Per-procedure parallelism jobs: %d@." Config.jobs ;
        if makefile <> "" then ClusterMakefile.create_cluster_makefile [] makefile ;
        (* Prepare tasks one cluster at a time while executing in parallel *)
        let runner = Tasks.Runner.create ~jobs:Config.jobs in
        let cluster_start_tasks i cluster =
          let tasks = analyze_cluster_tasks i cluster in
          let aggregate_tasks = Tasks.aggregate ~size:Config.procedures_per_process tasks in
          Tasks.Runner.start runner ~tasks:aggregate_tasks
        in
        List.iteri ~f:cluster_start_tasks clusters_to_analyze ;
        Tasks.Runner.complete runner )
      else if makefile <> "" then
        ClusterMakefile.create_cluster_makefile clusters_to_analyze makefile
      else (
        (* This branch is reached when -j 1 is used *)
        List.iteri ~f:analyze_cluster clusters_to_analyze ;
        L.progress "@\nAnalysis finished in %as@." Pp.elapsed_time () ) ;
      output_json_makefile_stats clusters_to_analyze
