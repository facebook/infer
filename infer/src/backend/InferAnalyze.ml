(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Main module for the analysis after the capture phase *)

open! IStd
module F = Format
module L = Logging
module CLOpt = CommandLineOption

let clear_caches_except_lrus () =
  Summary.OnDisk.clear_cache () ;
  Procname.SQLite.clear_cache () ;
  BufferOverrunUtils.clear_cache ()


let clear_caches () =
  Ondemand.LocalCache.clear () ;
  clear_caches_except_lrus ()


let proc_name_of_uid =
  let statement =
    ResultsDatabase.register_statement "SELECT proc_name FROM procedures WHERE proc_uid = :k"
  in
  fun proc_uid ->
    ResultsDatabase.with_registered_statement statement ~f:(fun db stmt ->
        Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT proc_uid)
        |> SqliteUtils.check_result_code db ~log:"proc_name of proc_uid bind proc_uid" ;
        let result_option =
          SqliteUtils.result_option ~finalize:false db ~log:"proc_name of proc_uid" stmt
            ~read_row:(fun stmt -> Sqlite3.column stmt 0 |> Procname.SQLite.deserialize)
        in
        match result_option with
        | Some proc_name ->
            proc_name
        | None ->
            L.die InternalError "Requested non-existent proc_uid: %s@." proc_uid )


let analyze_target : (TaskSchedulerTypes.target, string) Tasks.doer =
  let analyze_source_file exe_env source_file =
    if Topl.is_shallow_active () then DB.Results_dir.init (Topl.sourcefile ()) ;
    DB.Results_dir.init source_file ;
    L.task_progress SourceFile.pp source_file ~f:(fun () ->
        try
          Ondemand.analyze_file exe_env source_file ;
          if Topl.is_shallow_active () && Config.debug_mode then
            DotCfg.emit_frontend_cfg (Topl.sourcefile ()) (Topl.cfg ()) ;
          if Config.write_html then Printer.write_all_html_files source_file ;
          None
        with TaskSchedulerTypes.ProcnameAlreadyLocked {dependency_filename} ->
          Some dependency_filename )
  in
  (* In call-graph scheduling, log progress every [per_procedure_logging_granularity] procedures.
     The default roughly reflects the average number of procedures in a C++ file. *)
  let per_procedure_logging_granularity = 200 in
  (* [procs_left] is set to 1 so that we log the first procedure sent to us. *)
  let procs_left = ref 1 in
  let analyze_proc_name exe_env proc_name =
    decr procs_left ;
    if Int.( <= ) !procs_left 0 then (
      L.log_task "Analysing block of %d procs, starting with %a@." per_procedure_logging_granularity
        Procname.pp proc_name ;
      procs_left := per_procedure_logging_granularity ) ;
    try
      Ondemand.analyze_proc_name_toplevel exe_env proc_name ;
      None
    with TaskSchedulerTypes.ProcnameAlreadyLocked {dependency_filename} ->
      Some dependency_filename
  in
  fun target ->
    let exe_env = Exe_env.mk () in
    (* clear cache for each source file to avoid it growing unboundedly *)
    clear_caches_except_lrus () ;
    match target with
    | Procname procname ->
        analyze_proc_name exe_env procname
    | ProcUID proc_uid ->
        proc_name_of_uid proc_uid |> analyze_proc_name exe_env
    | File source_file ->
        analyze_source_file exe_env source_file


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


let get_source_files_to_analyze ~changed_files =
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
  let pp_n_source_files ~n_total fmt n_to_analyze =
    let pp_total_if_not_all fmt n_total =
      if Config.reactive_mode || Option.is_some changed_files then
        F.fprintf fmt " (out of %d)" n_total
    in
    Format.fprintf fmt "Found %d%a source file%s to analyze in %s" n_to_analyze pp_total_if_not_all
      n_total
      (if Int.equal n_to_analyze 1 then "" else "s")
      Config.results_dir
  in
  L.progress "%a@." (pp_n_source_files ~n_total:!n_all_source_files) !n_source_files_to_analyze ;
  source_files_to_analyze


let tasks_generator_builder_for sources =
  if Config.call_graph_schedule then (
    CLOpt.warnf "WARNING: '--call-graph-schedule' is deprecated. Use '--scheduler' instead.@." ;
    SyntacticCallGraph.make sources )
  else
    match Config.scheduler with
    | File ->
        FileScheduler.make sources
    | Restart ->
        RestartScheduler.make sources
    | SyntacticCallGraph ->
        SyntacticCallGraph.make sources


let analyze source_files_to_analyze =
  if Int.equal Config.jobs 1 then (
    let target_files =
      List.rev_map (Lazy.force source_files_to_analyze) ~f:(fun sf -> TaskSchedulerTypes.File sf)
    in
    let pre_analysis_gc_stats = GCStats.get ~since:ProgramStart in
    Tasks.run_sequentially ~f:analyze_target target_files ;
    ([BackendStats.get ()], [GCStats.get ~since:(PreviousStats pre_analysis_gc_stats)]) )
  else (
    L.environment_info "Parallel jobs: %d@." Config.jobs ;
    let build_tasks_generator () =
      tasks_generator_builder_for (Lazy.force source_files_to_analyze)
    in
    (* Prepare tasks one file at a time while executing in parallel *)
    RestartScheduler.setup () ;
    let allocation_traces_dir = ResultsDir.get_path AllocationTraces in
    if Config.memtrace_analysis then (
      Utils.create_dir allocation_traces_dir ;
      if Config.is_checker_enabled Biabduction then
        L.user_warning
          "Memtrace and biabduction are incompatible \
           (https://github.com/janestreet/memtrace/issues/2)@\n" ) ;
    let runner =
      (* use a ref to pass data from prologue to epilogue without too much machinery *)
      let gc_stats_pre_fork = ref None in
      let child_prologue () =
        BackendStats.reset () ;
        gc_stats_pre_fork := Some (GCStats.get ~since:ProgramStart) ;
        if Config.memtrace_analysis then
          let filename =
            allocation_traces_dir ^/ F.asprintf "memtrace.%a" Pid.pp (Unix.getpid ())
          in
          Memtrace.start_tracing ~context:None ~sampling_rate:Config.memtrace_sampling_rate
            ~filename
          |> ignore
      in
      let child_epilogue () =
        let gc_stats_in_fork =
          match !gc_stats_pre_fork with
          | Some stats ->
              Some (GCStats.get ~since:(PreviousStats stats))
          | None ->
              L.internal_error "child did not store GC stats in its prologue, what happened?" ;
              None
        in
        (BackendStats.get (), gc_stats_in_fork)
      in
      Tasks.Runner.create ~jobs:Config.jobs ~f:analyze_target ~child_prologue ~child_epilogue
        ~tasks:build_tasks_generator
    in
    let workers_stats = Tasks.Runner.run runner in
    let collected_stats =
      Array.fold workers_stats ~init:([], [])
        ~f:(fun ((backend_stats_list, gc_stats_list) as stats_list) stats_opt ->
          match stats_opt with
          | None ->
              stats_list
          | Some (backend_stats, gc_stats_opt) ->
              ( backend_stats :: backend_stats_list
              , Option.fold ~init:gc_stats_list ~f:(fun l x -> x :: l) gc_stats_opt ) )
    in
    collected_stats )


let invalidate_changed_procedures changed_files =
  if Config.incremental_analysis then (
    let changed_files =
      match changed_files with
      | Some cf ->
          cf
      | None ->
          L.die InternalError "Incremental analysis enabled without specifying changed files"
    in
    L.progress "Incremental analysis: invalidating procedures that have been changed@." ;
    let reverse_callgraph = CallGraph.create CallGraph.default_initial_capacity in
    ReverseAnalysisCallGraph.build reverse_callgraph ;
    let total_nodes = CallGraph.n_procs reverse_callgraph in
    SourceFile.Set.iter
      (fun sf ->
        SourceFiles.proc_names_of_source sf
        |> List.iter ~f:(CallGraph.flag_reachable reverse_callgraph) )
      changed_files ;
    if Config.debug_level_analysis > 0 then
      CallGraph.to_dotty reverse_callgraph "reverse_analysis_callgraph.dot" ;
    let invalidated_nodes =
      CallGraph.fold_flagged reverse_callgraph
        ~f:(fun node acc ->
          Ondemand.LocalCache.remove node.pname ;
          Summary.OnDisk.delete node.pname ;
          acc + 1 )
        0
    in
    L.progress
      "Incremental analysis: %d nodes in reverse analysis call graph, %d of which were invalidated \
       @."
      total_nodes invalidated_nodes ;
    ScubaLogging.log_count ~label:"incremental_analysis.total_nodes" ~value:total_nodes ;
    ScubaLogging.log_count ~label:"incremental_analysis.invalidated_nodes" ~value:invalidated_nodes ;
    (* save some memory *)
    CallGraph.reset reverse_callgraph ;
    ResultsDir.scrub_for_incremental () )


let main ~changed_files =
  let start = ExecutionDuration.counter () in
  register_active_checkers () ;
  if not Config.continue_analysis then
    if Config.reanalyze then (
      L.progress "Invalidating procedures to be reanalyzed@." ;
      Summary.OnDisk.reset_all ~filter:(Lazy.force Filtering.procedures_filter) () ;
      L.progress "Done@." )
    else if not Config.incremental_analysis then DBWriter.delete_all_specs () ;
  let source_files = lazy (get_source_files_to_analyze ~changed_files) in
  (* empty all caches to minimize the process heap to have less work to do when forking *)
  clear_caches () ;
  let backend_stats_list, gc_stats_list = analyze source_files in
  BackendStats.log_aggregate backend_stats_list ;
  GCStats.log_aggregate ~prefix:"backend_stats." Analysis gc_stats_list ;
  let analysis_duration = ExecutionDuration.since start in
  L.debug Analysis Quiet "Analysis phase finished in %a@\n" Mtime.Span.pp_float_s
    (ExecutionDuration.wall_time analysis_duration) ;
  ExecutionDuration.log ~prefix:"backend_stats.scheduler_process_analysis_time" Analysis
    analysis_duration ;
  ()
