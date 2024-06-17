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
open TaskSchedulerTypes

(** do a compaction only if a sufficient amount of time has passed since last compaction *)
let compaction_minimum_interval_ns =
  Int64.(of_int Config.compaction_minimum_interval_s * of_int 1000_000_000)


(** do a compaction if heap size over this value *)
let compaction_if_heap_greater_equal_to_words =
  (* we don't try hard to avoid overflow, apart from assuming that word size
     divides 1024 perfectly, thus multiplying with a smaller factor *)
  Config.compaction_if_heap_greater_equal_to_GB * 1024 * 1024 * (1024 / Sys.word_size_in_bits)


let do_compaction_if_needed =
  let last_compaction_time = ref (Mtime_clock.counter ()) in
  let time_since_last_compaction_is_over_threshold () =
    let ns_since_last_compaction =
      Mtime_clock.count !last_compaction_time |> Mtime.Span.to_uint64_ns
    in
    Int64.(ns_since_last_compaction >= compaction_minimum_interval_ns)
  in
  fun () ->
    let stat = Caml.Gc.quick_stat () in
    let heap_words = stat.Caml.Gc.heap_words in
    if
      heap_words >= compaction_if_heap_greater_equal_to_words
      && time_since_last_compaction_is_over_threshold ()
    then (
      L.log_task "Triggering compaction, heap size= %d GB@\n"
        (heap_words * Sys.word_size_in_bits / 1024 / 1024 / 1024) ;
      Gc.compact () ;
      last_compaction_time := Mtime_clock.counter () )
    else ()


let clear_caches () =
  Summary.OnDisk.clear_cache () ;
  BufferOverrunUtils.clear_cache () ;
  Attributes.clear_cache () ;
  Dependencies.clear ()


let proc_name_of_uid uid =
  match Attributes.load_from_uid uid with
  | Some attrs ->
      ProcAttributes.get_proc_name attrs
  | None ->
      L.die InternalError "Requested non-existent proc_uid: %s@." uid


let useful_time = ref ExecutionDuration.zero

let analyze_target : (TaskSchedulerTypes.target, TaskSchedulerTypes.analysis_result) Tasks.doer =
  let run_and_interpret_result ~f =
    try
      f () ;
      Some Ok
    with
    | RestartSchedulerException.ProcnameAlreadyLocked {dependency_filename} ->
        Some (RaceOn {dependency_filename})
    | MissingDependencyException.MissingDependencyException ->
        Some Ok
  in
  let analyze_source_file exe_env source_file =
    DB.Results_dir.init source_file ;
    L.task_progress SourceFile.pp source_file ~f:(fun () ->
        let result =
          run_and_interpret_result ~f:(fun () ->
              Ondemand.analyze_file exe_env AnalysisRequest.all source_file )
        in
        if Config.write_html then Printer.write_all_html_files source_file ;
        result )
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
    run_and_interpret_result ~f:(fun () ->
        Ondemand.analyze_proc_name_toplevel exe_env AnalysisRequest.all proc_name )
  in
  fun target ->
    let start = ExecutionDuration.counter () in
    let exe_env = Exe_env.mk () in
    let result =
      match target with
      | Procname procname ->
          analyze_proc_name exe_env procname
      | ProcUID proc_uid ->
          proc_name_of_uid proc_uid |> analyze_proc_name exe_env
      | File source_file ->
          analyze_source_file exe_env source_file
    in
    (* clear cache for each source file to avoid it growing unboundedly; we do it here to
       release memory before potentially going idle *)
    clear_caches () ;
    do_compaction_if_needed () ;
    useful_time := ExecutionDuration.add_duration_since !useful_time start ;
    result


let source_file_should_be_analyzed ~changed_files source_file =
  (* whether [fname] is one of the [changed_files] *)
  let is_changed_file =
    if Config.suffix_match_changed_files then
      let path_ends_with file =
        String.is_suffix ~suffix:(SourceFile.to_rel_path file) (SourceFile.to_rel_path source_file)
      in
      Option.map changed_files ~f:(SourceFile.Set.exists path_ends_with)
    else Option.map changed_files ~f:(SourceFile.Set.mem source_file)
  in
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
  let source_files_to_analyze = SourceFiles.get_all ~filter () in
  if not Config.incremental_analysis then
    (* write to Scuba a list of files analyzed by this non-incremental analysis, to compute some
       statistics on how frequently files are analyzed vs. modified *)
    ScubaLogging.log_many
    @@ List.map source_files_to_analyze ~f:(fun file ->
           LogEntry.mk_string ~label:"analyzed_file" ~message:(SourceFile.to_rel_path file) ) ;
  ScubaLogging.log_count ~label:"source_files_to_analyze" ~value:!n_source_files_to_analyze ;
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


let tasks_generator_builder_for replay_call_graph_opt sources =
  match replay_call_graph_opt with
  | Some replay_call_graph ->
      ReplayScheduler.make replay_call_graph sources
  | None -> (
    match Config.scheduler with
    | File ->
        FileScheduler.make sources
    | Restart ->
        RestartScheduler.make sources
    | SyntacticCallGraph ->
        SyntacticCallGraph.make sources )


let analyze replay_call_graph source_files_to_analyze =
  if Config.is_checker_enabled ConfigImpactAnalysis then
    L.debug Analysis Quiet "Config impact strict mode: %a@." ConfigImpactAnalysis.pp_mode
      ConfigImpactAnalysis.mode ;
  RestartScheduler.setup () ;
  if Int.equal Config.jobs 1 then (
    let target_files =
      List.rev_map (Lazy.force source_files_to_analyze) ~f:(fun sf -> TaskSchedulerTypes.File sf)
    in
    let pre_analysis_gc_stats = GCStats.get ~since:ProgramStart in
    Tasks.run_sequentially ~f:analyze_target target_files ;
    ( [Stats.get ()]
    , [GCStats.get ~since:(PreviousStats pre_analysis_gc_stats)]
    , [MissingDependencies.get ()] ) )
  else (
    L.environment_info "Parallel jobs: %d@." Config.jobs ;
    let build_tasks_generator () =
      (* USELESS HACK: this is called only in the orchestrator, which doesn't need to do any
         analysis itself so we can unset this ref to save minute amount of memory *)
      Ondemand.edges_to_ignore := None ;
      tasks_generator_builder_for replay_call_graph (Lazy.force source_files_to_analyze)
    in
    (* Prepare tasks one file at a time while executing in parallel *)
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
      let process_times_counter = ref None in
      let child_prologue _ =
        Stats.reset () ;
        gc_stats_pre_fork := Some (GCStats.get ~since:ProgramStart) ;
        process_times_counter := Some (ExecutionDuration.counter ()) ;
        if Config.memtrace_analysis then
          let filename =
            allocation_traces_dir ^/ F.asprintf "memtrace.%a" Pid.pp (Unix.getpid ())
          in
          Memtrace.start_tracing ~context:None ~sampling_rate:Config.memtrace_sampling_rate
            ~filename
          |> ignore
      in
      let child_epilogue _ =
        let gc_stats_in_fork =
          match !gc_stats_pre_fork with
          | Some stats ->
              Some (GCStats.get ~since:(PreviousStats stats))
          | None ->
              L.internal_error "child did not store GC stats in its prologue, what happened?" ;
              None
        in
        let () =
          match !process_times_counter with
          | Some counter ->
              Stats.set_process_times (ExecutionDuration.since counter)
          | None ->
              L.internal_error
                "Child did not start the process times counter in its prologue, what happened?"
        in
        Stats.set_useful_times !useful_time ;
        (Stats.get (), gc_stats_in_fork, MissingDependencies.get ())
      in
      ScubaLogging.log_count ~label:"num_analysis_workers" ~value:Config.jobs ;
      Tasks.Runner.create ~jobs:Config.jobs ~f:analyze_target ~child_prologue ~child_epilogue
        build_tasks_generator
    in
    let workers_stats = Tasks.Runner.run runner in
    let collected_backend_stats, collected_gc_stats, collected_missing_deps =
      Array.fold workers_stats ~init:([], [], [])
        ~f:(fun ((backend_stats_list, gc_stats_list, missing_deps_list) as stats_list) stats_opt ->
          match stats_opt with
          | None ->
              stats_list
          | Some (backend_stats, gc_stats_opt, missing_deps) ->
              ( backend_stats :: backend_stats_list
              , Option.fold ~init:gc_stats_list ~f:(fun l x -> x :: l) gc_stats_opt
              , missing_deps :: missing_deps_list ) )
    in
    (collected_backend_stats, collected_gc_stats, collected_missing_deps) )


let main ~changed_files =
  (* Pre-compute the task generator for a replay analysis if needed. This needs to happen now
     because it could depend on reading the previously-computed summaries (which we are about to
     delete in preparation for the new analysis), but also because loading the dependency graph now
     has an important side-effect: it sets [Ondemand.edges_to_ignore], which is information that
     worker processes will need *)
  let replay_call_graph_opt =
    if Config.replay_analysis_schedule then AnalysisDependencyGraph.load_previous_schedule ()
    else None
  in
  let start = ExecutionDuration.counter () in
  register_active_checkers () ;
  if not Config.continue_analysis then
    if Config.reanalyze then (
      L.progress "Invalidating procedures to be reanalyzed@." ;
      let procedures = Procedures.get_all ~filter:(Lazy.force Filtering.procedures_filter) () in
      Summary.OnDisk.delete_all ~procedures ;
      IssueLog.invalidate_all ~procedures ;
      L.progress "Done@." )
    else if not Config.incremental_analysis then DBWriter.delete_all_specs () ;
  let source_files = lazy (get_source_files_to_analyze ~changed_files) in
  (* empty all caches to minimize the process heap to have less work to do when forking *)
  clear_caches () ;
  let initial_spec_count =
    if Config.incremental_analysis then Some (Summary.OnDisk.get_count ()) else None
  in
  let backend_stats_list, gc_stats_list, missing_deps_list =
    analyze replay_call_graph_opt source_files
  in
  MissingDependencies.save missing_deps_list ;
  if Config.incremental_analysis then (
    let final_spec_count = Summary.OnDisk.get_count () in
    let initial_spec_count = Option.value_exn initial_spec_count in
    let specs_computed = final_spec_count - initial_spec_count in
    L.progress "Incremental analysis: Computed %d procedure summaries.@." specs_computed ;
    ScubaLogging.log_count ~label:"incremental_analysis.specs_computed" ~value:specs_computed ) ;
  Stats.log_aggregate backend_stats_list ;
  GCStats.log_aggregate ~prefix:"backend_stats." Analysis gc_stats_list ;
  let analysis_duration = ExecutionDuration.since start in
  L.debug Analysis Quiet "Analysis phase finished in %a@\n" Mtime.Span.pp
    (ExecutionDuration.wall_time analysis_duration) ;
  if Config.reactive_capture then
    ReactiveCapture.store_missed_captures
      ~source_files_filter:(source_file_should_be_analyzed ~changed_files)
      () ;
  ExecutionDuration.log ~prefix:"backend_stats.scheduler_process_analysis_time" Analysis
    analysis_duration ;
  (* delete any previous analysis schedule once the new analysis has finished to avoid keeping a
     stale schedule around that could be misused later *)
  ( try Unix.unlink (ResultsDir.get_path AnalysisDependencyGraph) with _ -> () ) ;
  if Config.is_originator && Config.store_analysis_schedule then
    AnalysisDependencyGraph.store_previous_schedule () ;
  ()
