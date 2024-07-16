(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let db_writer_socket_name = "sqlite_write_socket"

let infer_deps_file_name = "infer-deps.txt"

type id =
  | AllocationTraces
  | AnalysisDependencyGraph
  | AnalysisDependencyGraphDot
  | AnalysisDependencyInvalidationGraphDot
  | AnalysisDB
  | AnalysisDBShm
  | AnalysisDBWal
  | CallGraphCyclesDot
  | CaptureDB
  | CaptureDBShm
  | CaptureDBWal
  | CaptureDependencies
  | ChangedFunctions
  | DBWriterSocket
  | Debug
  | Differential
  | DuplicateFunctions
  | GlobalTypeEnvironment
  | Logs
  | MissingSourceFiles
  | MissingProcedures
  | PerfEvents
  | ProcnamesLocks
  | ReactiveCaptureMissingTypes
  | ReportConfigImpactJson
  | ReportCostsJson
  | ReportHtml
  | ReportJson
  | ReportSarif
  | ReportText
  | ReportXML
  | RetainCycles
  | RunState
  | Stats
  | SyntacticDependencyGraphDot
  | Temporary
[@@deriving enumerate]

type cleanup_action = Delete | Keep [@@deriving equal]

type entry_kind = Directory | File [@@deriving equal]

type t =
  { rel_path: string  (** path inside infer-out/ *)
  ; kind: entry_kind  (** unused for now, useful for documentation *)
  ; before_incremental_analysis: cleanup_action
        (** whether this should be deleted before an incremental analysis *)
  ; before_caching_capture: cleanup_action
        (** whether this should be deleted before sending to a remote cache for the capture phase,
            e.g., a distributed Buck cache. *)
  ; before_new_capture: cleanup_action
        (** whether this should be deleted before a from-scratch, non-incremental capture *) }

let keep = function None -> Delete | Some () -> Keep

let make ~kind ?keep_before_incremental_analysis ?keep_before_caching_capture
    ?keep_before_new_capture rel_path =
  let before_incremental_analysis = keep keep_before_incremental_analysis in
  let before_caching_capture = keep keep_before_caching_capture in
  let before_new_capture = keep keep_before_new_capture in
  {rel_path; kind; before_incremental_analysis; before_caching_capture; before_new_capture}


let file ?keep_before_incremental_analysis ?keep_before_caching_capture ?keep_before_new_capture
    rel_path =
  make ~kind:File ?keep_before_incremental_analysis ?keep_before_caching_capture
    ?keep_before_new_capture rel_path


let directory ?keep_before_incremental_analysis ?keep_before_caching_capture
    ?keep_before_new_capture rel_path =
  make ~kind:Directory ?keep_before_incremental_analysis ?keep_before_caching_capture
    ?keep_before_new_capture rel_path


let of_id = function
  | AllocationTraces ->
      directory "memtrace"
  | AnalysisDependencyGraph ->
      file "analysis_dependency_graph" ~keep_before_incremental_analysis:()
        ~keep_before_new_capture:()
  | AnalysisDependencyGraphDot ->
      file "analysis_dependency_graph.dot" ~keep_before_incremental_analysis:()
  | AnalysisDependencyInvalidationGraphDot ->
      file "analysis_dependency_invalidation_graph.dot" ~keep_before_incremental_analysis:()
  | AnalysisDB ->
      file "results.db" ~keep_before_incremental_analysis:()
  | AnalysisDBShm ->
      file "results.db-shm" ~keep_before_incremental_analysis:()
  | AnalysisDBWal ->
      file "results.db-wal" ~keep_before_incremental_analysis:()
  | CallGraphCyclesDot ->
      file "call_graph_cycles.dot" ~keep_before_caching_capture:()
  | CaptureDB ->
      file "capture.db" ~keep_before_incremental_analysis:() ~keep_before_caching_capture:()
  | CaptureDBShm ->
      file "capture.db-shm" ~keep_before_incremental_analysis:()
  | CaptureDBWal ->
      file "capture.db-wal" ~keep_before_incremental_analysis:()
  | CaptureDependencies ->
      file infer_deps_file_name ~keep_before_incremental_analysis:()
  | ChangedFunctions ->
      file "changed_functions.json" ~keep_before_caching_capture:()
  | DBWriterSocket ->
      file db_writer_socket_name ~keep_before_incremental_analysis:()
  | Debug ->
      directory "captured" ~keep_before_incremental_analysis:()
  | Differential ->
      directory "differential"
  | DuplicateFunctions ->
      file "duplicates.txt" ~keep_before_incremental_analysis:()
  | GlobalTypeEnvironment ->
      file ".global.tenv" ~keep_before_incremental_analysis:() ~keep_before_caching_capture:()
  | Logs ->
      file "logs" ~keep_before_incremental_analysis:()
  | MissingSourceFiles ->
      file "missing-source-files"
  | MissingProcedures ->
      file "missing-procedures"
  | PerfEvents ->
      file "perf_events.json"
  | ProcnamesLocks ->
      directory "procnames_locks"
  | ReactiveCaptureMissingTypes ->
      file "reactive_capture_missed_types.txt"
  | ReportConfigImpactJson ->
      file "config-impact-report.json"
  | ReportCostsJson ->
      file "costs-report.json"
  | ReportHtml ->
      file "report.html"
  | ReportJson ->
      file "report.json"
  | ReportSarif ->
      file "report.sarif"
  | ReportText ->
      file "report.txt"
  | ReportXML ->
      file "report.xml"
  | RetainCycles ->
      directory "retain_cycle_dotty"
  | RunState ->
      file ".infer_runstate.json" ~keep_before_incremental_analysis:()
  | Stats ->
      directory "stats"
  | SyntacticDependencyGraphDot ->
      file "syntactic_dependency_graph.dot" ~keep_before_incremental_analysis:()
  | Temporary ->
      directory "tmp" ~keep_before_incremental_analysis:()


let path_of_entry ~results_dir {rel_path; _} = results_dir ^/ rel_path

let get_path ~results_dir id = path_of_entry ~results_dir (of_id id)

let get_filtered_paths ~results_dir ~f =
  List.filter_map all_of_id ~f:(fun id ->
      let entry = of_id id in
      if f entry then Some (path_of_entry ~results_dir entry) else None )


let to_keep_before_incremental_capture_and_analysis ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_incremental_analysis; _} ->
      equal_cleanup_action before_incremental_analysis Keep )


let to_keep_before_caching_capture ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_caching_capture; _} ->
      equal_cleanup_action before_caching_capture Keep )


let to_keep_before_new_capture ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_new_capture; _} ->
      equal_cleanup_action before_new_capture Keep )
