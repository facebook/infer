(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let buck_infer_deps_file_name = "infer-deps.txt"

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
  | ChangedFunctionsTempResults
  | DatalogFacts
  | Debug
  | Differential
  | DuplicateFunctions
  | JavaGlobalTypeEnvironment
  | Logs
  | MissingSourceFiles
  | MissingProcedures
  | PerfEvents
  | ProcnamesLocks
  | ReportConfigImpactJson
  | ReportCostsJson
  | ReportHtml
  | ReportJson
  | ReportSarif
  | ReportText
  | ReportXML
  | RetainCycles
  | RunState
  | SyntacticDependencyGraphDot
  | Temporary
  | TestDeterminatorReport
  | TestDeterminatorTempResults
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

let of_id = function
  | AllocationTraces ->
      { rel_path= "memtrace"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | AnalysisDependencyGraph ->
      { rel_path= "analysis_dependency_graph"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Keep }
  | AnalysisDependencyGraphDot ->
      { rel_path= "analysis_dependency_graph.dot"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | AnalysisDependencyInvalidationGraphDot ->
      { rel_path= "analysis_dependency_invalidation_graph.dot"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | AnalysisDB ->
      { rel_path= "results.db"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | AnalysisDBShm ->
      { rel_path= "results.db-shm"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | AnalysisDBWal ->
      { rel_path= "results.db-wal"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | CallGraphCyclesDot ->
      { rel_path= "call_graph_cycles.dot"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | CaptureDB ->
      { rel_path= "capture.db"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | CaptureDBShm ->
      { rel_path= "capture.db-shm"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | CaptureDBWal ->
      { rel_path= "capture.db-wal"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | CaptureDependencies ->
      { rel_path= buck_infer_deps_file_name
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ChangedFunctions ->
      { rel_path= "changed_functions.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | ChangedFunctionsTempResults ->
      { rel_path= "changed_functions_results"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | Debug ->
      { rel_path= "captured"
      ; kind= Directory
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | Differential ->
      { rel_path= "differential"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | DuplicateFunctions ->
      { rel_path= "duplicates.txt"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | DatalogFacts ->
      { rel_path= "facts"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | JavaGlobalTypeEnvironment ->
      { rel_path= ".global.tenv"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | Logs ->
      { rel_path= "logs"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | MissingSourceFiles ->
      { rel_path= "missing-source-files"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | MissingProcedures ->
      { rel_path= "missing-procedures"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | PerfEvents ->
      { rel_path= "perf_events.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ProcnamesLocks ->
      { rel_path= "procnames_locks"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportConfigImpactJson ->
      { rel_path= "config-impact-report.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportCostsJson ->
      { rel_path= "costs-report.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportHtml ->
      { rel_path= "report.html"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportJson ->
      { rel_path= "report.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportSarif ->
      { rel_path= "report.sarif"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportText ->
      { rel_path= "report.txt"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | ReportXML ->
      { rel_path= "report.xml"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | RetainCycles ->
      { rel_path= "retain_cycle_dotty"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | RunState ->
      { rel_path= ".infer_runstate.json"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | SyntacticDependencyGraphDot ->
      { rel_path= "syntactic_dependency_graph.dot"
      ; kind= File
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | Temporary ->
      { rel_path= "tmp"
      ; kind= Directory
      ; before_incremental_analysis= Keep
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }
  | TestDeterminatorReport ->
      { rel_path= "test_determinator.json"
      ; kind= File
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Keep
      ; before_new_capture= Delete }
  | TestDeterminatorTempResults ->
      { rel_path= "test_determinator_results"
      ; kind= Directory
      ; before_incremental_analysis= Delete
      ; before_caching_capture= Delete
      ; before_new_capture= Delete }


let path_of_entry ~results_dir {rel_path; _} = results_dir ^/ rel_path

let get_path ~results_dir id = path_of_entry ~results_dir (of_id id)

let get_filtered_paths ~results_dir ~f =
  List.filter_map all_of_id ~f:(fun id ->
      let entry = of_id id in
      if f entry then Some (path_of_entry ~results_dir entry) else None )


let to_delete_before_incremental_capture_and_analysis ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_incremental_analysis; _} ->
      equal_cleanup_action before_incremental_analysis Delete )


let to_delete_before_caching_capture ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_caching_capture; _} ->
      equal_cleanup_action before_caching_capture Delete )


let to_keep_before_new_capture ~results_dir =
  get_filtered_paths ~results_dir ~f:(fun {before_new_capture; _} ->
      equal_cleanup_action before_new_capture Keep )
