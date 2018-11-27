(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type analysis_issue =
  { bug_kind: string
  ; bug_type: string
  ; clang_method_kind: string option
  ; exception_triggered_location: Logging.ocaml_pos option
  ; lang: string
  ; procedure_name: string
  ; source_location: Location.t }

type analysis_stats =
  { analysis_nodes_visited: int
  ; analysis_status: SymOp.failure_kind option
  ; analysis_total_nodes: int
  ; clang_method_kind: string option
  ; lang: string
  ; method_location: Location.t
  ; method_name: string
  ; num_preposts: int
  ; symops: int }

type dynamic_dispatch =
  | Dynamic_dispatch_successful
  | Dynamic_dispatch_parameters_arguments_mismatch
  | Dynamic_dispatch_model_specialization_failure

type call_trace =
  { call_location: Location.t
  ; call_result: string
  ; callee_clang_method_kind: string option
  ; callee_source_file: SourceFile.t option
  ; callee_name: string
  ; caller_name: string
  ; lang: string
  ; reason: string option
  ; dynamic_dispatch: dynamic_dispatch option }

type frontend_exception =
  { ast_node: string option
  ; exception_triggered_location: Logging.ocaml_pos
  ; exception_type: string
  ; lang: string
  ; source_location_start: Location.t
  ; source_location_end: Location.t }

type mem_perf =
  { minor_heap_mem: float
  ; promoted_minor_heap_mem: float
  ; major_heap_mem: float
  ; total_allocated_mem: float
  ; minor_collections: int
  ; major_collections: int
  ; heap_compactions: int
  ; top_heap_size: int
  ; stack_size: int
  ; minor_heap_size: int }

type time_perf =
  { real_time: float
  ; user_time: float
  ; sys_time: float
  ; children_user_time: float
  ; children_sys_time: float }

type performance_stats =
  { lang: string
  ; source_file: SourceFile.t option
  ; stats_type: string
  ; mem_perf: mem_perf option
  ; time_perf: time_perf option }

type procedures_translated =
  { lang: string
  ; procedures_translated_failed: int
  ; procedures_translated_total: int
  ; source_file: SourceFile.t }

type event =
  | AnalysisIssue of analysis_issue
  | AnalysisStats of analysis_stats
  | CallTrace of call_trace
  | FrontendException of frontend_exception
  | PerformanceStats of performance_stats
  | ProceduresTranslatedSummary of procedures_translated
  | UncaughtException of exn * int  (** exception, exitcode *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val log_skipped_pname : string -> unit

val dump : unit -> unit
