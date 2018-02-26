(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type analysis_stats =
  { analysis_nodes_visited: int
  ; analysis_status: SymOp.failure_kind option
  ; analysis_total_nodes: int
  ; clang_method_kind: ProcAttributes.clang_method_kind option
  ; lang: string
  ; method_location: Location.t
  ; method_name: string
  ; num_preposts: int
  ; symops: int }

type call_trace =
  { call_location: Location.t
  ; call_result: string
  ; callee_name: string
  ; caller_name: string
  ; lang: string
  ; reason: string option }

type frontend_exception =
  { ast_node: string option
  ; exception_file: string
  ; exception_line: int
  ; exception_type: string
  ; lang: string
  ; source_location_start: Location.t
  ; source_location_end: Location.t }

type procedures_translated =
  { lang: string
  ; procedures_translated_failed: int
  ; procedures_translated_total: int
  ; source_file: SourceFile.t }

type event =
  | AnalysisStats of analysis_stats
  | CallTrace of call_trace
  | FrontendException of frontend_exception
  | ProceduresTranslatedSummary of procedures_translated
  | UncaughtException of exn * int  (** exception, exitcode *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val dump : unit -> unit
