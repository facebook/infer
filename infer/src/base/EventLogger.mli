(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

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

type analysis_stats =
  { analysis_nodes_visited: int
  ; analysis_status: SymOp.failure_kind option
  ; analysis_total_nodes: int
  ; clang_method_kind: ProcAttributes.clang_method_kind
  ; lang: string
  ; method_location: Location.t
  ; method_name: string
  ; num_preposts: int
  ; symops: int }

type event =
  | UncaughtException of exn * int  (** exception, exitcode *)
  | FrontendException of frontend_exception  (** record of caught exception *)
  | ProceduresTranslatedSummary of procedures_translated  (** record of procedures translated *)
  | AnalysisStats of analysis_stats  (** record of stats from procedure analysis *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val dump : unit -> unit
