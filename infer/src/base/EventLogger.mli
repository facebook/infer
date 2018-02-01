(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type frontend_exception =
  { exception_type: string
  ; source_location_start: Location.t
  ; source_location_end: Location.t
  ; exception_file: string
  ; exception_line: int
  ; ast_node: string option
  ; lang: string }

type procedures_translated =
  { procedures_translated_total: int
  ; procedures_translated_failed: int
  ; lang: string
  ; source_file: SourceFile.t }

type analysis_stats =
  { num_preposts: int
  ; analysis_nodes_visited: int
  ; analysis_total_nodes: int
  ; symops: int
  ; method_location: Location.t
  ; analysis_status: SymOp.failure_kind option
  ; method_name: string }

type event =
  | UncaughtException of exn * int  (** exception, exitcode *)
  | FrontendException of frontend_exception  (** record of caught exception *)
  | ProceduresTranslatedSummary of procedures_translated  (** record of procedures translated *)
  | AnalysisStats of analysis_stats  (** record of stats from the backend *)

val get_log_identifier : unit -> string

val prepare : unit -> unit

val log : event -> unit

val log_multiple : event list -> unit

val dump : unit -> unit
