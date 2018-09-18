(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Performance Statistics gathering and reporting *)

open! IStd

type perf_stats

type stats_kind = Time of Mtime_clock.counter * Unix.process_times | Memory | TimeAndMemory

type stats_type =
  | ClangLinters of SourceFile.t
  | ClangFrontend of SourceFile.t
  | ClangFrontendLinters of SourceFile.t
  | JavaFrontend of SourceFile.t
  | TotalFrontend
  | Backend of SourceFile.t
  | TotalBackend
  | Reporting
  | Driver

val from_json : Yojson.Basic.json -> perf_stats

val aggregate : perf_stats list -> Yojson.Basic.json

val register_report : stats_kind -> stats_type -> unit
(** Register performance reporting function *)

val get_reporter : stats_type -> unit -> unit
(** Get reporting function that can be called at any time to create a performance report *)

val register_report_at_exit : stats_type -> unit
(** Create performance report when the current process terminates *)
