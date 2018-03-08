(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Performance Statistics gathering and reporting *)

open! IStd

type perf_stats

val from_json : Yojson.Basic.json -> perf_stats

val aggregate : perf_stats list -> Yojson.Basic.json

val register_report_at_exit : string -> ?source_file:SourceFile.t -> string -> unit
(** Create performance report when the current process terminates. Automatically disabled when
    [Config.buck_cache_mode] is true. *)
