(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

val pp : prefix:string -> Format.formatter -> t -> unit

type counter

type 'a evaluation_result = {result: 'a; execution_duration: t}

val zero : t

val counter : unit -> counter

val since : counter -> t

val add_duration_since : t -> counter -> t

val add : t -> t -> t

val wall_time : t -> Mtime.span

val total_useful_s : t -> float
(** seconds of user plus system time *)

val timed_evaluate : f:(unit -> 'a) -> 'a evaluation_result

val log : prefix:string -> Logging.debug_kind -> t -> unit
(** log to debug logs and to Scuba *)

val to_scuba_entries : prefix:string -> t -> LogEntry.t list
