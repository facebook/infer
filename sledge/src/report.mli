(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

val init : ?append:bool -> string -> unit
val step_solver : unit -> unit
val step_inst : Llair.ip -> unit
val step_term : Llair.block -> unit
val hit_loop_bound : int -> unit
val hit_switch_bound : int -> unit
val unknown_call : Llair.term -> unit
val alarm : Alarm.t -> dp_witness:(Format.formatter -> unit) -> unit

val reached_goal :
     dp_goal:(Format.formatter -> unit)
  -> dp_witness:(Format.formatter -> unit)
  -> unit

val unreachable_goal : dp_path:(Format.formatter -> unit) -> unit
val unimplemented : string -> Llair.func -> unit

type status =
  | Safe of {bound: int; switches: int}
  | Unsafe of {alarms: int; bound: int; switches: int}
  | Reached_goal of {steps: int}
  | Unreachable_goal
  | Ok
  | Unsound
  | Incomplete
  | InvalidInput of string
  | Unimplemented of string
  | InternalError of string
  | Timeout
  | Memout
  | Abort
  | Assert of string
  | UnknownError of string
[@@deriving compare, equal, sexp]

val pp_status : status pp
val pp_status_coarse : status pp
val safe_or_unsafe : unit -> status
val status : status -> unit
val coverage : Llair.program -> unit

type gc_stats = {allocated: float; promoted: float; peak_size: float}
[@@deriving sexp]

type times =
  {etime: float; utime: float; stime: float; cutime: float; cstime: float}
[@@deriving sexp]

type coverage = {steps: int; hit: int; fraction: float; solver_steps: int}
[@@deriving compare, equal, sexp]

type entry =
  | ProcessTimes of times
  | GcStats of gc_stats
  | Status of status
  | Coverage of coverage
[@@deriving sexp]

type t = {name: string; entry: entry} [@@deriving sexp]
