(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

val init : ?append:bool -> string -> unit
val step_solver : unit -> unit
val step_inst : Llair.block -> Llair.inst -> unit
val step_term : Llair.block -> unit
val hit_bound : int -> unit
val unknown_call : Llair.term -> unit
val invalid_access_inst : (Format.formatter -> unit) -> Llair.inst -> unit
val invalid_access_term : (Format.formatter -> unit) -> Llair.term -> unit

type status =
  | Safe of {bound: int}
  | Unsafe of {alarms: int; bound: int}
  | Ok
  | Unsound
  | Incomplete
  | InvalidInput of string
  | Unimplemented of string
  | InternalError of string
  | Timeout
  | Memout
  | Crash of string
  | UnknownError of string
[@@deriving compare, equal, sexp]

val pp_status : status pp
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
