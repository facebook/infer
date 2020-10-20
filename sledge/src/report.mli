(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Issue reporting *)

val init : ?append:bool -> string -> unit
val step : unit -> unit
val unknown_call : Llair.term -> unit
val invalid_access_inst : (Formatter.t -> unit) -> Llair.inst -> unit
val invalid_access_term : (Formatter.t -> unit) -> Llair.term -> unit

type status =
  | Safe of {steps: int}
  | Unsafe of {alarms: int; steps: int}
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

type gc_stats = {allocated: float; promoted: float; peak_size: float}
[@@deriving sexp]

type times =
  {etime: float; utime: float; stime: float; cutime: float; cstime: float}

type entry =
  | ProcessTimes of times
  | GcStats of gc_stats
  | Status of status
[@@deriving sexp]

type t = {name: string; entry: entry} [@@deriving sexp]
