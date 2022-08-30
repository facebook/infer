(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Debug trace logging *)

type config

val none : config
val all : config

exception Parse_failure of string

val parse_exn : string -> config
(** Parse a trace specification string to a config. May raise
    [Parse_failure]. *)

val init : ?colors:bool -> ?margin:int -> ?config:config -> unit -> unit
(** Initialize the configuration of debug tracing. *)

type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4
type 'a printf = ('a, unit) fmt -> 'a
type pf = {pf: 'a. 'a printf}

val pp_styled :
  [`Bold | `Cyan | `Magenta] -> ('a, unit) fmt -> Format.formatter -> 'a
(** If config.colors is set to true, print in the specified color *)

val printf : string -> 'a printf
(** Like [Format.printf], if enabled, otherwise like [Format.iprintf]. *)

val fprintf : string -> Format.formatter -> 'a printf
(** Like [Format.fprintf], if enabled, otherwise like [Format.ifprintf]. *)

val kprintf : string -> (Format.formatter -> unit) -> 'a printf
(** Like [Format.kprintf], if enabled, otherwise like [Format.ifprintf]. *)

val info : string -> 'a printf
(** Emit a message at the current indentation level, if enabled. *)

val infok : string -> (pf -> 'a) -> 'a
(** Emit a message at the current indentation level, if enabled. *)

val call : string -> (pf -> 'a) -> 'a
(** Increase indentation level and emit a message, if enabled. *)

val retn : string -> (pf -> 'a -> unit) -> 'a -> 'a
(** Decrease indentation level and emit a message, if enabled. *)

val dbg :
     ?call:(pf -> unit)
  -> ?retn:(pf -> 'a -> unit)
  -> ?rais:(pf -> exn -> Printexc.raw_backtrace -> unit)
  -> string
  -> (unit -> 'a)
  -> 'a
(** [dbg ~call ~retn ~rais function_name k] increases the indentation level
    and emits the [call] message, then invokes [k ()], then decreases the
    indentation level and either emits the [retn] or [rais] message,
    depending on whether [k ()] returned normally or exceptionally. If
    tracing [function_name] is not enabled, nothing is printed, but the
    [call], [retn], and [rais] functions are still executed. *)

val dbgs :
     (('s -> 'r * 's) -> 'n)
  -> ('m -> 's -> 'r * 's)
  -> ?call:(pf -> unit)
  -> ?retn:(pf -> 'r * ('s * 's) -> unit)
  -> ?rais:(pf -> 's -> exn -> Printexc.raw_backtrace -> unit)
  -> string
  -> 'm
  -> 'n
(** [dbgs thunk force] is similar to [dbg] but parameterized by a
    decomposition of a function ['m -> 'n] into and intermediate
    state-passing form using [force : 'm -> ('s -> 'r * 's)] and
    [thunk : ('s -> 'r * 's) -> 'n]. *)

val flush : unit -> unit
(** Flush the internal buffers. *)

val raisef : ?margin:int -> (string -> exn) -> ('a, unit -> _) fmt -> 'a
(** Take a function from a string message to an exception, and a format
    string with the additional arguments it specifies, and then call the
    function on the formatted string and raise the returned exception. *)

val fail : ('a, unit -> _) fmt -> 'a
(** Emit a message at the current indentation level, and raise a [Failure]
    exception indicating a fatal error. *)
