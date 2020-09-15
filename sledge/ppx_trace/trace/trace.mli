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
val parse : string -> (config, exn) result

val init : ?colors:bool -> ?margin:int -> ?config:config -> unit -> unit
(** Initialize the configuration of debug tracing. *)

type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4
type 'a printf = ('a, unit) fmt -> 'a
type pf = {pf: 'a. 'a printf}

val pp_styled :
  [`Bold | `Cyan | `Magenta] -> ('a, unit) fmt -> Format.formatter -> 'a
(** If config.colors is set to true, print in the specified color *)

val printf : string -> string -> 'a printf
(** Like [Format.printf], if enabled, otherwise like [Format.iprintf]. *)

val fprintf : string -> string -> Format.formatter -> 'a printf
(** Like [Format.fprintf], if enabled, otherwise like [Format.ifprintf]. *)

val kprintf : string -> string -> (Format.formatter -> unit) -> 'a printf
(** Like [Format.kprintf], if enabled, otherwise like [Format.ifprintf]. *)

val info : string -> string -> 'a printf
(** Emit a message at the current indentation level, if enabled. *)

val infok : string -> string -> (pf -> 'a) -> 'a
(** Emit a message at the current indentation level, if enabled. *)

val call : string -> string -> (pf -> 'a) -> 'a
(** Increase indentation level and emit a message, if enabled. *)

val retn : string -> string -> (pf -> 'a -> unit) -> 'a -> 'a
(** Decrease indentation level and emit a message, if enabled. *)

val trace :
     ?call:(pf -> unit)
  -> ?retn:(pf -> 'a -> unit)
  -> ?rais:(pf -> exn -> Printexc.raw_backtrace -> unit)
  -> string
  -> string
  -> (unit -> 'a)
  -> 'a
(** [trace ~call ~retn ~rais mod_name fun_name k] either simply invokes
    [k ()], when not enabled, or else increases the indentation level and
    emits the [call] message, then invokes [k ()], then decreases the
    indentation level and either emits the [retn] or [rais] message,
    depending on whether [k ()] returned normally or exceptionally. *)

val flush : unit -> unit
(** Flush the internal buffers. *)

val raisef : ?margin:int -> (string -> exn) -> ('a, unit -> _) fmt -> 'a
(** Take a function from a string message to an exception, and a format
    string with the additional arguments it specifies, and then call the
    function on the formatted string and raise the returned exception. *)

val fail : ('a, unit -> _) fmt -> 'a
(** Emit a message at the current indentation level, and raise a [Failure]
    exception indicating a fatal error. *)
