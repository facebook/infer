(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Debug trace logging *)

(** Tracing configuration for a toplevel module. *)
type trace_mod_funs =
  { trace_mod: bool option
        (** Enable/disable tracing of all functions in module *)
  ; trace_funs: bool Map.M(String).t
        (** Enable/disable tracing of individual functions *) }

type trace_mods_funs = trace_mod_funs Map.M(String).t

type config =
  { trace_all: bool  (** Enable all tracing *)
  ; trace_mods_funs: trace_mods_funs
        (** Specify tracing of individual toplevel modules *)
  ; colors: bool  (** Enable color output *) }

val none : config
val all : config
val parse : string -> (config, exn) result

val init : ?colors:bool -> ?margin:int -> config:config -> unit -> unit
(** Initialize the configuration of debug tracing. *)

type 'a printf = ('a, Formatter.t, unit) format -> 'a
type pf = {pf: 'a. 'a printf}

val pp_styled :
     [> `Bold | `Cyan | `Magenta | `None]
  -> ('a, Format.formatter, unit, unit) format4
  -> Format.formatter
  -> 'a
(** If config.colors is set to true, print in the specificed color *)

val printf : string -> string -> 'a printf
(** Like [Format.printf], if enabled, otherwise like [Format.iprintf]. *)

val fprintf : string -> string -> Formatter.t -> 'a printf
(** Like [Format.fprintf], if enabled, otherwise like [Format.ifprintf]. *)

val kprintf : string -> string -> (Formatter.t -> unit) -> 'a printf
(** Like [Format.kprintf], if enabled, otherwise like [Format.ifprintf]. *)

val info : string -> string -> 'a printf
(** Emit a message at the current indentation level, if enabled. *)

val call : string -> string -> (pf -> 'b) -> 'b
(** Increase indentation level and emit a message, if enabled. *)

val retn : string -> string -> (pf -> 'b -> unit) -> 'b -> 'b
(** Decrease indentation level and emit a message, if enabled. *)

val flush : unit -> unit
(** Flush the internal buffers. *)

val fail : ('a, unit -> _) fmt -> 'a
(** Emit a message at the current indentation level, and raise a [Failure]
    exception indicating a fatal error. *)
