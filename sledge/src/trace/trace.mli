(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Debug trace logging *)

val init : trace_all:bool -> unit
(** Initialize the configuration of debug tracing. *)

type 'a printf = ('a, Format.formatter, unit) format -> 'a

val info : string -> string -> 'a printf
(** Emit a message at the current indentation level, if enabled. *)

val call : string -> string -> ('a printf -> 'b) -> 'b
(** Increase indentation level and emit a message, if enabled. *)

val retn : string -> string -> ('a printf -> 'b -> unit) -> 'b -> 'b
(** Decrease indentation level and emit a message, if enabled. *)

val flush : unit -> unit
(** Flush the internal buffers. *)
