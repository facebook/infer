(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Json : sig
  (** only what we need for now *)
  type t = [`Int of int | `String of string | `Assoc of (string * t) list | `List of t list]
end

type scope = Global | Process

type logger

val log_begin_event :
     logger
  -> ?timestamp:Mtime.t
  -> ?categories:string list
  -> ?arguments:(string * Json.t) list
  -> name:string
  -> unit
  -> unit

val log_end_event :
  logger -> ?timestamp:Mtime.t -> ?arguments:(string * Json.t) list -> unit -> unit

val log_complete_event :
     logger
  -> timestamp:Mtime.t
  -> ?duration:Mtime.Span.t
  -> ?categories:string list
  -> ?arguments:(string * Json.t) list
  -> name:string
  -> unit
  -> unit
[@@warning "-unused-value-declaration"]

val log_instant_event : logger -> ?timestamp:Mtime.t -> name:string -> scope -> unit

val log : (logger -> unit) -> unit

val init : unit -> unit
