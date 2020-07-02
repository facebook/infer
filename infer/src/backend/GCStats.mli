(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type t

type since =
  | ProgramStart  (** get GC stats from the beginning of the program *)
  | PreviousStats of t
      (** get GC stats relative to another point in time where GC stats were obtained with
          [get ~since:ProgramStart] *)

val get : since:since -> t

val log : name:string -> L.debug_kind -> t -> unit
(** log to infer's log file and to Scuba *)

val log_aggregate : prefix:string -> L.debug_kind -> t list -> unit
(** log aggregate to infer's log file and to Scuba *)

val log_f : name:string -> L.debug_kind -> (unit -> 'a) -> 'a
(** log GC stats for the duration of the function passed as argument to infer's log file and to
    Scuba *)
