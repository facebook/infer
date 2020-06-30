(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type t

val pp : F.formatter -> t -> unit

val log : name:string -> L.debug_kind -> t -> unit
(** log to infer's log file and to Scuba *)

val log_f : name:string -> L.debug_kind -> (unit -> 'a) -> 'a
(** log GC stats for the duration of the function passed as argument to infer's log file and to
    Scuba *)

type since =
  | ProgramStart  (** get GC stats from the beginning of the program *)
  | PreviousStats of t
      (** get GC stats relative to another point in time where GC stats were obtained with
          [get ~since:ProgramStart] *)

val get : since:since -> t

val merge : t -> t -> t
(** combine statistics from two processes *)

val to_scuba_entries : prefix:string -> t -> LogEntry.t list
