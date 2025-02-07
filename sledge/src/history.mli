(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** An execution history is a current instruction pointer and some
    predecessors. [preds] are empty iff this is an entrypoint. *)

type t =
  | Init
  | Step of {curr: Llair.ip; preds: t iarray}
  | Goal_progress of t
[@@deriving sexp_of]

val init : t
(** Initial (empty) history. *)

val extend : Llair.ip -> t list -> t
(** Extend some list of predecessor histories with one step of progress at
    the given instruction pointer. *)

val progress_goal : t -> t
(** Decorate the history with an indication that progress has been made
    toward some goal. *)

val dump : ?show_root:bool -> t -> Format.formatter -> unit
(** Dump a human-readable representation of the history to the given
    formatter, with an indication of the goal-trace root if [show_root]. *)

val validate : t -> Format.formatter -> unit
(** Search for a concrete execution witness of a history, and report it. *)

val jobs : int ref
(** Number of parallel processes [validate] will use. *)

val dump_witness : string option ref
(** Filename into which to write a serialized form of the history instead of
    directly validating it. *)
