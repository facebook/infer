(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

type t =
  { conditions: ValueHistory.t list
        (** Each history represents a conditional that is affecting the path currently, with the
            most recent conditional first. The idea is to add these histories to the histories of
            all variables and memory locations modified while under the influence of these
            conditionals. *)
  ; timestamp: Timestamp.t  (** step number in an intra-procedural analysis *) }
[@@deriving compare, equal]

include AbstractDomain.Disjunct with type t := t

val initial : t

val post_exec_instr : t -> t
(** call this after each step of the symbolic execution to update the path information *)
