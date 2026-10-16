(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

type t = private
  { timestamp: Timestamp.t  (** step number in an intra-procedural analysis *)
  ; is_non_disj: bool
        (** whether we are currently executing the abstract state inside the non-disjunctive
            (=over-approximate) part of the state *)
  ; loop_exit_only: Procdesc.Node.id option
        (** [Some head] for a state that over-approximates the remaining iterations of the loop
            headed by [head] (see {!PulseLoopHavoc}): it is only used to reach the exits of that
            loop, not to execute its body again *) }
[@@deriving compare, equal]

include AbstractDomain.Disjunct with type t := t

val join : t -> t -> t

val initial : t

val post_exec_instr : t -> t
(** call this after each step of the symbolic execution to update the path information *)

val set_is_non_disj : t -> t

val set_loop_exit_only : Procdesc.Node.id option -> t -> t
