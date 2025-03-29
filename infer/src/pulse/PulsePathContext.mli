(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface

type t =
  { timestamp: Timestamp.t  (** step number in an intra-procedural analysis *)
  ; is_non_disj: bool
        (** whether we are currently executing the abstract state inside the non-disjunctive
            (=over-approximate) part of the state *) }
[@@deriving compare, equal]

include AbstractDomain.Disjunct with type t := t

val join : t -> t -> t

val initial : t

val post_exec_instr : t -> t
(** call this after each step of the symbolic execution to update the path information *)

val back_edge : t list -> t list -> int -> t list * int
(** pulse-infinite do nothing but needed for compilation *)
