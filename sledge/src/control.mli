(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The analysis' semantics of control flow. *)

open Domain_intf
open Control_intf

module type Queue

module PriorityQueue : Queue
module RandomQueue : Queue

module Make (_ : Config) (Domain : Domain) (_ : Queue) : sig
  val exec_pgm : Llair.program -> unit

  val compute_summaries :
    Llair.program -> Domain.summary list Llair.Function.Map.t
end

module MakeDirected
    (_ : Config)
    (Domain : Domain)
    (_ : Queue)
    (Goal : Goal.S) : sig
  val exec_pgm : Llair.program -> Goal.t -> unit

  val compute_summaries :
    Llair.program -> Goal.t -> Domain.summary list Llair.Function.Map.t
end
