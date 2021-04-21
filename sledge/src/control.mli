(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** The analysis' semantics of control flow. *)

open Control_intf

module type Queue

module PriorityQueue : Queue

module Make (_ : Config) (Dom : Domain_intf.Dom) (_ : Queue) : sig
  val exec_pgm : Llair.program -> unit

  val compute_summaries :
    Llair.program -> Dom.summary list Llair.Function.Map.t
end
