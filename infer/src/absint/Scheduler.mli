(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type S = sig
  module CFG : ProcCfg.S

  type t

  val schedule_succs : t -> CFG.Node.t -> t
  (** schedule the successors of [node] *)

  val pop : t -> (CFG.Node.t * CFG.Node.id list * t) option
  (** remove and return the node with the highest priority, the ids of its visited predecessors, and
      the new schedule *)

  val empty : CFG.t -> t
end

(** simple scheduler that visits CFG nodes in reverse postorder. fast/precise for straightline code
    and conditionals; not as good for loops (may visit nodes after a loop multiple times). *)
module ReversePostorder (CFG : ProcCfg.S) : S with module CFG = CFG
