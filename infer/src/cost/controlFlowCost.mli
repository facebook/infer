(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module BasicCost = CostDomain.BasicCost
module Node = ProcCfg.DefaultNode

module Item : sig
  type t = [`Edge of Node.id * Node.id | `Node of Node.id]

  val pp : F.formatter -> t -> unit
end

module Sum : sig
  type t

  val pp : F.formatter -> t -> unit
end

(** A Control-flow cost represents the number of times the flow of control can go through a certain
    CFG item (a node or an edge), or a sum of such things *)
type t = [`Edge of Node.id * Node.id | `Node of Node.id | `Sum of int * Item.t list]

val make_node : Node.id -> t

val make_pred_edge : 'a -> 'b -> [> `Edge of 'b * 'a]

val make_succ_edge : 'a -> 'b -> [> `Edge of 'a * 'b]

val pp : F.formatter -> t -> unit

val sum : Item.t list -> t

module Set : sig
  type elt = t [@@deriving compare, equal]

  type t

  val create : elt -> t

  val compare_size : t -> t -> int

  val cost : t -> BasicCost.t

  val merge : from:t -> to_:t -> unit

  val pp_equalities : F.formatter -> t -> unit

  val normalize_sums : normalizer:(elt -> elt) -> t -> unit

  val sum_items : t -> Item.t list

  val infer_equalities_from_sums :
    on_infer:(elt -> elt -> unit) -> normalizer:(elt -> elt) -> t -> unit

  val init_cost : of_node:(Node.id -> BasicCost.t) -> t -> unit

  val improve_cost_from_sums :
       on_improve:(Sum.t -> BasicCost.t -> BasicCost.t -> unit)
    -> of_item:(Item.t -> BasicCost.t)
    -> t
    -> unit

  val improve_cost_with : t -> BasicCost.t -> BasicCost.t option
end
