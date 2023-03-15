(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(* The definitions below taken from [Bou] "Efficient chaotic iteration strategies with widenings"
   by François Bourdoncle.
*)

(** A hierarchical ordering of a set is a well-parenthesized permutation of its elements without two
    consecutive "(". I defines a total order <= over its elements. The elements between two matching
    parentheses are called a Component. The first element of a Component is called the head. Let
    denote by H(v) the set of head of the nested components containing v. *)

module Partition : sig
  type 'node t = private
    | Empty
    | Node of {node: 'node; next: 'node t}
    | Component of {head: 'node; rest: 'node t; next: 'node t}

  val fold_nodes : ('node t, 'node, _) Container.fold

  val iter_nodes : 'a t -> f:('a -> unit) -> unit

  val fold_heads : ('node t, 'node, _) Container.fold

  val expand : fold_right:('a, 'b, 'b t) Container.fold -> 'a t -> 'b t
  (** Maps a partition nodes from ['a] to ['b] using the expansion [fold_right]. [fold_right] should
      not return its [~init] directly but must always provide a non-empty sequence. *)

  val pp : pp_node:(F.formatter -> 'node -> unit) -> F.formatter -> 'node t -> unit
end

module type PreProcCfg = sig
  module Node : sig
    type t

    type id

    val id : t -> id

    module IdMap : PrettyPrintable.PPMap with type key = id
  end

  type t

  val fold_succs : t -> (Node.t, Node.t, 'accum) Container.fold

  val start_node : t -> Node.t
end

(** A weak topological ordering (WTO) of a directed graph is a hierarchical ordering of its vertices
    such that for every edge u -> v,

    u < v and v is not in H(u) (forward edge)

    or

    v <= u and v is in H(u) (feedback edge)

    where H(u) is the set of heads of the nested components containing u.

    A WTO of a directed graph is such that the head v of every feedback edge u -> v is the head of a
    component containing its tail u. *)

module type S = sig
  module CFG : PreProcCfg

  val make : CFG.t -> CFG.Node.t Partition.t
end

module type Make = functor (CFG : PreProcCfg) -> S with module CFG = CFG

(** Implementation of Bourdoncle's "Hierarchical decomposition of a directed graph into strongly
    connected components and subcomponents". See [Bou] Figure 4, page 10. *)
module Bourdoncle_SCC : Make
