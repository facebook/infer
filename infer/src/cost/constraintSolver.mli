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

type debug = {f: 'a. ('a, F.formatter, unit, unit) IStd.format4 -> 'a} [@@unboxed]

module Equalities : sig
  type t
end

val compute_costs : debug:debug -> BasicCost.t Node.IdMap.t -> Equalities.t -> unit
(** repeatedly improve the costs given the constraints *)

val get_node_nb_exec : Equalities.t -> Node.t -> BasicCost.t
(** compute the number of times a node is executed by taking into account the program structural
    (e.g. control-flow) constraints *)

val collect_constraints : debug:debug -> Procdesc.t -> Equalities.t
(** collect initial constraints for a CFG *)
