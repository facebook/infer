(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Defs : module type of AbstractDomain.FiniteSet (Procdesc.Node)
(** The node in which the reaching definition x := e is defined.

    A definition x :=e, declared at node N, reaches the current node
   if there is a path from node N to the current node such that x is
   not modified along the path **)

module ReachingDefsMap : module type of AbstractDomain.Map (Var) (Defs)
(** Map var -> its reaching definition *)

type invariant_map

val compute_invariant_map : Summary.t -> Tenv.t -> invariant_map

val extract_post : Procdesc.Node.id -> invariant_map -> ReachingDefsMap.t option
