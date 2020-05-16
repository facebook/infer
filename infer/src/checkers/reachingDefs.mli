(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The node in which the reaching definition x := e is defined.

    A definition x :=e, declared at node N, reaches the current node if there is a path from node N
    to the current node such that x is not modified along the path **)
module Defs : module type of AbstractDomain.FiniteSet (Procdesc.Node)

(** Map var -> its reaching definition *)
module ReachingDefsMap : module type of AbstractDomain.Map (Var) (Defs)

type invariant_map

val compute_invariant_map : Procdesc.t -> invariant_map

val extract_post : Procdesc.Node.id -> invariant_map -> ReachingDefsMap.t option
