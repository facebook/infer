(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BasicCost = CostDomain.BasicCost
module Node = ProcCfg.DefaultNode

(* Map associating to each node a bound on the number of times it can be executed.
   This bound is computed using environments (map: val -> values), using the following
   observation: the number of environments associated with a program point is an upperbound
   of the number of times the program point can be executed in any execution.
   The size of an environment env is computed as:
     |env| = |env(v1)| * ... * |env(n_k)|

   where |env(v)| is the size of the interval associated to v by env.

    Reference: see Stefan Bygde PhD thesis, 2010
*)

val lookup_upperbound : BasicCost.t Node.IdMap.t -> Node.id -> BasicCost.t
(** given a bound map and a node, lookup the number of times it can be executed *)

val compute_upperbound_map :
     Procdesc.t
  -> BufferOverrunAnalysis.invariant_map
  -> Control.invariant_map
  -> LoopInvariant.VarsInLoop.t Procdesc.NodeMap.t
  -> BasicCost.t Node.IdMap.t
(** compute a map from each node to an upper bound on the number of times it can be executed. *)
