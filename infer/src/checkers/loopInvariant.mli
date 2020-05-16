(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module InvariantVars : module type of AbstractDomain.FiniteSet (Var)

module VarsInLoop : module type of AbstractDomain.FiniteSet (Var)

module LoopNodes : module type of AbstractDomain.FiniteSet (Procdesc.Node)

module VarSet : module type of AbstractDomain.FiniteSet (Var)

(** Map loop header node -> all nodes in the loop *)
module LoopHeadToLoopNodes = Procdesc.NodeMap

(** Map loop head -> invariant vars in loop *)
module LoopHeadToInvVars = Procdesc.NodeMap

type invariant_map = VarsInLoop.t Procdesc.NodeMap.t

val get_inv_vars_in_loop :
     Tenv.t
  -> ReachingDefs.invariant_map
  -> is_pure_by_default:bool
  -> get_callee_purity:
       (Procname.t -> PurityDomain.ModifiedParamIndices.t AbstractDomain.Types.top_lifted option)
  -> Procdesc.Node.t
  -> LoopNodes.t
  -> VarSet.t

val get_loop_inv_var_map :
     Tenv.t
  -> (Procname.t -> PurityDomain.ModifiedParamIndices.t AbstractDomain.Types.top_lifted option)
  -> ReachingDefs.invariant_map
  -> LoopNodes.t LoopHeadToInvVars.t
  -> invariant_map
