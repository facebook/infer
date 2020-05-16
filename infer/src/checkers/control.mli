(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module LoopHead = Procdesc.Node
module LoopHeads = Procdesc.NodeSet

(** Map control var -> loop head location *)
module ControlMap : module type of PrettyPrintable.MakePPMap (Var)

module GuardNodes : module type of AbstractDomain.FiniteSet (Procdesc.Node)

(** Map exit node -> loop head set *)
module ExitNodeToLoopHeads = Procdesc.NodeMap

(** Map loop head -> prune nodes in the loop guard *)
module LoopHeadToGuardNodes = Procdesc.NodeMap

type invariant_map

type loop_control_maps =
  { exit_map: LoopHeads.t ExitNodeToLoopHeads.t
  ; loop_head_to_guard_nodes: GuardNodes.t LoopHeadToGuardNodes.t }

val compute_invariant_map : Procdesc.t -> loop_control_maps -> invariant_map

val compute_control_vars :
     invariant_map
  -> LoopInvariant.VarsInLoop.t LoopHeadToGuardNodes.t
  -> LoopHead.t
  -> Location.t ControlMap.t
