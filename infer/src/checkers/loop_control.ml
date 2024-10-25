(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open Control

(* Get a set of nodes, `exit_nodes`, that themselves are not in the loop but their predecessors are

    Visually:

    target (loop head)
     /|
    / .
   /  |
   . node_in_loop
   .  |\
   .  . \
   .  . exit_node
   \  .
    \ |
     \|
    source

   Often, exit_node is a prune node. *)
let get_exit_nodes_in_loop loop_nodes =
  let succs_of_loop_nodes =
    GuardNodes.fold
      (fun n acc ->
        Procdesc.Node.get_succs n
        |> List.fold ~init:acc ~f:(fun acc succ -> GuardNodes.add succ acc) )
      loop_nodes GuardNodes.empty
  in
  GuardNodes.diff succs_of_loop_nodes loop_nodes


let is_prune node =
  match Procdesc.Node.get_kind node with Procdesc.Node.Prune_node _ -> true | _ -> false


(** Remove pairs of prune nodes that are for the same condition, i.e. sibling of the same parent.
    This is necessary to prevent picking unnecessary control variables in do-while like loops *)
let remove_prune_node_pairs exit_nodes guard_nodes =
  let except_exit_nodes = GuardNodes.diff guard_nodes exit_nodes in
  L.(debug Analysis Medium) "Except exit nodes: [%a]\n" GuardNodes.pp except_exit_nodes ;
  except_exit_nodes
  |> GuardNodes.filter (fun node ->
         is_prune node
         && Procdesc.Node.get_siblings node |> Sequence.hd
            |> Option.exists ~f:(fun sibling -> not (GuardNodes.mem sibling except_exit_nodes)) )
  |> GuardNodes.union exit_nodes


(** Get a pair of maps (exit_map, loop_head_to_guard_map) where exit_map : exit_node -> loop_head
    set (i.e. target of the back edges) loop_head_to_guard_map : loop_head -> guard_nodes and
    guard_nodes contains the nodes that may affect the looping behavior, i.e. occur in the guard of
    the loop conditional. *)
let get_control_maps loop_head_to_loop_nodes =
  let acc = (ExitNodeToLoopHeads.empty, LoopHeadToGuardNodes.empty) in
  let exit_map, loop_head_to_guard_nodes =
    Procdesc.NodeMap.fold
      (fun loop_head loop_nodes (exit_map, loop_head_to_guard_nodes) ->
        let exit_nodes = get_exit_nodes_in_loop loop_nodes in
        L.(debug Analysis Medium) "Exit nodes: [%a]\n" GuardNodes.pp exit_nodes ;
        (* find all the prune nodes in the loop guard *)
        let guard_prune_nodes =
          Procdesc.Loop.get_all_nodes_upwards_until loop_head (GuardNodes.elements exit_nodes)
          |> remove_prune_node_pairs exit_nodes
          |> GuardNodes.filter is_prune
        in
        let exit_map' =
          GuardNodes.fold
            (fun exit_node exit_map_acc ->
              ExitNodeToLoopHeads.update exit_node
                (function
                  | Some existing_loop_heads ->
                      Some (LoopHeads.add loop_head existing_loop_heads)
                  | None ->
                      Some (LoopHeads.singleton loop_head) )
                exit_map_acc )
            exit_nodes exit_map
        in
        let loop_head_to_guard_nodes' =
          LoopHeadToGuardNodes.update loop_head
            (function
              | Some existing_guard_nodes ->
                  Some (GuardNodes.union existing_guard_nodes guard_prune_nodes)
              | None ->
                  Some guard_prune_nodes )
            loop_head_to_guard_nodes
        in
        (exit_map', loop_head_to_guard_nodes') )
      loop_head_to_loop_nodes acc
  in
  {exit_map; loop_head_to_guard_nodes}


let get_loop_control_maps loop_head_to_source_nodes_map =
  let loop_head_to_loop_nodes =
    Procdesc.Loop.get_loop_head_to_loop_nodes loop_head_to_source_nodes_map
  in
  get_control_maps loop_head_to_loop_nodes
