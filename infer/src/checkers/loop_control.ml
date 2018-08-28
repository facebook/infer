(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let nid_int n = (Procdesc.Node.get_id n :> int)

type edge_type = {source: Procdesc.Node.t; target: Procdesc.Node.t} [@@deriving compare]

(* Find back-edges by using Tarjan's DFS traversal *)
(* instead of marking, we keep track of the pred node we came from *)
let get_back_edges pdesc =
  let rec aux visited back_edges wl =
    match wl with
    | [] ->
        back_edges
    | (n, pred, ancestors) :: wl' ->
        if Procdesc.NodeSet.mem n visited then
          if Procdesc.NodeSet.mem n ancestors then
            let back_edges' =
              match pred with
              | Some n_parent ->
                  {source= n_parent; target= n} :: back_edges
              | None ->
                  assert false
            in
            aux visited back_edges' wl'
          else aux visited back_edges wl'
        else
          let ancestors = Procdesc.NodeSet.add n ancestors in
          let works =
            List.fold ~init:wl'
              ~f:(fun acc m -> (m, Some n, ancestors) :: acc)
              (Procdesc.Node.get_succs n)
          in
          aux (Procdesc.NodeSet.add n visited) back_edges works
  in
  let start_wl = [(Procdesc.get_start_node pdesc, None, Procdesc.NodeSet.empty)] in
  aux Procdesc.NodeSet.empty [] start_wl


(* Get a set of nodes, `exit_nodes`, that themselves are not in the loop but their predecessors are

  Visually:

  target
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
    Control.GuardNodes.fold
      (fun n acc ->
        Procdesc.Node.get_succs n |> Control.GuardNodes.of_list |> Control.GuardNodes.union acc )
      loop_nodes Control.GuardNodes.empty
  in
  Control.GuardNodes.diff succs_of_loop_nodes loop_nodes |> Control.GuardNodes.elements


(* Starting from the start_nodes, find all the nodes upwards until the
   target is reached, i.e picking up predecessors which have not been
   already added to the found_nodes *)
let get_all_nodes_upwards_until target start_nodes =
  let rec aux found_nodes = function
    | [] ->
        found_nodes
    | node :: wl' ->
        if Control.GuardNodes.mem node found_nodes then aux found_nodes wl'
        else
          let preds = Procdesc.Node.get_preds node in
          aux (Control.GuardNodes.add node found_nodes) (List.append preds wl')
  in
  aux (Control.GuardNodes.singleton target) start_nodes


let is_prune node =
  match Procdesc.Node.get_kind node with Procdesc.Node.Prune_node _ -> true | _ -> false


(* Remove pairs of prune nodes that are for the same condition,
   i.e. sibling of the same parent. This is necessary to prevent
   picking unnecessary control variables in do-while like loops *)
let remove_prune_node_pairs exit_nodes guard_nodes =
  let exit_nodes = Control.GuardNodes.of_list exit_nodes in
  let except_exit_nodes = Control.GuardNodes.diff guard_nodes exit_nodes in
  L.(debug Analysis Medium) "Except exit nodes: [%a]\n" Control.GuardNodes.pp except_exit_nodes ;
  except_exit_nodes
  |> Control.GuardNodes.filter (fun node ->
         is_prune node
         && Procdesc.Node.get_siblings node |> Sequence.hd
            |> Option.value_map ~default:false ~f:(fun sibling ->
                   not (Control.GuardNodes.mem sibling except_exit_nodes) ) )
  |> Control.GuardNodes.union exit_nodes


let get_loop_head_to_source_nodes cfg =
  get_back_edges cfg
  |> List.fold ~init:Procdesc.NodeMap.empty ~f:(fun loop_head_to_source_list {source; target} ->
         Procdesc.NodeMap.update target
           (function Some source_list -> Some (source :: source_list) | None -> Some [source])
           loop_head_to_source_list )


(* Get a pair of maps (exit_map, loop_head_to_guard_map) where
   exit_map : exit_node -> loop_head set (i.e. target of the back edges) 
   loop_head_to_guard_map : loop_head -> guard_nodes and
   guard_nodes contains the nodes that may affect the looping behavior, i.e. 
   occur in the guard of the loop conditional. *)
let get_control_maps cfg =
  (* Since there could be multiple back-edges per loop, collect all
     source nodes per loop head *)
  (* loop_head (target of back-edges) --> source nodes *)
  let loop_head_to_source_nodes_map = get_loop_head_to_source_nodes cfg in
  Procdesc.NodeMap.fold
    (fun loop_head source_list
         (Control.({exit_map; loop_head_to_guard_nodes}), loop_head_to_loop_nodes) ->
      L.(debug Analysis Medium)
        "Back-edge source list : [%a] --> loop_head: %i \n" (Pp.comma_seq Procdesc.Node.pp)
        source_list (nid_int loop_head) ;
      let loop_nodes = get_all_nodes_upwards_until loop_head source_list in
      let exit_nodes = get_exit_nodes_in_loop loop_nodes in
      L.(debug Analysis Medium) "Exit nodes: [%a]\n" (Pp.comma_seq Procdesc.Node.pp) exit_nodes ;
      (* find all the prune nodes in the loop guard *)
      let guard_prune_nodes =
        get_all_nodes_upwards_until loop_head exit_nodes
        |> remove_prune_node_pairs exit_nodes
        |> Control.GuardNodes.filter is_prune
      in
      let exit_map' =
        (List.fold_left ~init:exit_map ~f:(fun exit_map_acc exit_node ->
             Control.ExitNodeToLoopHeads.update exit_node
               (function
                 | Some existing_loop_heads ->
                     Some (Control.LoopHeads.add loop_head existing_loop_heads)
                 | None ->
                     Some (Control.LoopHeads.singleton loop_head))
               exit_map_acc ))
          exit_nodes
      in
      let loop_head_to_guard_nodes' =
        Control.LoopHeadToGuardNodes.update loop_head
          (function
            | Some existing_guard_nodes ->
                Some (Control.GuardNodes.union existing_guard_nodes guard_prune_nodes)
            | None ->
                Some guard_prune_nodes)
          loop_head_to_guard_nodes
      in
      let loop_head_to_loop_nodes' =
        LoopInvariant.LoopHeadToLoopNodes.update loop_head
          (function
            | Some existing_loop_nodes ->
                Some (LoopInvariant.LoopNodes.union existing_loop_nodes loop_nodes)
            | None ->
                Some loop_nodes)
          loop_head_to_loop_nodes
      in
      let open Control in
      ( {exit_map= exit_map'; loop_head_to_guard_nodes= loop_head_to_guard_nodes'}
      , loop_head_to_loop_nodes' ) )
    loop_head_to_source_nodes_map
    ( (let open Control in
      { exit_map= Control.ExitNodeToLoopHeads.empty
      ; loop_head_to_guard_nodes= Control.LoopHeadToGuardNodes.empty })
    , LoopInvariant.LoopHeadToLoopNodes.empty )
