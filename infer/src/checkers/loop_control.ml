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


(* Get a pair of two maps (exit_to_guard_map, loop_header_to_guard_map) where
   exit_to_guard_map : exit_node -> guard_nodes
   loop_header_to_guard_map : loop_header (i.e. target of the back edge) -> guard_nodes and
   guard_nodes contains the nodes that may affect the looping behavior, i.e. 
   occur in the guard of the loop conditional. *)
let get_control_maps cfg =
  (* get back edges*)
  let back_edge_set = get_back_edges cfg in
  List.fold_left
    ~f:(fun Control.({exit_map; loop_header_map}) {source; target} ->
      L.(debug Analysis Medium)
        "Back-edge source: %i -> target: %i\n" (nid_int source) (nid_int target) ;
      let loop_nodes = get_all_nodes_upwards_until target [source] in
      let exit_nodes = get_exit_nodes_in_loop loop_nodes in
      (* find all the prune nodes in the loop guard *)
      let guard_prune_nodes =
        get_all_nodes_upwards_until target exit_nodes |> Control.GuardNodes.filter is_prune
      in
      let exit_map' =
        (List.fold_left ~init:exit_map ~f:(fun acc exit_node ->
             (*Make sure an exit node only belongs to a single loop *)
             assert (not (Control.ExitNodeToGuardNodes.mem exit_node acc)) ;
             Control.ExitNodeToGuardNodes.add exit_node guard_prune_nodes acc ))
          exit_nodes
      in
      let loop_map' =
        (*Make sure a loop header only belongs to a single loop *)
        assert (not (Control.LoopHeaderToGuardNodes.mem target loop_header_map)) ;
        Control.LoopHeaderToGuardNodes.add target guard_prune_nodes loop_header_map
      in
      Control.{exit_map= exit_map'; loop_header_map= loop_map'} )
    back_edge_set
    ~init:
      Control.
        { exit_map= Control.ExitNodeToGuardNodes.empty
        ; loop_header_map= Control.LoopHeaderToGuardNodes.empty }
