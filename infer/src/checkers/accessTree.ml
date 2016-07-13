(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

(** tree of (trace, access path) associations organized by structure of access paths *)
module Make (TraceDomain : AbstractDomain.S) = struct

  module AccessMap = PrettyPrintable.MakePPMap(struct
      type t = AccessPath.access
      let compare = AccessPath.access_compare
      let pp_key = AccessPath.pp_access
    end)

  module BaseMap = PrettyPrintable.MakePPMap(struct
      type t = AccessPath.base
      let compare = AccessPath.base_compare
      let pp_key = AccessPath.pp_base
    end)

  type node = TraceDomain.astate * tree
  and tree =
    | Subtree of node AccessMap.t (* map from access -> nodes. a leaf is encoded as an empty map *)
    | Star (* special leaf for starred access paths *)

  (* map from base var -> access subtree *)
  type t = node BaseMap.t

  (** Here's how to represent a few different kinds of trace * access path associations:
      (x, T)               := { x |-> (T, Subtree {}) }
      (x.f, T)             := { x |-> (empty, Subtree { f |-> (T, Subtree {}) }) }
      (x*, T)              := { x |-> (T, Star) }
      (x.f*, T)            := { x |-> (empty, Subtree { f |-> (T, Star) }) }
      (x, T1), (y, T2)     := { x |-> (T1, Subtree {}), y |-> (T2, Subtree {}) }
      (x.f, T1), (x.g, T2) := { x |-> (empty, Subtree { f |-> (T1, Subtree {}),
                                                        g |-> (T2, Subtree {}) }) }
  *)

  let empty = BaseMap.empty

  let make_node trace subtree =
    trace, Subtree subtree

  let empty_node =
    make_node TraceDomain.initial AccessMap.empty

  let make_normal_leaf trace =
    make_node trace AccessMap.empty

  let make_starred_leaf trace =
    trace, Star

  let make_access_node base_trace access trace =
    make_node base_trace (AccessMap.singleton access (make_normal_leaf trace))

  let make_empty_trace_access_node trace access =
    make_access_node TraceDomain.initial access trace

  (** find all of the traces in [tree] and join them with [orig_trace] *)
  let rec join_all_traces orig_trace tree =
    let node_join_traces _ (trace, node) trace_acc =
      let trace_acc' = TraceDomain.join trace_acc trace in
      match node with
      | Star -> trace_acc'
      | Subtree subtree -> join_all_traces trace_acc' subtree in
    AccessMap.fold node_join_traces tree orig_trace

  (** retrieve the trace associated with [ap] from [tree] *)
  let get_trace ap tree =
    let rec accesses_get_trace access_list trace tree =
      match access_list, tree with
      | _, Star ->
          trace, Star
      | [], (Subtree _ as tree) ->
          trace, tree
      | access :: accesses, Subtree subtree ->
          let access_trace, access_subtree = AccessMap.find access subtree in
          accesses_get_trace accesses access_trace access_subtree in
    let get_trace_ base accesses tree =
      let base_trace, base_tree = BaseMap.find base tree in
      accesses_get_trace accesses base_trace base_tree in
    let base, accesses = AccessPath.extract ap in
    match get_trace_ base accesses tree with
    | trace, Star ->
        Some trace
    | trace, Subtree subtree ->
        if AccessPath.is_exact ap
        then Some trace
        else
          (* input query was [ap]*, and [trace] is the trace associated with [ap]. get the traces
             associated with the children of [ap] in [tree] and join them with [trace] *)
          Some (join_all_traces trace subtree)
    | exception Not_found ->
        None

  let rec access_tree_lteq ((lhs_trace, lhs_tree) as lhs) ((rhs_trace, rhs_tree) as rhs) =
    if lhs == rhs
    then true
    else
      TraceDomain.(<=) ~lhs:lhs_trace ~rhs:rhs_trace &&
      match lhs_tree, rhs_tree with
      | Subtree lhs_subtree, Subtree rhs_subtree ->
          AccessMap.for_all
            (fun k lhs_v ->
               try
                 let rhs_v = AccessMap.find k rhs_subtree in
                 access_tree_lteq lhs_v rhs_v
               with Not_found -> false)
            lhs_subtree
      | _, Star ->
          true
      | Star, Subtree _ ->
          false

  let (<=) ~lhs ~rhs =
    if lhs == rhs
    then true
    else
      BaseMap.for_all
        (fun k lhs_v ->
           try
             let rhs_v = BaseMap.find k rhs in
             access_tree_lteq lhs_v rhs_v
           with Not_found -> false)
        lhs

  let node_join f_node_merge f_trace_merge ((trace1, tree1) as node1) ((trace2, tree2) as node2) =
    if node1 == node2
    then node1
    else
      let trace' = f_trace_merge trace1 trace2 in
      (* note: this is much-uglified by address equality optimization checks. skip to the else cases
         for the actual semantics *)
      match tree1, tree2 with
      | Subtree subtree1, Subtree subtree2 ->
          let tree' = AccessMap.merge (fun _ v1 v2 -> f_node_merge v1 v2) subtree1 subtree2 in
          if trace' == trace1 && tree' == subtree1
          then node1
          else if trace' == trace2 && tree' == subtree2
          then node2
          else trace', Subtree tree'
      | Star, Star ->
          if trace' == trace1
          then node1
          else if trace' == trace2
          then node2
          else trace', Star
      | Star, Subtree t ->
          (* vacuum up all the traces associated with the subtree t and join them with trace' *)
          let trace'' = join_all_traces trace' t in
          if trace'' == trace1
          then node1
          else trace'', Star
      | Subtree t, Star ->
          (* same as above, but kind-of duplicated to allow address equality optimization *)
          let trace'' = join_all_traces trace' t in
          if trace'' == trace2
          then node2
          else trace'', Star

  let join tree1 tree2 =
    if tree1 == tree2
    then tree1
    else
      let rec node_merge node1_opt node2_opt =
        match node1_opt, node2_opt with
        | Some node1, Some node2 ->
            let joined_node = node_join node_merge TraceDomain.join node1 node2 in
            if joined_node == node1
            then node1_opt
            else if joined_node == node2
            then node2_opt
            else Some joined_node
        | None, node_opt | node_opt, None ->
            node_opt in
      BaseMap.merge (fun _ n1 n2 -> node_merge n1 n2) tree1 tree2

  let pp fmt base_tree =
    let rec pp_node fmt (trace, subtree) =
      let pp_subtree fmt = function
        | Subtree access_map -> AccessMap.pp ~pp_value:pp_node fmt access_map
        | Star -> F.fprintf fmt "*" in
      F.fprintf fmt "(%a, %a)" TraceDomain.pp trace pp_subtree subtree in
    BaseMap.pp ~pp_value:pp_node fmt base_tree
end
