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
  type astate = t

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

  let initial = empty

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

  (** find all of the traces in the subtree and join them with [orig_trace] *)
  let rec join_all_traces orig_trace = function
    | Subtree subtree ->
        let join_all_traces_ orig_trace tree =
          let node_join_traces _ (trace, node) trace_acc =
            join_all_traces (TraceDomain.join trace_acc trace) node in
          AccessMap.fold node_join_traces tree orig_trace in
        join_all_traces_ orig_trace subtree
    | Star ->
        orig_trace

  (** retrieve the trace and subtree associated with [ap] from [tree] *)
  let get_node ap tree =
    let rec accesses_get_node access_list trace tree =
      match access_list, tree with
      | _, Star ->
          trace, Star
      | [], (Subtree _ as tree) ->
          trace, tree
      | access :: accesses, Subtree subtree ->
          let access_trace, access_subtree = AccessMap.find access subtree in
          accesses_get_node accesses access_trace access_subtree in
    let get_node_ base accesses tree =
      let base_trace, base_tree = BaseMap.find base tree in
      accesses_get_node accesses base_trace base_tree in
    let base, accesses = AccessPath.extract ap in
    match get_node_ base accesses tree with
    | trace, subtree ->
        if AccessPath.is_exact ap
        then Some (trace, subtree)
        else
          (* input query was [ap]*, and [trace] is the trace associated with [ap]. get the traces
             associated with the children of [ap] in [tree] and join them with [trace] *)
          Some (join_all_traces trace subtree, subtree)
    | exception Not_found ->
        None

  (** retrieve the trace associated with [ap] from [tree] *)
  let get_trace ap tree =
    Option.map fst (get_node ap tree)

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
      | Star, t ->
          (* vacuum up all the traces associated with the subtree t and join them with trace' *)
          let trace'' = join_all_traces trace' t in
          if trace'' == trace1
          then node1
          else trace'', Star
      | t, Star ->
          (* same as above, but kind-of duplicated to allow address equality optimization *)
          let trace'' = join_all_traces trace' t in
          if trace'' == trace2
          then node2
          else trace'', Star

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
        node_opt

  (* helper for [add_access]. [last_trace] is the trace associated with [tree] in the parent. *)
  let access_tree_add_trace ~node_to_add ~seen_array_access ~is_exact accesses node =
    let rec access_tree_add_trace_ ~seen_array_access accesses node =
      match accesses, node with
      | [], (trace, tree) ->
          begin
            match is_exact, seen_array_access with
            | true, false ->
                (* adding x.f, do strong update on both subtree and its traces *)
                node_to_add
            | true, true ->
                (* adding x[_], do weak update on subtree and on its immediate trace *)
                node_join node_merge TraceDomain.join node_to_add node
            | _ ->
                (* adding x.f* or x[_]*, join with traces of subtree and replace it with * *)
                let node_trace, node_tree = node_to_add in
                let trace' = join_all_traces (TraceDomain.join trace node_trace) tree in
                make_starred_leaf (join_all_traces trace' node_tree)
          end
      | _, (_, Star) ->
          node_join node_merge TraceDomain.join node_to_add node
      | access :: accesses, (trace, Subtree subtree) ->
          let access_node =
            try AccessMap.find access subtree
            with Not_found -> make_normal_leaf TraceDomain.initial in
          (* once we encounter a subtree rooted in an array access, we have to do weak updates in
             the entire subtree. the reason: if I do x[i].f.g = <interesting trace>, then
             x[j].f.g = <empty trace>, I don't want to overwrite <interesting trace>. instead, I
             should get <interesting trace> |_| <empty trace> *)
          let seen_array_access = seen_array_access || match access with
            | AccessPath.ArrayAccess _ -> true
            | AccessPath.FieldAccess _ -> false in
          let access_node' = access_tree_add_trace_ ~seen_array_access accesses access_node in
          trace, Subtree (AccessMap.add access access_node' subtree) in
    access_tree_add_trace_ ~seen_array_access accesses node

  (** add ([ap], [node]) to [tree]. *)
  let add_node ap node_to_add tree =
    let base, accesses = AccessPath.extract ap in
    let is_exact = AccessPath.is_exact ap in
    let base_node =
      try BaseMap.find base tree
      with Not_found -> make_normal_leaf TraceDomain.initial in
    let base_node' =
      access_tree_add_trace ~node_to_add ~seen_array_access:false ~is_exact accesses base_node in
    BaseMap.add base base_node' tree

  (** add [ap] to [tree] and associate its leaf node with [trace].
      if [ap] or a suffix of [ap] is not already present in the tree, it will be added with empty
      traces associated with each of the inner nodes.
      if [ap] is already present in the tree and contains no array accesses, this overwrites the
      existing trace. if [ap] does contain array accesses, it joins the existing trace with [trace].
  *)
  let add_trace ap trace tree =
    add_node ap (make_normal_leaf trace) tree

  let join tree1 tree2 =
    if tree1 == tree2
    then tree1
    else BaseMap.merge (fun _ n1 n2 -> node_merge n1 n2) tree1 tree2

  let rec access_map_fold_ f base accesses m acc =
    AccessMap.fold (fun access node acc -> node_fold_ f base (accesses @ [access]) node acc) m acc
  and node_fold_ f base accesses (trace, tree) acc =
    let cur_ap_raw = base, accesses in
    match tree with
    | Subtree access_map ->
        let acc' = f acc (AccessPath.Exact cur_ap_raw) trace in
        access_map_fold_ f base accesses access_map acc'
    | Star ->
        f acc (AccessPath.Abstracted cur_ap_raw) trace

  let node_fold (f : 'a -> AccessPath.t -> TraceDomain.astate -> 'a) base node acc =
    node_fold_ f base [] node acc

  let access_map_fold (f : 'a -> AccessPath.t -> TraceDomain.astate -> 'a) base m acc =
    access_map_fold_ f base [] m acc

  let fold (f : 'a ->  AccessPath.t -> TraceDomain.astate -> 'a) tree acc_ =
    BaseMap.fold (fun base node acc -> node_fold f base node acc) tree acc_

  (* replace the normal leaves of [node] with starred leaves *)
  let rec node_add_stars ((trace, tree) as node) = match tree with
    | Subtree subtree ->
        if AccessMap.is_empty subtree
        then make_starred_leaf trace
        else
          let subtree' = AccessMap.map node_add_stars subtree in
          if subtree' == subtree
          then node
          else trace, Subtree subtree'
    | Star -> node

  let widen ~prev ~next ~num_iters =
    if prev == next
    then prev
    else
      let trace_widen prev next =
        TraceDomain.widen ~prev ~next ~num_iters in
      let rec node_widen prev_node_opt next_node_opt =
        match prev_node_opt, next_node_opt with
        | Some prev_node, Some next_node ->
            let widened_node = node_join node_widen trace_widen prev_node next_node in
            if widened_node == prev_node
            then prev_node_opt
            else if widened_node == next_node
            then next_node_opt
            else Some widened_node
        | None, Some next_node ->
            let widened_node = node_add_stars next_node in
            if widened_node == next_node
            then next_node_opt
            else Some widened_node
        | Some _, None | None, None ->
            prev_node_opt in
      BaseMap.merge (fun _ prev_node next_node -> node_widen prev_node next_node) prev next

  let rec pp_node fmt (trace, subtree) =
    let pp_subtree fmt = function
      | Subtree access_map -> AccessMap.pp ~pp_value:pp_node fmt access_map
      | Star -> F.fprintf fmt "*" in
    F.fprintf fmt "(%a, %a)" TraceDomain.pp trace pp_subtree subtree

  let pp fmt base_tree =
    BaseMap.pp ~pp_value:pp_node fmt base_tree
end
