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
end
