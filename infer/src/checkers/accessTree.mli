(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** tree of (trace, access path) associations organized by structure of access paths *)
module type S = sig
  module TraceDomain : AbstractDomain.WithBottom

  module AccessMap : PrettyPrintable.PPMap with type key = AccessPath.access

  module BaseMap = AccessPath.BaseMap

  type node = TraceDomain.t * tree

  and tree =
    | Subtree of node AccessMap.t
        (** map from access -> nodes. a leaf is encoded as an empty map *)
    | Star  (** special leaf for starred access paths *)

  (** map from base var -> access subtree. Here's how to represent a few different kinds of
      trace * access path associations: {[
      (x, T)               := { x |-> (T, Subtree {}) }
      (x.f, T)             := { x |-> (empty, Subtree { f |-> (T, Subtree {}) }) }
      (x*, T)              := { x |-> (T, Star) }
      (x.f*, T)            := { x |-> (empty, Subtree { f |-> (T, Star) }) }
      (x, T1), (y, T2)     := { x |-> (T1, Subtree {}), y |-> (T2, Subtree {}) }
      (x.f, T1), (x.g, T2) := { x |-> (empty, Subtree { f |-> (T1, Subtree {}),
                                                        g |-> (T2, Subtree {}) }) }
      ]}
  *)

  include AbstractDomain.WithBottom with type t = node BaseMap.t

  val empty_node : node

  val make_node : TraceDomain.t -> node AccessMap.t -> node

  val make_access_node : TraceDomain.t -> AccessPath.access -> TraceDomain.t -> node
  (** for testing only *)

  val make_normal_leaf : TraceDomain.t -> node
  (** create a leaf node with no successors *)

  val make_starred_leaf : TraceDomain.t -> node
  (** create a leaf node with a wildcard successor *)

  val get_node : AccessPath.Abs.t -> t -> node option
  (** retrieve the node associated with the given access path *)

  val get_trace : AccessPath.Abs.t -> t -> TraceDomain.t option
  (** retrieve the trace associated with the given access path *)

  val add_node : AccessPath.Abs.t -> node -> t -> t
  (** add the given access path to the tree and associate its last access with with the given node.
      if any of the accesses in the path are not already present in the tree, they will be added
      with with empty traces associated with each of the inner nodes. *)

  val add_trace : AccessPath.Abs.t -> TraceDomain.t -> t -> t
  (** add the given access path to the tree and associate its last access with with the given trace.
      if any of the accesses in the path are not already present in the tree, they will be added
      with with empty traces associated with each of the inner nodes. *)

  val node_join : node -> node -> node
  (** join two nodes *)

  val fold : ('a -> AccessPath.Abs.t -> node -> 'a) -> t -> 'a -> 'a
  (** apply a function to each (access path, node) pair in the tree. *)

  val trace_fold : ('a -> AccessPath.Abs.t -> TraceDomain.t -> 'a) -> t -> 'a -> 'a

  val exists : (AccessPath.Abs.t -> node -> bool) -> t -> bool

  val iter : (AccessPath.Abs.t -> node -> unit) -> t -> unit

  val depth : t -> int
  (** number of traces in the tallest branch of the tree *)

  val pp_node : Format.formatter -> node -> unit
end

module type Config = sig
  val max_depth : int

  val max_width : int
end

module DefaultConfig : Config

module Make (TraceDomain : AbstractDomain.WithBottom) (Config : Config) :
  S with module TraceDomain = TraceDomain

(** Concise representation of a set of access paths *)
module PathSet (Config : Config) : sig
  include module type of Make (AbstractDomain.BooleanOr) (Config)

  val mem : AccessPath.Abs.t -> t -> bool
end
