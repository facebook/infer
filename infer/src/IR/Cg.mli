(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Module for call graphs *)

(** the type of a call graph *)
type t

(** A call graph consists of a set of nodes (Typ.Procname.t), and edges between them.
    A node can be defined or undefined (to represent whether we have code for it).
    In an edge from [n1] to [n2], indicating that [n1] calls [n2],
    [n1] is the parent and [n2] is the child.
    Node [n1] is dependent on [n2] if there is a path from [n1] to [n2]
    using the child relationship. *)

val add_edge : t -> Typ.Procname.t -> Typ.Procname.t -> unit
(** [add_edge cg f t] adds an edge from [f] to [t] in the call graph [cg].
    The nodes are also added as undefined, unless already present. *)

val add_defined_node : t -> Typ.Procname.t -> unit
(** Add a node to the call graph as defined *)

val create : SourceFile.t -> t
(** Create an empty call graph *)

val extend : t -> t -> unit
(** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2];
    undefined nodes become defined if at least one side is. *)

val get_defined_nodes : t -> Typ.Procname.t list
(** Return the list of nodes which are defined *)

val load_from_file : DB.filename -> t option
(** Load a call graph from a file *)

val remove_node_defined : t -> Typ.Procname.t -> unit
(** Remove the defined flag from a node, if it exists. *)

val save_call_graph_dotty : SourceFile.t -> t -> unit
(** Print the call graph as a dotty file. *)

val store_to_file : DB.filename -> t -> unit
(** Save a call graph into a file *)
