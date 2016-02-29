(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Module for call graphs *)

type in_out_calls =
  { in_calls: int; (** total number of in calls transitively *)
    out_calls: int (** total number of out calls transitively *)
  }

type t (** the type of a call graph *)

(** A call graph consists of a set of nodes (Procname.t), and edges between them.
    A node can be defined or undefined (to represent whether we have code for it).
    In an edge from [n1] to [n2], indicating that [n1] calls [n2],
    [n1] is the parent and [n2] is the child.
    Node [n1] is dependent on [n2] if there is a path from [n1] to [n2]
    using the child relationship. *)

(** [add_edge cg f t] adds an edge from [f] to [t] in the call graph [cg].
    The nodes are also added as undefined, unless already present. *)
val add_edge : t -> Procname.t -> Procname.t -> unit

(** Add a node to the call graph as defined *)
val add_defined_node : t -> Procname.t -> unit

(** Check if [source] recursively calls [dest] *)
val calls_recursively: t -> Procname.t -> Procname.t -> bool

(** Create an empty call graph *)
val create : unit -> t

(** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2];
    undefined nodes become defined if at least one side is. *)
val extend : t -> t -> unit

(** Return all the children of [n], whether defined or not *)
val get_all_children : t -> Procname.t -> Procname.Set.t

(** Compute the ancestors of the node, if not pre-computed already *)
val get_ancestors : t -> Procname.t -> Procname.Set.t

(** Compute the heirs of the node, if not pre-computed already *)
val get_heirs : t -> Procname.t -> Procname.Set.t

(** Return the in/out calls of the node *)
val get_calls : t -> Procname.t -> in_out_calls

(** Return the list of nodes which are defined *)
val get_defined_nodes : t -> Procname.t list

(** Return the children of [n] which are defined *)
val get_defined_children: t -> Procname.t -> Procname.Set.t

(** Return the nodes dependent on [n] *)
val get_dependents: t -> Procname.t -> Procname.Set.t

(** Return the number of LOC of the source file *)
val get_nLOC: t -> int

(** Return the list of nodes with calls *)
val get_nodes_and_calls : t -> (Procname.t * in_out_calls) list

(** Return all the nodes with their defined children *)
val get_nodes_and_defined_children : t -> (Procname.t * Procname.Set.t) list

(** Return the list of nodes, with defined flag, and the list of edges *)
val get_nodes_and_edges : t -> (Procname.t * bool) list * (Procname.t * Procname.t) list

(** Return the children of [n] which are not heirs of [n] and are defined *)
val get_nonrecursive_dependents : t -> Procname.t -> Procname.Set.t

(** Return the parents of [n] *)
val get_parents : t -> Procname.t -> Procname.Set.t

(** Return the ancestors of [n] which are also heirs of [n] *)
val get_recursive_dependents: t -> Procname.t -> Procname.Set.t

(** Return the path of the source file *)
val get_source : t -> DB.source_file

(** Load a call graph from a file *)
val load_from_file : DB.filename -> t option

(** Returns true if the node is defined *)
val node_defined : t -> Procname.t -> bool

(** Print the current call graph as a dotty file. If the filename is [None],
    use the current file dir inside the DB dir. *)
val save_call_graph_dotty : DB.filename option -> (Procname.t -> 'a list) -> t -> unit

(** Save a call graph into a file *)
val store_to_file : DB.filename -> t -> unit
