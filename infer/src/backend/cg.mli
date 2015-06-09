(*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*)

(** Module for call graphs *)

open Utils

type in_out_calls =
  { in_calls: int; (** total number of in calls transitively *)
    out_calls: int (** total number of out calls transitively *)
  }

type t (** the type of a call graph *)

(** A call graph consists of a set of nodes (Procname.t), and edges between them.
A node can be defined or undefined (to represent whether we have code for it).
In an edge from [n1] to [n2], indicating that [n1] calls [n2], [n1] is the parent and [n2] is the child.
Node [n1] is dependent on [n2] if there is a path from [n1] to [n2] using the child relationship. *)

(** [add_edge cg f t] adds an edge from [f] to [t] in the call graph [cg]. The nodes are also added as undefined, unless already present. *)
val add_edge : t -> Procname.t -> Procname.t -> unit

val add_node : t -> Procname.t -> unit (** Add a node to the call graph as defined *)
val calls_recursively: t -> Procname.t -> Procname.t -> bool (** [calls_recursively g source dest] returns [true] if function [source] recursively calls function [dest] *)
val create : unit -> t (** Create an empty call graph *)
val extend : t -> t -> unit (** [extend cg1 gc2] extends [cg1] in place with nodes and edges from [gc2]; undefined nodes become defined if at least one side is. *)
val get_all_children : t -> Procname.t -> Procname.Set.t (** Return all the children of [n], whether defined or not *)
val get_ancestors : t -> Procname.t -> Procname.Set.t (** Compute the ancestors of the node, if not pre-computed already *)
val get_calls : t -> Procname.t -> in_out_calls (** Return the in/out calls of the node *)
val get_defined_nodes : t -> Procname.t list (** Return the list of nodes which are defined *)
val get_defined_children: t -> Procname.t -> Procname.Set.t (** Return the children of [n] which are defined *)
val get_dependents: t -> Procname.t -> Procname.Set.t (** Return the nodes dependent on [n] *)
val get_nLOC: t -> int (** Return the number of LOC of the source file *)
val get_nodes_and_calls : t -> (Procname.t * in_out_calls) list (** Return the list of nodes with calls *)
val get_nodes_and_defined_children : t -> (Procname.t * Procname.Set.t) list (** Return all the nodes with their defined children *)
val get_nodes_and_edges : t -> (Procname.t * bool) list * (Procname.t * Procname.t) list (** Return the list of nodes, with defined flag, and the list of edges *)
val get_nonrecursive_dependents : t -> Procname.t -> Procname.Set.t (** Return the children of [n] which are not heirs of [n] and are defined *)
val get_parents : t -> Procname.t -> Procname.Set.t (** Return the parents of [n] *)
val get_recursive_dependents: t -> Procname.t -> Procname.Set.t (** Return the ancestors of [n] which are also heirs of [n] *)
val get_source : t -> DB.source_file (** Return the path of the source file *)
val load_from_file : DB.filename -> t option (** Load a call graph from a file *)
val node_defined : t -> Procname.t -> bool (** Returns true if the node is defined *)
val restrict_defined : t -> Procname.Set.t option -> unit  (** if not None, restrict defined nodes to the given set *)
val save_call_graph_dotty : DB.filename option -> (Procname.t -> 'a list) -> t -> unit (** Print the current call graph as a dotty file. If the filename is [None], use the current file dir inside the DB dir. *)
val store_to_file : DB.filename -> t -> unit (** Save a call graph into a file *)
