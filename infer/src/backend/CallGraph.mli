(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module type NodeSig = sig
  type t = private {id: int; pname: Typ.Procname.t; mutable successors: int list; mutable flag: bool}

  val make : int -> Typ.Procname.t -> int list -> t

  val add_successor : t -> int -> unit

  val set_flag : t -> unit

  val unset_flag : t -> unit

  val pp_dot : F.formatter -> t -> unit
end

module Node : NodeSig

type t

val reset : t -> unit
(** empty the graph and shrink it to its initial size *)

val create : int -> t
(** [create n] makes an empty graph with initial capacity [n] which grows as required *)

val n_procs : t -> int
(** number of procedures in graph *)

val mem : t -> int -> bool
(** is an int [id] the index of a node in the graph? *)

val flag : t -> Typ.Procname.t -> unit

val flag_reachable : t -> Typ.Procname.t -> unit
(** flag all nodes reachable from the node of the given procname, if it exists *)

val get_unflagged_leaves : t -> Node.t list
(** get all leaves that have their flag set to false *)

val remove : t -> Typ.Procname.t -> unit

val to_dotty : t -> string -> unit
(** output call graph in dotty format with the given filename in results dir *)

val trim_id_map : t -> unit
(** remove all pnames that do not correspond to a defined procedure from id_map *)

val remove_unflagged_and_unflag_all : t -> unit
(** remove all nodes with flag set to false, and set flag to false on all remaining nodes *)

val add_edge : t -> pname:Typ.Procname.t -> successor_pname:Typ.Procname.t -> unit
(** add an edge from [pname] to [successor_pname] in the graph, creating a node for [pname] if there
    isn't one already *)

val create_node : t -> Typ.Procname.t -> Typ.Procname.t list -> unit
(** create a new node with edges from [pname] to [successor_pnames] in the graph *)

val fold_flagged : t -> f:(Node.t -> 'a -> 'a) -> 'a -> 'a
(** perform a fold over the nodes in the graph with flag set to true *)

val default_initial_capacity : int
(** reasonable minimum capacity for the graph that is prime *)
