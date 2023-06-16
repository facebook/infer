(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

module Node : sig
  type t = private {id: int; pname: Procname.t; mutable successors: int list; mutable flag: bool}

  val set_flag : t -> unit
end

type t

val reset : t -> unit
(** empty the graph and shrink it to its initial size *)

val create : int -> t
(** [create n] makes an empty graph with initial capacity [n] which grows as required *)

val n_procs : t -> int
(** number of procedures in graph *)

val mem : t -> int -> bool
(** is an int [id] the index of a node in the graph? *)

val mem_procname : t -> Procname.t -> bool
(** is there a node for [procname] in the graph? *)

val flag_reachable : t -> Procname.t -> unit
(** flag all nodes reachable from the node of the given procname, if it exists *)

val iter_unflagged_leaves : f:(Node.t -> unit) -> t -> unit
(** iterate over all leaves that have their flag set to false *)

val remove : t -> Procname.t -> unit

val to_dotty : t -> ResultsDirEntryName.id -> unit
(** output call graph in dotty format with the given filename in results dir *)

val add_edge : t -> Procname.t -> successor:Procname.t -> unit
(** add an edge from the proc name to [successor] in the graph, creating a node for the proc name if
    there isn't one already *)

val create_node : t -> Procname.t -> Procname.t list -> unit
(** create a new node with edges from [pname] to [successor_pnames] in the graph *)

val create_node_with_id : t -> id:int -> Procname.t -> successors:int list -> unit

val node_of_id : t -> int -> Node.t option

val fold_flagged : t -> f:(Node.t -> 'a -> 'a) -> 'a -> 'a
(** perform a fold over the nodes in the graph with flag set to true *)

val default_initial_capacity : int
(** reasonable minimum capacity for the graph that is prime *)
