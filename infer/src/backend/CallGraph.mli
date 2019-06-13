(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module type NodeSig = sig
  type t = private {id: int; pname: Typ.Procname.t; successors: int list; mutable flag: bool}

  val make : int -> Typ.Procname.t -> int list -> t

  val set_flag : t -> unit

  val unset_flag : t -> unit

  val pp_dot : F.formatter -> t -> unit
end

module Node : NodeSig

type t

val clear : t -> unit

val create : int -> t
(** [create n] makes an empty graph with initial capacity [n] which grows as required *)

val n_procs : t -> int
(** number of procedures in graph *)

val build_from_sources : t -> SourceFile.t list -> unit
(** build restriction of call graph to procedures reachable from provided sources *)

val mem : t -> int -> bool
(** is an int [id] the index of a node in the graph? *)

val flag_reachable : t -> Typ.Procname.t -> unit
(** flag all nodes reachable from the node of the given procname, if it exists *)

val remove_reachable : t -> Typ.Procname.t -> unit
(** remove all nodes reachable from procname *)

val get_unflagged_leaves : t -> Node.t list

val to_dotty : t -> string -> unit
(** output call graph in dotty format with the given filename in results dir *)
