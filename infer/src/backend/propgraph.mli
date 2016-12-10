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

(** Propositions seen as graphs *)

type t (** prop considered as a graph *)

type node (** node of the graph *)

type edge (** multi-edge: one source and many destinations *)

(** create a graph from a prop *)
val from_prop : Prop.normal Prop.t -> t

(** Return [true] if root node *)
val is_root : node -> bool

(** Return [true] if the nodes are connected. Used to compute reachability. *)
val nodes_connected : node -> node -> bool

(** Return the source of the edge *)
val edge_get_source : edge -> node option

(** Return the successor nodes of the edge *)
val edge_get_succs : edge -> node list

(** [edge_from_source g n footprint_part is_hpred] finds and edge
    with the given source [n] in prop [g].
    [footprint_part] indicates whether to search the edge in the footprint part,
    and [is_pred] whether it is an hpred edge. *)
val edge_from_source : t -> node -> bool -> bool -> edge option

(** [get_succs g n footprint_part is_hpred] returns the successor nodes of [n] in [g].
    [footprint_part] indicates whether to search the successors in the footprint part,
    and [is_pred] whether to follow hpred edges. *)
val get_succs : t -> node -> bool -> bool -> node list

(** [get_edges footprint_part g] returns the list of edges in [g],
    in the footprint part if [fotprint_part] is true *)
val get_edges : bool -> t -> edge list

(** [contains_edge footprint_part g e] returns true if the graph [g] contains edge [e],
    searching the footprint part if [footprint_part] is true. *)
val contains_edge : bool -> t -> edge -> bool

(** [iter_edges footprint_part f g] iterates function [f] on the edges in [g]
    in the same order as returned by [get_edges];
    if [footprint_part] is true the edges are taken from the footprint part. *)
val iter_edges : bool -> (edge -> unit) -> t -> unit

(** Graph annotated with the differences w.r.t. a previous graph *)
type diff

(** [compute_diff default_color oldgraph newgraph] returns the list of edges
    which are only in [newgraph] *)
val compute_diff : Pp.color -> t -> t -> diff

(** [diff_get_colormap footprint_part diff] returns the colormap of a computed diff,
    selecting the footprint colormap if [footprint_part] is true. *)
val diff_get_colormap : bool -> diff -> Pp.colormap

(** Print a list of propositions, prepending each one with the given string,
    If !Config.pring_using_diff is true, print the diff w.r.t. the given prop,
    extracting its local stack vars if the boolean is true. *)
val pp_proplist :
  Pp.env -> string -> (Prop.normal Prop.t * bool) ->
  Format.formatter -> Prop.normal Prop.t list -> unit

(** dump a prop list coming form the given initial prop *)
val d_proplist : 'a Prop.t -> 'b Prop.t list -> unit
