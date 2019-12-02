(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Propositions seen as graphs *)

(** prop considered as a graph *)
type 'a t

val from_prop : 'a Prop.t -> 'a t
(** create a graph from a prop *)

(** Graph annotated with the differences w.r.t. a previous graph *)
type 'a diff

val compute_diff : Pp.color -> 'a t -> 'a t -> 'a diff
(** [compute_diff default_color oldgraph newgraph] returns the list of edges which are only in
    [newgraph] *)

val diff_get_colormap : bool -> 'a diff -> Pp.colormap
(** [diff_get_colormap footprint_part diff] returns the colormap of a computed diff, selecting the
    footprint colormap if [footprint_part] is true. *)

val pp_proplist : Pp.env -> string -> 'a Prop.t * bool -> Format.formatter -> 'b Prop.t list -> unit
(** Print a list of propositions, prepending each one with the given string, If
    !Config.pring_using_diff is true, print the diff w.r.t. the given prop, extracting its local
    stack vars if the boolean is true. *)

val d_proplist : 'a Prop.t -> 'b Prop.t list -> unit
(** dump a prop list coming form the given initial prop *)
