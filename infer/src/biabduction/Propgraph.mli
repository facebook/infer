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

(** prop considered as a graph *)
type t

val from_prop : Prop.normal Prop.t -> t
(** create a graph from a prop *)

(** Graph annotated with the differences w.r.t. a previous graph *)
type diff

val compute_diff : Pp.color -> t -> t -> diff
(** [compute_diff default_color oldgraph newgraph] returns the list of edges
    which are only in [newgraph] *)

val diff_get_colormap : bool -> diff -> Pp.colormap
(** [diff_get_colormap footprint_part diff] returns the colormap of a computed diff,
    selecting the footprint colormap if [footprint_part] is true. *)

val pp_proplist :
  Pp.env -> string -> Prop.normal Prop.t * bool -> Format.formatter -> Prop.normal Prop.t list
  -> unit
(** Print a list of propositions, prepending each one with the given string,
    If !Config.pring_using_diff is true, print the diff w.r.t. the given prop,
    extracting its local stack vars if the boolean is true. *)

val d_proplist : 'a Prop.t -> 'b Prop.t list -> unit
(** dump a prop list coming form the given initial prop *)
