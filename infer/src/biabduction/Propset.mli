(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Functions for Sets of Propositions with and without sharing *)

(** {2 Sets of Propositions} *)

[@@@warning "-unused-value-declaration"]

(** Sets of propositions. The invariant is maintaned that Prop.prop_rename_primed_footprint_vars is
    called on any prop added to the set. *)
type t

val compare : t -> t -> int
(** Compare propsets *)

val singleton : Tenv.t -> Prop.normal Prop.t -> t
(** Singleton set. *)

val mem : Prop.normal Prop.t -> t -> bool
(** Set membership. *)

val union : t -> t -> t
(** Set union. *)

val inter : t -> t -> t
(** Set intersection *)

val add : Tenv.t -> Prop.normal Prop.t -> t -> t
(** Add [prop] to propset. *)

val diff : t -> t -> t
(** Set difference. *)

val empty : t
(** The empty set of propositions. *)

val size : t -> int
(** Size of the set *)

val from_proplist : Tenv.t -> Prop.normal Prop.t list -> t

val to_proplist : t -> Prop.normal Prop.t list

val map : Tenv.t -> (Prop.normal Prop.t -> Prop.normal Prop.t) -> t -> t
(** Apply function to all the elements of the propset. *)

val map_option : Tenv.t -> (Prop.normal Prop.t -> Prop.normal Prop.t option) -> t -> t
(** Apply function to all the elements of the propset, removing those where it returns [None]. *)

val fold : ('a -> Prop.normal Prop.t -> 'a) -> 'a -> t -> 'a
(** [fold f pset a] computes [(f pN ... (f p2 (f p1 a))...)], where [p1 ... pN] are the elements of
    pset, in increasing order. *)

val iter : (Prop.normal Prop.t -> unit) -> t -> unit
(** [iter f pset] computes (f p1;f p2;..;f pN) where [p1 ... pN] are the elements of pset, in
    increasing order. *)

val partition : (Prop.normal Prop.t -> bool) -> t -> t * t

val subseteq : t -> t -> bool

val is_empty : t -> bool
(** Set emptiness check. *)

val filter : (Prop.normal Prop.t -> bool) -> t -> t

[@@@warning "+unused-value-declaration"]

(** {2 Pretty print} *)

val d : Prop.normal Prop.t -> t -> unit
(** dump a propset coming form the given initial prop *)
