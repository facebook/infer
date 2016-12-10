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

(** Functions for Sets of Propositions with and without sharing *)

(** {2 Sets of Propositions} *)

(** Sets of propositions.
    The invariant is maintaned that Prop.prop_rename_primed_footprint_vars is called on any prop added to the set. *)
type t

(** Compare propsets *)
val compare : t -> t -> int

(** Singleton set. *)
val singleton : Tenv.t -> Prop.normal Prop.t -> t

(** Set membership. *)
val mem : Prop.normal Prop.t -> t -> bool

(** Set union. *)
val union : t -> t -> t

(** Set intersection *)
val inter : t -> t -> t

(** Add [prop] to propset. *)
val add : Tenv.t -> Prop.normal Prop.t -> t -> t

(** Set difference. *)
val diff : t -> t -> t

(** The empty set of propositions. *)
val empty : t

(** Size of the set *)
val size : t -> int

val from_proplist : Tenv.t -> Prop.normal Prop.t list -> t

val to_proplist : t -> Prop.normal Prop.t list

(** Apply function to all the elements of the propset. *)
val map : Tenv.t -> (Prop.normal Prop.t -> Prop.normal Prop.t) -> t -> t

(** Apply function to all the elements of the propset, removing those where it returns [None]. *)
val map_option : Tenv.t -> (Prop.normal Prop.t -> Prop.normal Prop.t option) -> t -> t

(** [fold f pset a] computes [(f pN ... (f p2 (f p1 a))...)],
    where [p1 ... pN] are the elements of pset, in increasing
    order. *)
val fold : ('a -> Prop.normal Prop.t -> 'a) -> 'a -> t -> 'a

(** [iter f pset] computes (f p1;f p2;..;f pN)
    where [p1 ... pN] are the elements of pset, in increasing order. *)
val iter : (Prop.normal Prop.t -> unit) -> t -> unit

val partition : (Prop.normal Prop.t -> bool) -> t -> t * t

val subseteq : t -> t -> bool

(** Set emptiness check. *)
val is_empty : t -> bool

val filter : (Prop.normal Prop.t -> bool) -> t -> t

(** {2 Pretty print} *)

(** Pretty print a set of propositions, obtained from the given prop. *)
val pp : Pp.env -> Prop.normal Prop.t -> Format.formatter -> t -> unit

(** dump a propset coming form the given initial prop *)
val d : Prop.normal Prop.t -> t -> unit
