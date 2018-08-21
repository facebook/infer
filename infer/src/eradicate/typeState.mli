(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for typestates: maps from expressions to annotated types, with extensions. *)

(** Typestate *)
type t

type range = Typ.t * TypeAnnotation.t * Location.t list

val add_id : Ident.t -> range -> t -> t

val add : Pvar.t -> range -> t -> t

val empty : t

val equal : t -> t -> bool

val join : t -> t -> t

val lookup_id : Ident.t -> t -> range option

val lookup_pvar : Pvar.t -> t -> range option

val pp : Format.formatter -> t -> unit

val range_add_locs : range -> Location.t list -> range

val remove_id : Ident.t -> t -> t
