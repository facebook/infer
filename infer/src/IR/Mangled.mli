(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module for Mangled Names *)

(** Type of mangled names *)
type t [@@deriving compare]

val equal : t -> t -> bool
(** Equality for mangled names *)

val from_string : string -> t
(** Convert a string to a mangled name *)

val mangled : string -> string -> t
(** Create a mangled name from a plain and mangled string *)

val to_string : t -> string
(** Convert a mangled name to a string *)

val to_string_full : t -> string
(** Convert a full mangled name to a string *)

val pp : Format.formatter -> t -> unit
(** Pretty print a mangled name *)

val this : t

val is_this : t -> bool

val self : t [@@warning "-32"]

val is_self : t -> bool

val rename : f:(string -> string) -> t -> t
(** Maps over both the plain and the mangled components. *)

module Set : Caml.Set.S with type elt = t
(** Set of Mangled. *)

module Map : Caml.Map.S with type key = t
(** Map with Mangled as key *)
