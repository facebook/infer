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

(** Module for Mangled Names *)

(** Type of mangled names *)

type t [@@deriving compare]

(** Equality for mangled names *)

val equal : t -> t -> bool

(** Convert a string to a mangled name *)

val from_string : string -> t

(** Create a mangled name from a plain and mangled string *)

val mangled : string -> string -> t

(** Convert a mangled name to a string *)

val to_string : t -> string

(** Convert a full mangled name to a string *)

val to_string_full : t -> string

(** Get mangled string if given *)

val get_mangled : t -> string

(** Pretty print a mangled name *)

val pp : Format.formatter -> t -> unit

(** Set of Mangled. *)

module Set : Caml.Set.S with type elt = t

(** Map with Mangled as key *)

module Map : Caml.Map.S with type key = t
