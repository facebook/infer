(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Names for fields of class/struct/union *)
type t [@@deriving compare, equal]

val make : Typ.Name.t -> string -> t
(** create a field of the given class and fieldname *)

val get_class_name : t -> Typ.Name.t

val get_field_name : t -> string

val is_java : t -> bool

module Set : Caml.Set.S with type elt = t
(** Set for fieldnames *)

module Map : Caml.Map.S with type key = t
(** Map for fieldnames *)

val is_java_outer_instance : t -> bool
(** Check if the field is the synthetic this$n of a nested class, used to access the n-th outer
    instance. *)

val to_string : t -> string
(** Convert a field name to a string. *)

val to_full_string : t -> string

val to_simplified_string : t -> string
(** Convert a fieldname to a simplified string with at most one-level path. *)

val pp : F.formatter -> t -> unit
(** Pretty print a field name. *)
