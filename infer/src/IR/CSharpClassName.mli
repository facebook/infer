(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t [@@deriving compare, equal, yojson_of]

module Map : Caml.Map.S with type key = t

module Set : Caml.Set.S with type elt = t

val make : namespace:string option -> classname:string -> t

val from_string : string -> t

val to_string : t -> string
(** [to_string (from_string "X.Y.Z") = "X.Y.Z"] *)

val pp : Format.formatter -> t -> unit
(** [pp] includes namespace if any *)

val pp_with_verbosity : verbose:bool -> Format.formatter -> t -> unit
(** if [verbose] then print namespace if present, otherwise only print class *)

val namespace : t -> string option

val classname : t -> string

val is_external_via_config : t -> bool
(** Considered external based on config flags. *)

val get_outer_class_name : t -> t option
(** If this is an inner class, return the closest outer, e.g. A$B for A$B$C. None if the class is
    outermost *)

val is_anonymous_inner_class_name : t -> bool
(** True if it is either "classic" anonymous csharp class: *)

val get_user_defined_class_if_anonymous_inner : t -> t option
(** If the current class is anonymous ([is_anonymous_inner_class_name] is true), return the
    corresponding user defined (not anonymous) class this anonymous class belongs to.

    In general case, BOTH anonymous classes and user-defined classes can be nested:
    SomeClass$NestedClass$1$17$5. In this example, we should return SomeClass$NestedClass.

    If this is not an anonymous class, returns [None]. *)

module Normalizer : HashNormalizer.S with type t = t
