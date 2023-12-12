(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val make : package:string option -> classname:string -> t
(** [make ~package:(Some "java.lang") "Object"] creates a value representing [java.lang.Object] *)

val from_string : string -> t
(** [from_string "java.lang.Object"] is same as [make ~package:(Some "java.lang") "Object"] *)

val to_string : t -> string
(** [to_string (from_string "X.Y.Z") = "X.Y.Z"] *)

val pp : Format.formatter -> t -> unit
(** [pp] includes package if any *)

val pp_with_verbosity : verbose:bool -> Format.formatter -> t -> unit
(** if [verbose] then print package if present, otherwise only print class *)

val package : t -> string option

val classname : t -> string

val get_outer_class_name : t -> t option
(** If this is an inner class, return the closest outer, e.g. A$B for A$B$C. None if the class is
    outermost *)

val is_anonymous_inner_class_name : t -> bool
(** True if it is either "classic" anonymous Java class:
    https://docs.oracle.com/javase/tutorial/java/javaOO/anonymousclasses.html, or a synthetic Java
    class corresponding to a lambda expression. *)

val get_user_defined_class_if_anonymous_inner : t -> t option
(** If the current class is anonymous ([is_anonymous_inner_class_name] is true), return the
    corresponding user defined (not anonymous) class this anonymous class belongs to.

    In general case, BOTH anonymous classes and user-defined classes can be nested:
    SomeClass$NestedClass$1$17$5. In this example, we should return SomeClass$NestedClass.

    If this is not an anonymous class, returns [None]. *)
