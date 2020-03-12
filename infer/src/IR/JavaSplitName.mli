(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Module for representing a Java type. *)

type t [@@deriving compare, equal]

val make : ?package:string -> string -> t

val of_string : string -> t
(** Given a package.type_name string, it looks for the latest dot and splits the string in two
    (package, rest-of-type-name). If there is no dot symbol, assumes the whole string is a type
    name. NB the RHS is not just a classname, eg it can be an array type. *)

val java_lang_object : t
(** [java.lang.Object] type *)

val java_lang_object_array : t
(** [java.lang.Object\[\]] type *)

val java_lang_string : t
(** [java.lang.String] type *)

val void : t
(** Java [void] type *)

val package : t -> string option

val type_name : t -> string

val pp_type_verbosity : verbose:bool -> F.formatter -> t -> unit
