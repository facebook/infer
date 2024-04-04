(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare, equal, yojson_of, sexp, hash, normalize]

val make : string -> t

val classname : t -> string

val components : t -> string list

val wildcard : t [@@warning "-unused-value-declaration"]

val pp : F.formatter -> t -> unit

val to_string : t -> string

val static_companion : t -> t
[@@warning "-unused-value-declaration"]
(** return the class of the companion class object of this class eg: Foo -> Foo$static *)

val static_companion_origin : t -> t
[@@warning "-unused-value-declaration"]
(** return the origin class of a companion class object eg: Foo$static -> Foo. the result is not
    specified if is the name is not a valid static class name *)

val is_static : t -> bool
[@@warning "-unused-value-declaration"]
(** tests if the name is a valid static class name (ie. ends with "$static") *)
