(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t [@@deriving compare, equal, yojson_of, sexp, hash]

val make : ?namespace:string -> string -> t

val classname : t -> string

val components : t -> string list

val wildcard : t

val pp : F.formatter -> t -> unit

val to_string : t -> string

val static_companion : t -> t
(** return the class of the companion class object of this class eg: Foo -> Foo$static *)

val static_companion_origin : t -> t
(** return the origin class of a companion class object eg: Foo$static -> Foo. the result is not
    specified if is the name is not a valid static class name *)

val is_static : t -> bool
(** tests if the name is a valid static class name (ie. ends with "$static") *)

val is_builtins : t -> bool
(** tests if the name of the class is $builtins where hhbc functions and models belong to *)
