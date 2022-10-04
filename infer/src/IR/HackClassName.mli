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

val pp : F.formatter -> t -> unit

val to_string : t -> string
