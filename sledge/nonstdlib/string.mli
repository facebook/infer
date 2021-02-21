(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of ContainersLabels.String

type t = string [@@deriving compare, equal, hash, sexp]

val index : string -> char -> int option
val index_exn : string -> char -> int
val index_from : string -> int -> char -> int option
val index_from_exn : string -> int -> char -> int
val rindex : string -> char -> int option
val rindex_exn : string -> char -> int
val rindex_from : string -> int -> char -> int option
val rindex_from_exn : string -> int -> char -> int
val rtake : int -> string -> string
val rdrop : int -> string -> string
val rtake_drop : int -> string -> string * string

module Set : NSSet.S with type elt = string
module Map : NSMap.S with type key = string
module Tbl : HashTable.S with type key = string
