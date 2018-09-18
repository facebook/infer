(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Clang | Java [@@deriving compare]

val equal : t -> t -> bool

val to_string : t -> string

val to_explicit_string : t -> string

val of_string : string -> t option

val curr_language : t ref

val curr_language_is : t -> bool
