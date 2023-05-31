(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Clang | CIL | Erlang | Hack | Java | Python [@@deriving compare, enumerate]

val equal : t -> t -> bool

val to_string : t -> string

val curr_language : t ref

val curr_language_is : t -> bool
