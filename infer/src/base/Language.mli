(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = Clang | Java | Python [@@deriving compare]

val equal : t -> t -> bool

val to_string : t -> string

val to_explicit_string : t -> string

val of_string : string -> t option

val curr_language : t ref

val curr_language_is : t -> bool
