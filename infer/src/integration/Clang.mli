(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type compiler = Clang | Make [@@deriving compare]

val capture : compiler -> prog:string -> args:string list -> unit
