(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val compare : ?debug:bool -> string -> string -> bool

val semdiff : ?debug:bool -> string -> string -> unit
