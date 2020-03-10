(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}
[@@deriving compare, equal, hash, sexp]

val pp : t pp
val none : t
val is_none : t -> bool
val mk : ?dir:string -> ?file:string -> ?col:int -> line:int -> t
