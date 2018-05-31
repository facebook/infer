(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}

val compare : t -> t -> int

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val fmt : t fmt

val none : t

val mk : ?dir:string -> ?file:string -> ?col:int -> line:int -> t
