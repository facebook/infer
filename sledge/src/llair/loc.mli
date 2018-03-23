(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Source code debug locations *)

type t = {dir: string; file: string; line: int; col: int}

val compare : t -> t -> int

val t_of_sexp : Sexp.t -> t

val sexp_of_t : t -> Sexp.t

val fmt : t fmt

val none : t

val mk : ?dir:string -> ?file:string -> ?col:int -> line:int -> t
