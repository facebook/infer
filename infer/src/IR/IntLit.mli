(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** signed and unsigned integer literals *)
type t

exception OversizedShift

val add : t -> t -> t

val compare : t -> t -> int
(** compare integers ignoring the distinction between pointers and non-pointers *)

val compare_value : t -> t -> int
(** compare the value of the integers, notice this is different from const compare,
    which distinguished between signed and unsigned +1 *)

val div : t -> t -> t

val eq : t -> t -> bool

val of_int : int -> t

val of_int32 : int32 -> t

val of_int64 : int64 -> t

val of_string : string -> t

val geq : t -> t -> bool

val gt : t -> t -> bool [@@warning "-32"]

val isminusone : t -> bool

val isnegative : t -> bool

val isnull : t -> bool

val isone : t -> bool

val iszero : t -> bool

val leq : t -> t -> bool

val logand : t -> t -> t

val lognot : t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val lt : t -> t -> bool

val minus_one : t

val mul : t -> t -> t

val neg : t -> t

val neq : t -> t -> bool

val null : t
(** null behaves like zero except for the function isnull *)

val one : t

val pp : F.formatter -> t -> unit

val rem : t -> t -> t

val shift_left : t -> t -> t

(* shift_right performs arithmetic shift, for the following reasons: *)
(* In C++, whether right shift is logical or arithmetical is implementation defined.
 * Usually an arithmetic shift is implemented. *)
(* In Java, the current frontend refuses to translate logical right shift. *)

val shift_right : t -> t -> t

val sub : t -> t -> t

val to_int : t -> int option

val to_int_exn : t -> int

val to_big_int : t -> Z.t

val to_float : t -> float

val to_signed : t -> t option

val to_string : t -> string
(** convert to signed if the value is representable *)

val two : t

val zero : t
