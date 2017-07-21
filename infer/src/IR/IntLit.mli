(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(** signed and unsigned integer literals *)
type t

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

val of_int64_unsigned : int64 -> bool -> t

val geq : t -> t -> bool

val gt : t -> t -> bool

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

val sub : t -> t -> t

val to_int : t -> int

val to_signed : t -> t option

val to_string : t -> string
(** convert to signed if the value is representable *)

val two : t

val zero : t
