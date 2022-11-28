(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* OCaml needs to know not only that this has the same interface as [Z] but also that the types it
   defines are, in fact, the same as [Z] *)
include module type of struct
  include Z
end

val protect : ('a -> 'b) -> 'a -> 'b option
(** [None] instead of throwing [Division_by_zero | Invalid_argument _ | Z.Overflow] *)

val yojson_of_t : [%yojson_of: t]

(* the functions below shadow definitions in [Z] to give them safer types *)
[@@@warning "-unused-value-declaration"]

val div : t -> t -> t option

val rem : t -> t -> t option

val div_rem : t -> t -> (t * t) option

val cdiv : t -> t -> t option

val fdiv : t -> t -> t option

val ediv_rem : t -> t -> (t * t) option

val ediv : t -> t -> t option

val erem : t -> t -> t option

val divexact : t -> t -> t option

val gcd : t -> t -> t option

val gcdext : t -> t -> (t * t * t) option

val lcm : t -> t -> t option

val powm : t -> t -> (t -> t) option

val powm_sec : t -> t -> (t -> t) option

val invert : t -> t -> t option

val ( / ) : t -> t -> t option

val ( /> ) : t -> t -> t option

val ( /< ) : t -> t -> t option

val ( /| ) : t -> t -> t option

val ( mod ) : t -> t -> t option
