(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** IArray - Immutable view of an array

    Note that an iarray can be constructed from an array without copying,
    and the client might keep other references to the backing array and use
    them to later modify it. So IArray is not a safe immutable data
    structure, it only attempts to make it inconvenient to unintentionally
    mutate. *)

open Import0

include
  module type of Array.Permissioned
    with type ('a, 'p) t := ('a, 'p) Array.Permissioned.t

type 'a t = ('a, immutable) Array.Permissioned.t
[@@deriving compare, equal, hash, sexp]

module Import : sig
  type 'a iarray = 'a t [@@deriving compare, equal, hash, sexp]
end

val pp : (unit, unit) fmt -> 'a pp -> 'a t pp
val empty : 'a t
val of_ : 'a -> 'a t

val of_array : 'a array -> 'a t
(** [of_array a] is an iarray that shares its representation with array [a],
    therefore mutating [a] changes [of_array a]. The intended use for
    [of_array] is for converting an array to an iarray when the array will
    not be used after conversion, or with care for multi-step initialization
    of an iarray. *)

val contains_dup : compare:('a -> 'a -> int) -> 'a t -> bool

val fold_map :
  'a t -> init:'accum -> f:('accum -> 'a -> 'accum * 'b) -> 'accum * 'b t

val fold_map_until :
     'a t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum * 'b, 'final) Continue_or_stop.t)
  -> finish:('accum * 'b t -> 'final)
  -> 'final

val map_preserving_phys_equal : 'a t -> f:('a -> 'a) -> 'a t
(** Like map, but preserves [phys_equal] if [f] preserves [phys_equal] of
    every element. *)

val combine_adjacent : f:('a -> 'a -> 'a option) -> 'a t -> 'a t
