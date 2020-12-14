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

open! NS0

type 'a t [@@deriving compare, equal, hash, sexp]

module Import : sig
  type 'a iarray = 'a t [@@deriving compare, equal, hash, sexp]
end

val pp : (unit, unit) fmt -> 'a pp -> 'a t pp

val to_array : 'a t -> 'a array
(** [to_array v] is the array backing [v], no copy is performed. *)

val of_array : 'a array -> 'a t
(** [of_array a] is an iarray that shares its representation with array [a],
    therefore mutating [a] changes [of_array a]. The intended use for
    [of_array] is for converting an array to an iarray when the array will
    not be used after conversion, or with care for multi-step initialization
    of an iarray. *)

val empty : 'a t
val of_ : 'a -> 'a t
val of_iter : 'a iter -> 'a t
val of_list : 'a list -> 'a t
val of_list_rev : 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val sub : 'a t -> pos:int -> len:int -> 'a t
val concat : 'a t list -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like map, but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val reduce_adjacent : 'a t -> f:('a -> 'a -> 'a option) -> 'a t
val split : ('a * 'b) t -> 'a t * 'b t
val combine : 'a t -> 'b t -> ('a * 'b) t option
val combine_exn : 'a t -> 'b t -> ('a * 'b) t
val is_empty : 'a t -> bool
val length : 'a t -> int
val get : 'a t -> int -> 'a
val to_iter : 'a t -> 'a iter
val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
val fold : 'a t -> 's -> f:('a -> 's -> 's) -> 's
val fold_right : 'a t -> 's -> f:('a -> 's -> 's) -> 's
val fold_map : 'a t -> 's -> f:('a -> 's -> 'b * 's) -> 'b t * 's

val fold_map_until :
     'a t
  -> 's
  -> f:('a -> 's -> [`Continue of 'b * 's | `Stop of 'c])
  -> finish:('b t * 's -> 'c)
  -> 'c

val fold2_exn : 'a t -> 'b t -> 's -> f:('a -> 'b -> 's -> 's) -> 's
