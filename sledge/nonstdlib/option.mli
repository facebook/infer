(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of Containers.Option

type 'a t = 'a option [@@deriving compare, equal, hash, sexp]

val pp : ('a_pp -> 'a -> unit, unit) fmt -> 'a_pp -> 'a option pp
val map : 'a t -> f:('a -> 'b) -> 'b t
val map_or : 'a t -> default:'b -> f:('a -> 'b) -> 'b
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit
val exists : 'a t -> f:('a -> bool) -> bool
val for_all : 'a t -> f:('a -> bool) -> bool
val fold : 'a t -> init:'s -> f:('s -> 'a -> 's) -> 's
