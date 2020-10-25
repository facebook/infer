(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of ContainersLabels.Array

type 'a t = 'a array [@@deriving compare, equal, sexp]

val is_empty : 'a t -> bool
val map : 'a t -> f:('a -> 'b) -> 'b t

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like [map], but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val combine : 'a t -> 'b t -> ('a * 'b) t option
val combine_exn : 'a t -> 'b t -> ('a * 'b) t
val fold : 'a array -> init:'s -> f:('s -> 'a -> 's) -> 's
val to_list_rev_map : 'a array -> f:('a -> 'b) -> 'b list
val pp : (unit, unit) fmt -> 'a pp -> 'a array pp
