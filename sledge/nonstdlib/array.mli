(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0
include module type of Base.Array

val pp : (unit, unit) fmt -> 'a pp -> 'a array pp

val map_endo : 'a t -> f:('a -> 'a) -> 'a t
(** Like map, but specialized to require [f] to be an endofunction, which
    enables preserving [==] if [f] preserves [==] of every element. *)

val fold_map_inplace : 'a array -> init:'s -> f:('s -> 'a -> 's * 'a) -> 's
val to_list_rev_map : 'a array -> f:('a -> 'b) -> 'b list
