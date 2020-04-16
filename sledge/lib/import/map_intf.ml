(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0

module type S = sig
  type key

  module Key : sig
    type t = key

    include Comparator.S with type t := t
  end

  include Core_kernel.Map_intf.Make_S_plain_tree(Key).S

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val map_endo : 'a t -> f:('a -> 'a) -> 'a t
  (** Like map, but specialized to require [f] to be an endofunction, which
      enables preserving [==] if [f] preserves [==] of every element. *)

  val merge_endo :
       'a t
    -> 'b t
    -> f:
         (   key:key
          -> [`Both of 'a * 'b | `Left of 'a | `Right of 'b]
          -> 'a option)
    -> 'a t
  (** Like merge, but specialized to require [f] to preserve the type of the
      left argument, which enables preserving [==] if [f] preserves [==] of
      every element. *)

  val merge_skewed :
    'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t

  val fold_until :
       'v t
    -> init:'a
    -> f:(key:key -> data:'v -> 'a -> ('a, 'r) Continue_or_stop.t)
    -> finish:('a -> 'r)
    -> 'r

  val choose : 'a t -> (key * 'a) option
  (** Find an unspecified element. [O(1)]. *)

  val pop : 'a t -> (key * 'a * 'a t) option
  (** Find and remove an unspecified element. [O(1)]. *)

  val pop_min_elt : 'a t -> (key * 'a * 'a t) option
  (** Find and remove minimum element. [O(log n)]. *)

  val find_and_remove : 'a t -> key -> ('a * 'a t) option
  (** Find and remove an element. *)

  val pp : key pp -> 'a pp -> 'a t pp

  val pp_diff :
       data_equal:('a -> 'a -> bool)
    -> key pp
    -> 'a pp
    -> ('a * 'a) pp
    -> ('a t * 'a t) pp
end
