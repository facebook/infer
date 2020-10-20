(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open NS0

exception Not_found = Stdlib.Not_found

module type S = sig
  type key
  type +'a t [@@deriving compare, equal, sexp_of]

  module Provide_of_sexp (_ : sig
    type t = key [@@deriving of_sexp]
  end) : sig
    val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  end

  (** {1 Construct} *)

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val add_exn : 'a t -> key:key -> data:'a -> 'a t
  val set : 'a t -> key:key -> data:'a -> 'a t
  val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
  val remove : 'a t -> key -> 'a t

  val merge :
       'a t
    -> 'b t
    -> f:
         (   key:key
          -> [`Left of 'a | `Both of 'a * 'b | `Right of 'b]
          -> 'c option)
    -> 'c t

  val merge_endo :
       'a t
    -> 'b t
    -> f:
         (   key:key
          -> [`Left of 'a | `Both of 'a * 'b | `Right of 'b]
          -> 'a option)
    -> 'a t
  (** Like merge, but specialized to require [f] to preserve the type of the
      left argument, which enables preserving [==] if [f] preserves [==] of
      every value. *)

  val merge_skewed :
    'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t

  val union : 'a t -> 'a t -> f:(key -> 'a -> 'a -> 'a option) -> 'a t

  (** {1 Query} *)

  val is_empty : 'a t -> bool
  val is_singleton : 'a t -> bool
  val length : 'a t -> int

  val choose_key : 'a t -> key option
  (** Find an unspecified key. Different keys may be chosen for equivalent
      maps. [O(1)]. *)

  val choose : 'a t -> (key * 'a) option
  (** Find an unspecified binding. Different bindings may be chosen for
      equivalent maps. [O(1)]. *)

  val choose_exn : 'a t -> key * 'a
  (** Find an unspecified binding. Different bindings may be chosen for
      equivalent maps. [O(1)]. *)

  val min_binding : 'a t -> (key * 'a) option
  val mem : 'a t -> key -> bool
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val find_multi : 'a list t -> key -> 'a list

  val find_and_remove : 'a t -> key -> ('a * 'a t) option
  (** Find and remove the binding for a key. *)

  val pop : 'a t -> (key * 'a * 'a t) option
  (** Find and remove an unspecified binding. Different bindings may be
      chosen for equivalent maps. [O(1)]. *)

  val pop_min_binding : 'a t -> (key * 'a * 'a t) option
  (** Find and remove binding with minimum key. [O(log n)]. *)

  (** {1 Transform} *)

  val change : 'a t -> key -> f:('a option -> 'a option) -> 'a t
  val update : 'a t -> key -> f:('a option -> 'a) -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t

  val map_endo : 'a t -> f:('a -> 'a) -> 'a t
  (** Like map, but specialized to require [f] to be an endofunction, which
      enables preserving [==] if [f] preserves [==] of every value. *)

  val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t

  (** {1 Traverse} *)

  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val existsi : 'a t -> f:(key:key -> data:'a -> bool) -> bool
  val for_alli : 'a t -> f:(key:key -> data:'a -> bool) -> bool
  val fold : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b

  (** {1 Convert} *)

  val to_alist :
    ?key_order:[`Increasing | `Decreasing] -> 'a t -> (key * 'a) list

  val data : 'a t -> 'a list

  val to_iter2 :
       'a t
    -> 'b t
    -> (key * [`Left of 'a | `Both of 'a * 'b | `Right of 'b]) iter

  val symmetric_diff :
       data_equal:('a -> 'b -> bool)
    -> 'a t
    -> 'b t
    -> (key * [> `Left of 'a | `Unequal of 'a * 'b | `Right of 'b]) iter

  (** {1 Pretty-print} *)

  val pp : key pp -> 'a pp -> 'a t pp

  val pp_diff :
       data_equal:('a -> 'a -> bool)
    -> key pp
    -> 'a pp
    -> ('a * 'a) pp
    -> ('a t * 'a t) pp
end
