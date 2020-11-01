(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

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
  val add_exn : key:key -> data:'a -> 'a t -> 'a t
  val add : key:key -> data:'a -> 'a t -> 'a t
  val add_multi : key:key -> data:'a -> 'a list t -> 'a list t
  val remove : key -> 'a t -> 'a t

  val merge :
       'a t
    -> 'b t
    -> f:
         (   key
          -> [`Left of 'a | `Both of 'a * 'b | `Right of 'b]
          -> 'c option)
    -> 'c t

  val merge_endo :
       'a t
    -> 'b t
    -> f:
         (   key
          -> [`Left of 'a | `Both of 'a * 'b | `Right of 'b]
          -> 'a option)
    -> 'a t
  (** Like merge, but specialized to require [f] to preserve the type of the
      left argument, which enables preserving [==] if [f] preserves [==] of
      every value. *)

  val union : 'a t -> 'a t -> f:(key -> 'a -> 'a -> 'a option) -> 'a t
  val partition : 'a t -> f:(key -> 'a -> bool) -> 'a t * 'a t

  val partition_map :
    'a t -> f:(key -> 'a -> ('b, 'c) Either.t) -> 'b t * 'c t

  (** {1 Query} *)

  val is_empty : 'a t -> bool
  val is_singleton : 'a t -> bool
  val length : 'a t -> int
  val only_binding : 'a t -> (key * 'a) option
  val classify : 'a t -> [`Zero | `One of key * 'a | `Many]

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
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a option
  val find_exn : key -> 'a t -> 'a
  val find_multi : key -> 'a list t -> 'a list

  val find_and_remove : key -> 'a t -> ('a * 'a t) option
  (** Find and remove the binding for a key. *)

  val pop : 'a t -> (key * 'a * 'a t) option
  (** Find and remove an unspecified binding. Different bindings may be
      chosen for equivalent maps. [O(1)]. *)

  val pop_min_binding : 'a t -> (key * 'a * 'a t) option
  (** Find and remove binding with minimum key. [O(log n)]. *)

  (** {1 Transform} *)

  val change : key -> 'a t -> f:('a option -> 'a option) -> 'a t
  val update : key -> 'a t -> f:('a option -> 'a) -> 'a t
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
  val fold : 'a t -> 's -> f:(key:key -> data:'a -> 's -> 's) -> 's

  (** {1 Convert} *)

  val keys : 'a t -> key iter
  val values : 'a t -> 'a iter
  val to_iter : 'a t -> (key * 'a) iter
  val to_list : 'a t -> (key * 'a) list
  val of_iter : (key * 'a) iter -> 'a t
  val of_list : (key * 'a) list -> 'a t

  val to_iter2 :
       'a t
    -> 'b t
    -> (key * [`Left of 'a | `Both of 'a * 'b | `Right of 'b]) iter

  val symmetric_diff :
       'a t
    -> 'b t
    -> eq:('a -> 'b -> bool)
    -> (key * [> `Left of 'a | `Unequal of 'a * 'b | `Right of 'b]) iter

  (** {1 Pretty-print} *)

  val pp : key pp -> 'a pp -> 'a t pp

  val pp_diff :
       key pp
    -> 'a pp
    -> ('a * 'a) pp
    -> eq:('a -> 'a -> bool)
    -> ('a t * 'a t) pp
end
