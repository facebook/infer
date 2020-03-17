(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Import0

module type S = sig
  type key
  type +'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val t_of_sexp : (Sexp.t -> key) -> (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val pp : key pp -> 'a pp -> 'a t pp

  val pp_diff :
       data_equal:('a -> 'a -> bool)
    -> key pp
    -> 'a pp
    -> ('a * 'a) pp
    -> ('a t * 'a t) pp

  (* initial constructors *)
  val empty : 'a t

  (* constructors *)
  val set : 'a t -> key:key -> data:'a -> 'a t
  val add_exn : 'a t -> key:key -> data:'a -> 'a t
  val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
  val remove : 'a t -> key -> 'a t
  val update : 'a t -> key -> f:('a option -> 'a) -> 'a t

  val merge :
       'a t
    -> 'b t
    -> f:
         (   key:key
          -> [`Both of 'a * 'b | `Left of 'a | `Right of 'b]
          -> 'c option)
    -> 'c t

  val merge_skewed :
    'a t -> 'a t -> combine:(key:key -> 'a -> 'a -> 'a) -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val filter_keys : 'a t -> f:(key -> bool) -> 'a t
  val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t

  (* queries *)
  val is_empty : 'b t -> bool
  val length : 'b t -> int
  val mem : 'a t -> key -> bool
  val find : 'a t -> key -> 'a option
  val find_and_remove : 'a t -> key -> ('a * 'a t) option
  val find_multi : 'a list t -> key -> 'a list
  val data : 'a t -> 'a list
  val to_alist : 'a t -> (key * 'a) list

  (* traversals *)
  val iter : 'a t -> f:('a -> unit) -> unit
  val iteri : 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val for_alli : 'a t -> f:(key:key -> data:'a -> bool) -> bool
  val fold : 'a t -> init:'s -> f:(key:key -> data:'a -> 's -> 's) -> 's
end
