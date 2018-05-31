(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Vector - Immutable view of an array

    Note that vectors and arrays can be interconverted without copying. So
    Vector is not a safe immutable data structure, it is only attempts to
    make it inconvenient to mutate. *)

open Base

type +'a t

module Infix : sig
  type +'a vector = 'a t [@@deriving compare, sexp]
end

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t

(* val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t *)
(* val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t *)
(* val binary_search : *)
(* ('a t, 'a, 'key) Base.Binary_searchable_intf.binary_search *)
(* val binary_search_segmented : *)
(* ('a t, 'a) Binary_searchable_intf.binary_search_segmented *)
(* val mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool *)

val length : 'a t -> int

val is_empty : 'a t -> bool

val iter : 'a t -> f:('a -> unit) -> unit

val rev_iter : 'a t -> f:('a -> unit) -> unit

val fold : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum

(* val fold_result : *)
(* 'a t -> init:'accum -> f:('accum -> 'a -> ('accum, 'e) result) *)
(* -> ('accum, 'e) result *)
(* val fold_until : *)
(* 'a t -> init:'accum *)
(* -> f:('accum -> 'a -> ('accum, 'stop) Container_intf.Continue_or_stop.t) *)
(* -> ('accum, 'stop) Container_intf.Finished_or_stopped_early.t *)
(* val exists : 'a t -> f:('a -> bool) -> bool *)

val for_all : 'a t -> f:('a -> bool) -> bool

(* val count : 'a t -> f:('a -> bool) -> int *)
(* val sum : *)
(* (module Base__.Commutative_group.S with type t = 'sum) -> *)
(* 'a t -> f:('a -> 'sum) -> 'sum *)

val find : 'a t -> f:('a -> bool) -> 'a option

(* val find_map : 'a t -> f:('a -> 'b option) -> 'b option *)

val to_list : 'a t -> 'a list

val to_array : 'a t -> 'a array
(** [to_array v] is an array that shares its representation with vector [v],
    therefore mutating [to_array v] changes [v] (as well as all the shallow
    copies of [v] that are likely to exist). The intended use for [to_array]
    is e.g. to pattern match on a vector, or to interface with some existing
    array code that is known to not mutate its arguments. *)

(* val min_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option *)
(* val max_elt : 'a t -> cmp:('a -> 'a -> int) -> 'a option *)
(* val invariant : 'a Base__Invariant_intf.t -> 'a t Base__Invariant_intf.t *)
(* val max_length : int *)

external get : 'a t -> int -> 'a = "%array_safe_get"

(* external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get" *)

val create : len:int -> 'a -> 'a t

val init : int -> f:(int -> 'a) -> 'a t

(* val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t *)
(* val append : 'a t -> 'a t -> 'a t *)
(* val concat : 'a t list -> 'a t *)
(* val copy : 'a t -> 'a t *)

val of_array : 'a array -> 'a t
(** [of_array a] is a vector that shares its representation with array [a],
    therefore mutating [a] changes [of_array a]. The intended use for
    [of_array] is for converting an array to a vector when the array will
    not be used after conversion, or with care for multi-step initialization
    of a vector. *)

val of_list : 'a list -> 'a t

val of_option : 'a option -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

(* val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t *)
(* val folding_mapi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b * 'c) ->
   'c t *)
(* val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'b * 'c t *)
(* val fold_mapi : *)
(* 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b * 'c) -> 'b * 'c t *)

val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

(* val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b *)

val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

(* val sort : 'a t -> cmp:('a -> 'a -> int) -> 'a t *)
(* val stable_sort : 'a t -> cmp:('a -> 'a -> int) -> 'a t *)
(* val is_sorted : 'a t -> cmp:('a -> 'a -> int) -> bool *)
(* val is_sorted_strictly : 'a t -> cmp:('a -> 'a -> int) -> bool *)
(* val concat_map : 'a t -> f:('a -> 'b t) -> 'b t *)
(* val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t *)
(* val partition_tf : 'a t -> f:('a -> bool) -> 'a t * 'a t *)
(* val partitioni_tf : 'a t -> f:(int -> 'a -> bool) -> 'a t * 'a t *)
(* val cartesian_product : 'a t -> 'b t -> ('a * 'b) t *)
(* val transpose : 'a t t -> 'a t t option *)
(* val transpose_exn : 'a t t -> 'a t t *)
(* val normalize : 'a t -> int -> int *)

val slice : 'a t -> int -> int -> 'a t

(* val nget : 'a t -> int -> 'a *)
(* val filter_opt : 'a option t -> 'a t *)
(* val filter_map : 'a t -> f:('a -> 'b option) -> 'b t *)
(* val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t *)
(* val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool *)
(* val existsi : 'a t -> f:(int -> 'a -> bool) -> bool *)
(* val counti : 'a t -> f:(int -> 'a -> bool) -> int *)

val iter2_exn : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit

(* val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t *)
(* val fold2_exn : 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c *)

val for_all2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

(* val exists2_exn : 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool *)
(* val filter : 'a t -> f:('a -> bool) -> 'a t *)
(* val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t *)

val of_list_rev : 'a list -> 'a t

(* val of_list_map : 'a list -> f:('a -> 'b) -> 'b t *)
(* val of_list_rev_map : 'a list -> f:('a -> 'b) -> 'b t *)

val find_exn : 'a t -> f:('a -> bool) -> 'a

(* val find_map_exn : 'a t -> f:('a -> 'b option) -> 'b *)
(* val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option *)
(* val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a *)
(* val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option *)
(* val find_mapi_exn : 'a t -> f:(int -> 'a -> 'b option) -> 'b *)
(* val find_consecutive_duplicate : *)
(* 'a t -> equal:('a -> 'a -> bool) -> ('a * 'a) option *)
(* val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option *)
(* val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a *)
(* val random_element : ?random_state:Random.State.t -> 'a t -> 'a option *)
(* val random_element_exn : ?random_state:Random.State.t -> 'a t -> 'a *)
(* val zip : 'a t -> 'b t -> ('a * 'b) t option *)
(* val zip_exn : 'a t -> 'b t -> ('a * 'b) t *)
(* val unzip : ('a * 'b) t -> 'a t * 'b t *)
(* val last : 'a t -> 'a *)

val empty : 'a t
(* val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool *)
(* val to_sequence : 'a t -> 'a Sequence.t *)
