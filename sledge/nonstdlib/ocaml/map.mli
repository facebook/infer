(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   For instance:
   {[
     module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
               0 -> Stdlib.compare y0 y1
             | c -> c
       end

     module PairsMap = Map.Make(IntPairs)

     let m = PairsMap.(empty |> add (0,1) "hello" |> add (1,0) "world")
   ]}

   This creates a new module [PairsMap], with a new type ['a PairsMap.t]
   of maps from [int * int] to ['a]. In this example, [m] contains [string]
   values so its type is [string PairsMap.t].
*)

open! NS0

module type OrderedType =
  sig
    type t
      (** The type of the map keys. *)

    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Stdlib.compare}. *)
  end
(** Input signature of the functor {!Map.Make}. *)

module type S =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    include Comparer.S1 with type 'a t := 'a t

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m] to a value that is physically equal to [y],
       [m] is returned unchanged (the result of the function is
       then physically equal to [m]). Otherwise, the previous binding
       of [x] in [m] disappears.
       @before 4.03 Physical equality was not ensured. *)

    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    (** [update x f m] returns a map containing the same bindings as
        [m], except for the binding of [x]. Depending on the value of
        [y] where [y] is [f (find_opt x m)], the binding of [x] is
        added, removed or updated. If [y] is [None], the binding is
        removed if it exists; otherwise, if [y] is [Some z] then [x]
        is associated to [z] in the resulting map.  If [x] was already
        bound in [m] to a value that is physically equal to [z], [m]
        is returned unchanged (the result of the function is then
        physically equal to [m]).
        @since 4.06.0
    *)

    val singleton: key -> 'a -> 'a t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x].
        @since 3.12.0
     *)

    val is_singleton: 'a t -> bool
    (** Test whether a map contains only a single binding or not. *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map.
       If [x] was not in [m], [m] is returned unchanged
       (the result of the function is then physically equal to [m]).
       @before 4.03 Physical equality was not ensured. *)

    val merge:
         (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    (** [merge f m1 m2] computes a map whose keys are a subset of the keys of
        [m1] and of [m2]. The presence of each such binding, and the
        corresponding value, is determined with the function [f].
        In terms of the [find_opt] operation, we have
        [find_opt x (merge f m1 m2) = f x (find_opt x m1) (find_opt x m2)]
        for any key [x], provided that [f x None None = None].
        @since 3.12.0
     *)

    val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    (** [union f m1 m2] computes a map whose keys are a subset of the keys
        of [m1] and of [m2].  When the same binding is defined in both
        arguments, the function [f] is used to combine them.
        This is a special case of [merge]: [union f m1 m2] is equivalent
        to [merge f' m1 m2], where
        - [f' _key None None = None]
        - [f' _key (Some v) None = Some v]
        - [f' _key None (Some v) = Some v]
        - [f' key (Some v1) (Some v2) = f key v1 v2]

        @since 4.03.0
    *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    module Provide_equal (_ : sig
      type t = key [@@deriving equal]
    end) : sig
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)
    end

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    (** [for_all p m] checks if all the bindings of the map
        satisfy the predicate [p].
        @since 3.12.0
     *)

    val exists: (key -> 'a -> bool) -> 'a t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfies the predicate [p].
        @since 3.12.0
     *)

    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. If every binding in [m] satisfies [p],
        [m] is returned unchanged (the result of the function is then
        physically equal to [m])
        @since 3.12.0
       @before 4.03 Physical equality was not ensured.
     *)

    val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
    (** [filter_map f m] applies the function [f] to every binding of
        [m], and builds a map from the results. For each binding
        [(k, v)] in the input map:
        - if [f k v] is [None] then [k] is not in the result,
        - if [f k v] is [Some v'] then the binding [(k, v')]
          is in the output map.

        For example, the following function on maps whose values are lists
        {[
        filter_map
          (fun _k li -> match li with [] -> None | _::tl -> Some tl)
          m
        ]}
        drops all bindings of [m] whose value is an empty list, and pops
        the first element of each value that is non-empty.

        @since 4.11.0
     *)

    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [m] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [m] that do not satisfy [p].
        @since 3.12.0
     *)

    val cardinal: 'a t -> int
    (** Return the number of bindings of a map.
        @since 3.12.0
     *)

    val bindings: 'a t -> (key * 'a) list
    (** Return the list of all bindings of the given map.
       The returned list is sorted in increasing order of keys with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Map.Make}.
        @since 3.12.0
     *)

    val only_binding: 'a t -> (key * 'a) option
    (** Return the binding of a singleton map, or None otherwise. *)

    val classify : 'a t -> (key, 'a) zero_one_many2

    val min_binding: 'a t -> (key * 'a)
    (** Return the binding with the smallest key in a given map
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the map is empty.
        @since 3.12.0
     *)

    val min_binding_opt: 'a t -> (key * 'a) option
    (** Return the binding with the smallest key in the given map
       (with respect to the [Ord.compare] ordering), or [None]
       if the map is empty.
        @since 4.05
     *)

    val max_binding: 'a t -> (key * 'a)
    (** Same as {!Map.S.min_binding}, but returns the binding with
        the largest key in the given map.
        @since 3.12.0
     *)

    val max_binding_opt: 'a t -> (key * 'a) option
    (** Same as {!Map.S.min_binding_opt}, but returns the binding with
        the largest key in the given map.
        @since 4.05
     *)

    val choose: 'a t -> (key * 'a)
    (** Return one binding of the given map, or raise [Not_found] if
       the map is empty. Which binding is chosen is unspecified,
       and different bindings may be chosen for equal maps.
        @since 3.12.0
     *)

    val choose_opt: 'a t -> (key * 'a) option
    (** Return one binding of the given map, or [None] if
       the map is empty. Which binding is chosen is unspecified,
       and different bindings may be chosen for equal maps.
        @since 4.05
     *)

    val divide : 'a t -> ('a t * key * 'a * 'a t) option
    (** [divide m] returns [None] if [m] is empty and otherwise
        [Some (l, key, data, r)], where
          [key] is some key bound in [m];
          [data] is the data associated to [key] in [m];
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [key];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [key].
        Runs in constant time, and the [l] and [r] maps are close
        to the same size.
     *)

    val divide_exn : 'a t -> ('a t * key * 'a * 'a t)
    (** Same as {!Map.S.divide}, but raises [Not_found] if the map is empty. *)

    val split: key -> 'a t -> 'a t * 'a option * 'a t
    (** [split x m] returns a triple [(l, data, r)], where
          [l] is the map with all the bindings of [m] whose key
        is strictly less than [x];
          [r] is the map with all the bindings of [m] whose key
        is strictly greater than [x];
          [data] is [None] if [m] contains no binding for [x],
          or [Some v] if [m] binds [v] to [x].
        @since 3.12.0
     *)

    val find: key -> 'a t -> 'a
    (** [find x m] returns the current value of [x] in [m],
       or raises [Not_found] if no binding for [x] exists. *)

    val find_opt: key -> 'a t -> 'a option
    (** [find_opt x m] returns [Some v] if the current value of [x]
        in [m] is [v], or [None] if no binding for [x] exists.
        @since 4.05
    *)

    val find_first: (key -> bool) -> 'a t -> key * 'a
    (** [find_first f m], where [f] is a monotonically increasing function,
       returns the binding of [m] with the lowest key [k] such that [f k],
       or raises [Not_found] if no such key exists.

       For example, [find_first (fun k -> Ord.compare k x >= 0) m] will return
       the first binding [k, v] of [m] where [Ord.compare k x >= 0]
       (intuitively: [k >= x]), or raise [Not_found] if [x] is greater than any
       element of [m].

        @since 4.05
       *)

    val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
    (** [find_first_opt f m], where [f] is a monotonically increasing function,
       returns an option containing the binding of [m] with the lowest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       *)

    val find_last: (key -> bool) -> 'a t -> key * 'a
    (** [find_last f m], where [f] is a monotonically decreasing function,
       returns the binding of [m] with the highest key [k] such that [f k],
       or raises [Not_found] if no such key exists.
        @since 4.05
       *)

    val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
    (** [find_last_opt f m], where [f] is a monotonically decreasing function,
       returns an option containing the binding of [m] with the highest key [k]
       such that [f k], or [None] if no such key exists.
        @since 4.05
       *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    (** {1 Iterators} *)

    val to_seq : 'a t -> (key * 'a) Seq.t
    (** Iterate on the whole map, in ascending order of keys
        @since 4.07 *)

    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    (** [to_seq_from k m] iterates on a subset of the bindings of [m],
        in ascending order of keys, from key [k] or above.
        @since 4.07 *)

    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    (** Add the given bindings to the map, in order.
        @since 4.07 *)

    val of_seq : (key * 'a) Seq.t -> 'a t
    (** Build a map from the given bindings
        @since 4.07 *)

    module Provide_sexp_of (_ : sig
      type t = key [@@deriving sexp_of]
    end) : sig
      type 'a t [@@deriving sexp_of]
    end
    with type 'a t := 'a t

    module Provide_of_sexp (_ : sig
      type t = key [@@deriving of_sexp]
    end) : sig
      type 'a t [@@deriving of_sexp]
    end
    with type 'a t := 'a t
  end
(** Output signature of the functor {!Map.Make}. *)

type ('key, +'a, 'compare_key) t [@@deriving compare, equal, sexp]

type ('compare_key, 'compare_a) compare [@@deriving compare, equal, sexp]

module Make (Ord : Comparer.S) :
  S with type key = Ord.t
    with type +'a t = (Ord.t, 'a, Ord.compare) t
    with type 'compare_a compare = (Ord.compare, 'compare_a) compare
(** Functor building an implementation of the map structure
   given a totally ordered type. *)
