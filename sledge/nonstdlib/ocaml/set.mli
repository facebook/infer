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

(** Sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.

   The {!Make} functor constructs implementations for any type, given a
   [compare] function.
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

     module PairsSet = Set.Make(IntPairs)

     let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))
   ]}

   This creates a new module [PairsSet], with a new type [PairsSet.t]
   of sets of [int * int].
*)

open! NS0

module type OrderedType =
  sig
    type t
      (** The type of the set elements. *)

    val compare : t -> t -> int
      (** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Stdlib.compare}. *)
  end
(** Input signature of the functor {!Set.Make}. *)

module type S =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    include Comparer.S with type t := t

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged
       (the result of the function is then physically equal to [s]).
       @before 4.03 Physical equality was not ensured. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    val disjoint: t -> t -> bool
    (** Test if two sets are disjoint.
        @since 4.08.0 *)

    val diff: t -> t -> t
    (** Set difference: [diff s1 s2] contains the elements of [s1]
       that are not in [s2]. *)

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    module Provide_equal (_ : sig
      type t = elt [@@deriving equal]
    end) : sig
    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)
    end

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val map: (elt -> elt) -> t -> t
    (** [map f s] is the set whose elements are [f a0],[f a1]... [f
        aN], where [a0],[a1]...[aN] are the elements of [s].

       The elements are passed to [f] in increasing order
       with respect to the ordering over the type of the elements.

       If no element of [s] is changed by [f], [s] is returned
       unchanged. (If each output of [f] is physically equal to its
       input, the returned set is physically equal to [s].)
       @since 4.04.0 *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. If [p] satisfies every element in [s],
       [s] is returned unchanged (the result of the function is then
       physically equal to [s]).
       @before 4.03 Physical equality was not ensured.*)

    val filter_map: (elt -> elt option) -> t -> t
    (** [filter_map f s] returns the set of all [v] such that
        [f x = Some v] for some element [x] of [s].

       For example,
       {[filter_map (fun n -> if n mod 2 = 0 then Some (n / 2) else None) s]}
       is the set of halves of the even elements of [s].

       If no element of [s] is changed or dropped by [f] (if
       [f x = Some x] for each element [x]), then
       [s] is returned unchanged: the result of the function
       is then physically equal to [s].

       @since 4.11.0
     *)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val only_elt: t -> elt option
    (** Return the element of a singleton set, or None otherwise. *)

    val classify : t -> elt zero_one_many

    val min_elt: t -> elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val min_elt_opt: t -> elt option
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or [None]
       if the set is empty.
        @since 4.05
    *)

    val max_elt: t -> elt
    (** Same as {!Set.S.min_elt}, but returns the largest element of the
       given set. *)

    val max_elt_opt: t -> elt option
    (** Same as {!Set.S.min_elt_opt}, but returns the largest element of the
        given set.
        @since 4.05
    *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified, and
       different elements may be chosen for equal sets. *)

    val choose_opt: t -> elt option
    (** Return one element of the given set, or [None] if
        the set is empty. Which element is chosen is unspecified, and
        different elements may be chosen for equal sets.
        @since 4.05
    *)

    val pop : t -> elt * t
    (** Find and remove an unspecified element, or raise [Not_found] if the
        set is empty. [O(1)]. *)

    val pop_opt : t -> (elt * t) option
    (** Find and remove an unspecified element, or [None] if the set is
        empty. [O(1)]. *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val find: elt -> t -> elt
    (** [find x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or raise [Not_found] if no such element
        exists.
        @since 4.01.0 *)

    val find_opt: elt -> t -> elt option
    (** [find_opt x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or [None] if no such element
        exists.
        @since 4.05 *)

    val find_first: (elt -> bool) -> t -> elt
    (** [find_first f s], where [f] is a monotonically increasing function,
       returns the lowest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.

       For example, [find_first (fun e -> Ord.compare e x >= 0) s] will return
       the first element [e] of [s] where [Ord.compare e x >= 0] (intuitively:
       [e >= x]), or raise [Not_found] if [x] is greater than any element of
       [s].

        @since 4.05
       *)

    val find_first_opt: (elt -> bool) -> t -> elt option
    (** [find_first_opt f s], where [f] is a monotonically increasing function,
       returns an option containing the lowest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val find_last: (elt -> bool) -> t -> elt
    (** [find_last f s], where [f] is a monotonically decreasing function,
       returns the highest element [e] of [s] such that [f e],
       or raises [Not_found] if no such element exists.
        @since 4.05
       *)

    val find_last_opt: (elt -> bool) -> t -> elt option
    (** [find_last_opt f s], where [f] is a monotonically decreasing function,
       returns an option containing the highest element [e] of [s] such that
       [f e], or [None] if no such element exists.
        @since 4.05
       *)

    val of_list: elt list -> t
    (** [of_list l] creates a set from a list of elements.
        This is usually more efficient than folding [add] over the list,
        except perhaps for lists with many duplicated elements.
        @since 4.02.0 *)

    (** {1 Iterators} *)

    val to_seq_from : elt -> t -> elt Seq.t
    (** [to_seq_from x s] iterates on a subset of the elements of [s]
        in ascending order, from [x] or above.
        @since 4.07 *)

    val to_seq : t -> elt Seq.t
    (** Iterate on the whole set, in ascending order
        @since 4.07 *)

    val add_seq : elt Seq.t -> t -> t
    (** Add the given elements to the set, in order.
        @since 4.07 *)

    val of_seq : elt Seq.t -> t
    (** Build a set from the given bindings
        @since 4.07 *)

    module Provide_sexp_of (_ : sig
      type t = elt [@@deriving sexp_of]
    end) : sig
      type t [@@deriving sexp_of]
    end
    with type t := t

    module Provide_of_sexp (_ : sig
      type t = elt [@@deriving of_sexp]
    end) : sig
      type t [@@deriving of_sexp]
    end
    with type t := t
  end
(** Output signature of the functor {!Set.Make}. *)

type ('elt, 'cmp) t [@@deriving compare, equal, sexp]

type 'compare_elt compare [@@deriving compare, equal, sexp]

module Make (Ord : Comparer.S) :
  S with type elt = Ord.t
    with type t = (Ord.t, Ord.compare) t
    with type compare = Ord.compare compare
(** Functor building an implementation of the set structure
   given a totally ordered type. *)
