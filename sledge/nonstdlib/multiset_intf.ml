(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Multiset - Set with (signed) rational multiplicity for each element *)

open! NS0

module type MULTIPLICITY = sig
  type t [@@deriving compare, equal, hash, sexp]

  include Comparer.S with type t := t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
end

module type S = sig
  type mul
  type elt
  type t [@@deriving compare, equal, sexp_of]

  val hash_fold_t : elt Hash.folder -> t Hash.folder

  include Comparer.S with type t := t

  module Provide_of_sexp (_ : sig
    type t = elt [@@deriving of_sexp]
  end) : sig
    type t [@@deriving of_sexp]
  end
  with type t := t

  val pp :
       ?pre:(unit, unit) fmt
    -> ?suf:(unit, unit) fmt
    -> (unit, unit) fmt
    -> (elt * mul) pp
    -> t pp

  (* constructors *)

  val empty : t
  (** The empty multiset over the provided order. *)

  val of_ : elt -> mul -> t

  val add : elt -> mul -> t -> t
  (** Add to multiplicity of single element. [O(log n)] *)

  val remove : elt -> t -> t
  (** Set the multiplicity of an element to zero. [O(log n)] *)

  val union : t -> t -> t
  (** Add multiplicities pointwise. [O(n + m)] *)

  val diff : t -> t -> t
  (** Subtract multiplicities pointwise. [O(n + m)] *)

  val map : t -> f:(elt -> mul -> elt * mul) -> t
  (** Map over the elements in ascending order. Preserves physical equality
      if [f] does. *)

  val map_counts : t -> f:(mul -> mul) -> t
  (** Map over the multiplicities of the elements in ascending order. *)

  val mapi_counts : t -> f:(elt -> mul -> mul) -> t
  (** Map over the multiplicities of the elements in ascending order. *)

  val flat_map : t -> f:(elt -> mul -> t) -> t
  (** Flat map over the elements in ascending order. Preserves physical
      equality if [f e m] is a singleton [(e', m')] with [e == e'] and
      [Mul.equal m m'] for all elements. *)

  val partition : t -> f:(elt -> mul -> bool) -> t * t
  val partition_map : t -> f:(elt -> mul -> (mul, mul) Either.t) -> t * t

  (* queries *)

  val is_empty : t -> bool
  val is_singleton : t -> bool

  val length : t -> int
  (** Number of elements with non-zero multiplicity. [O(1)]. *)

  val count : elt -> t -> mul
  (** Multiplicity of an element. [O(log n)]. *)

  val only_elt : t -> (elt * mul) option
  (** The only element of a singleton multiset. [O(1)]. *)

  val min_elt : t -> (elt * mul) option
  (** Minimum element. [O(log n)]. *)

  val pop_min_elt : t -> (elt * mul * t) option
  (** Find and remove minimum element. [O(log n)]. *)

  val classify : t -> (elt, mul) zero_one_many2
  (** Classify a set as either empty, singleton, or otherwise. *)

  val find_and_remove : elt -> t -> mul option * t
  (** Find and remove an element. *)

  val to_iter : t -> (elt * mul) iter
  (** Enumerate elements in ascending order. *)

  (* traversals *)

  val iter : t -> f:(elt -> mul -> unit) -> unit
  (** Iterate over the elements in ascending order. *)

  val exists : t -> f:(elt -> mul -> bool) -> bool
  (** Search for an element satisfying a predicate. *)

  val for_all : t -> f:(elt -> mul -> bool) -> bool
  (** Test whether all elements satisfy a predicate. *)

  val fold : t -> 's -> f:(elt -> mul -> 's -> 's) -> 's
  (** Fold over the elements in ascending order. *)
end
