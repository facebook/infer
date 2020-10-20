(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Multiset - Set with (signed) rational multiplicity for each element *)

open NS0

module type MULTIPLICITY = sig
  type t [@@deriving compare, equal, hash, sexp]

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
end

module type S = sig
  type mul
  type elt
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash_fold_t : elt Hash.folder -> t Hash.folder
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : (Sexp.t -> elt) -> Sexp.t -> t
  val pp : (unit, unit) fmt -> (elt * mul) pp -> t pp

  (* constructors *)

  val empty : t
  (** The empty multiset over the provided order. *)

  val of_ : elt -> mul -> t

  val add : t -> elt -> mul -> t
  (** Add to multiplicity of single element. [O(log n)] *)

  val remove : t -> elt -> t
  (** Set the multiplicity of an element to zero. [O(log n)] *)

  val union : t -> t -> t
  (** Sum multiplicities pointwise. [O(n + m)] *)

  val map : t -> f:(elt -> mul -> elt * mul) -> t
  (** Map over the elements in ascending order. Preserves physical equality
      if [f] does. *)

  val map_counts : t -> f:(elt -> mul -> mul) -> t
  (** Map over the multiplicities of the elements in ascending order. *)

  (* queries *)

  val is_empty : t -> bool
  val is_singleton : t -> bool

  val length : t -> int
  (** Number of elements with non-zero multiplicity. [O(1)]. *)

  val count : t -> elt -> mul
  (** Multiplicity of an element. [O(log n)]. *)

  val choose_exn : t -> elt * mul
  (** Find an unspecified element. [O(1)]. *)

  val choose : t -> (elt * mul) option
  (** Find an unspecified element. [O(1)]. *)

  val pop : t -> (elt * mul * t) option
  (** Find and remove an unspecified element. [O(1)]. *)

  val min_elt : t -> (elt * mul) option
  (** Minimum element. [O(log n)]. *)

  val pop_min_elt : t -> (elt * mul * t) option
  (** Find and remove minimum element. [O(log n)]. *)

  val classify : t -> [`Zero | `One of elt * mul | `Many]
  (** Classify a set as either empty, singleton, or otherwise. *)

  val find_and_remove : t -> elt -> (mul * t) option
  (** Find and remove an element. *)

  val to_list : t -> (elt * mul) list
  (** Convert to a list of elements in ascending order. *)

  (* traversals *)

  val iter : t -> f:(elt -> mul -> unit) -> unit
  (** Iterate over the elements in ascending order. *)

  val exists : t -> f:(elt -> mul -> bool) -> bool
  (** Search for an element satisfying a predicate. *)

  val for_all : t -> f:(elt -> mul -> bool) -> bool
  (** Test whether all elements satisfy a predicate. *)

  val fold : t -> f:(elt -> mul -> 's -> 's) -> init:'s -> 's
  (** Fold over the elements in ascending order. *)
end
