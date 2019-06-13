(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Qset - Set with (signed) rational multiplicity for each element *)

open Base

type ('elt, 'cmp) t

type ('elt, 'cmp) comparator =
  (module Comparator.S with type t = 'elt and type comparator_witness = 'cmp)

module M (Elt : sig
  type t
  type comparator_witness
end) : sig
  type nonrec t = (Elt.t, Elt.comparator_witness) t
end

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

module type M_of_sexp = sig
  type t [@@deriving of_sexp]

  include Comparator.S with type t := t
end

module type Compare_m = sig end
module type Hash_fold_m = Hasher.S

val sexp_of_m__t :
  (module Sexp_of_m with type t = 'elt) -> ('elt, 'cmp) t -> Sexp.t

val m__t_of_sexp :
     (module M_of_sexp with type t = 'elt and type comparator_witness = 'cmp)
  -> Sexp.t
  -> ('elt, 'cmp) t

val compare_m__t :
  (module Compare_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> int

val equal_m__t :
  (module Compare_m) -> ('elt, 'cmp) t -> ('elt, 'cmp) t -> bool

val hash_fold_m__t :
     (module Hash_fold_m with type t = 'elt)
  -> Hash.state
  -> ('elt, _) t
  -> Hash.state

val hash_m__t :
  (module Hash_fold_m with type t = 'elt) -> ('elt, _) t -> Hash.hash_value

val empty : ('elt, 'cmp) comparator -> ('elt, 'cmp) t
(** The empty multiset over the provided order. *)

val add : ('a, 'c) t -> 'a -> Q.t -> ('a, 'c) t
(** Add to multiplicity of single element. [O(log n)] *)

val remove : ('a, 'c) t -> 'a -> ('a, 'c) t
(** Set the multiplicity of an element to zero. [O(log n)] *)

val union : ('a, 'c) t -> ('a, 'c) t -> ('a, 'c) t
(** Sum multiplicities pointwise. [O(n + m)] *)

val length : _ t -> int
(** Number of elements with non-zero multiplicity. [O(1)]. *)

val count : ('a, _) t -> 'a -> Q.t
(** Multiplicity of an element. [O(log n)]. *)

val count_and_remove : ('a, 'c) t -> 'a -> (Q.t * ('a, 'c) t) option
(** Multiplicity of an element, and remove it. [O(log n)]. *)

val map : ('a, 'c) t -> f:('a -> Q.t -> 'a * Q.t) -> ('a, 'c) t
(** Map over the elements in ascending order. Preserves physical equality if
    [f] does. *)

val map_counts : ('a, 'c) t -> f:('a -> Q.t -> Q.t) -> ('a, 'c) t
(** Map over the multiplicities of the elements in ascending order. *)

val fold : ('a, _) t -> f:('a -> Q.t -> 's -> 's) -> init:'s -> 's
(** Fold over the elements in ascending order. *)

val fold_map :
     ('a, 'c) t
  -> f:('a -> Q.t -> 's -> 'a * Q.t * 's)
  -> init:'s
  -> ('a, 'c) t * 's
(** Folding map over the elements in ascending order. Preserves physical
    equality if [f] does. *)

val for_all : ('a, _) t -> f:('a -> Q.t -> bool) -> bool
(** Universal property test. [O(n)] but returns as soon as a violation is
    found, in ascending order. *)

val iter : ('a, _) t -> f:('a -> Q.t -> unit) -> unit
(** Iterate over the elements in ascending order. *)

val min_elt : ('a, _) t -> ('a * Q.t) option
(** Minimum element. *)

val min_elt_exn : ('a, _) t -> 'a * Q.t
(** Minimum element. *)

val to_list : ('a, _) t -> ('a * Q.t) list
(** Convert to a list of elements in ascending order. *)
