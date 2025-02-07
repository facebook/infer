(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Multiset - Set with multiplicity for each element *)

include module type of Multiset_intf

type ('elt, 'mul, 'compare_elt) t [@@deriving compare, equal, sexp]
type ('compare_elt, 'compare_mul) compare [@@deriving compare, equal, sexp]

module Make (Elt : sig
  type t [@@deriving equal, sexp_of]

  include Comparer.S with type t := t
end)
(Mul : MULTIPLICITY) :
  S
    with type mul = Mul.t
    with type elt = Elt.t
    with type compare = (Elt.compare, Mul.compare) compare
    with type t = (Elt.t, Mul.t, Elt.compare) t
