(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of NSSet_intf

type ('elt, 'compare_elt) t [@@deriving compare, equal, sexp]
type 'compare_elt compare [@@deriving compare, equal, sexp]

module Make (Elt : sig
  type t [@@deriving compare, equal, sexp_of]
end) : S with type elt = Elt.t

module Make_from_Comparer (Elt : sig
  type t [@@deriving equal, sexp_of]

  include Comparer.S with type t := t
end) :
  S
    with type elt = Elt.t
    with type compare_elt = Elt.compare
    with type compare = Elt.compare compare
    with type t = (Elt.t, Elt.compare) t
