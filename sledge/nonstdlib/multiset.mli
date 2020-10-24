(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Multiset - Set with (signed) rational multiplicity for each element *)

include module type of Multiset_intf

module Make
    (Mul : MULTIPLICITY) (Elt : sig
      type t [@@deriving compare, sexp_of]
    end) : S with type mul = Mul.t with type elt = Elt.t
