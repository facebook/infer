(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Arithmetic terms *)

include module type of Arithmetic_intf

(** Arithmetic terms, e.g. polynomials, polymorphic in the type of
    indeterminates. *)
type ('trm, 'cmp) t [@@deriving compare, equal, sexp]

(** Functor that, given a totally ordered type of indeterminate terms,
    builds an implementation of the embedding-independent arithmetic
    operations, and a functor that, given an embedding of arithmetic terms
    into indeterminate terms, builds an implementation of the arithmetic
    operations. *)
module Make (Ord : sig
  type t [@@deriving equal, sexp]

  include Comparer.S with type t := t
end) : sig
  include S0 with type t = (Ord.t, Ord.compare) t with type trm := Ord.t

  module Embed
      (Var : Var_intf.S)
      (Trm : TRM with type t = Ord.t with type var := Var.t)
      (_ : EMBEDDING with type trm := Trm.t and type t := t) :
    S with type trm := Trm.t with type t = t
end
