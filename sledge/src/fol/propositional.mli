(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Propositional formulas *)

include module type of Propositional_intf

module Make (Trm : sig
  type t [@@deriving compare, equal, sexp]
end) : sig
  module Fml : FORMULA with type trm := Trm.t
  module Fmls : FORMULA_SET with type elt := Fml.t with type t = Fml.set
end
