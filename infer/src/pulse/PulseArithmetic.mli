(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module Bound : sig
  type t = Int of IntLit.t | MinusInfinity | PlusInfinity
end

type t = private Between of Bound.t * Bound.t | Outside of IntLit.t * IntLit.t
[@@deriving compare]

val equal_to : IntLit.t -> t

val pp : F.formatter -> t -> unit

type abduction_result =
  | Unsatisfiable  (** the assertion is never true given the parameters *)
  | Satisfiable of t option * t option
      (** the assertion is satisfiable and when it is true then the lhs and rhs can be optionally
          refined to the given new intervals *)

val abduce_binop_is_true : negated:bool -> Binop.t -> t option -> t option -> abduction_result
(** given [arith_lhs_opt bop arith_rhs_opt] and if not [negated], return either

   - [Unsatisfiable] iff lhs bop rhs = ∅

   - [Satisfiable (abduced_lhs_opt,abduced_rhs_opt)] iff lhs bop rhs ≠ ∅, such that (taking lhs=true
    if lhs_opt is [None], same for rhs) [abduced_lhs_opt=Some alhs] if (lhs bop rhs ≠ ∅ => alhs⇔lhs)
    (and similarly for rhs)

    If [negated] then imagine a similar explanation replacing "= ∅" with "≠ ∅" and vice-versa.
*)
