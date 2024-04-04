(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

(** Concrete interval domain (CItv) *)

type t [@@deriving compare, equal]

val equal_to : IntLit.t -> t

val is_equal_to_zero : t -> bool

val is_not_equal_to_zero : t -> bool
(** whether this is literally [≠0] *)

val is_non_pointer : t -> bool
(** whether both lb and ub are primitive integers *)

val pp : F.formatter -> t -> unit

val intersection : t -> t -> t option
(** [None] if the intersection is empty *)

type abduction_result =
  | Unsatisfiable  (** the assertion is never true given the parameters *)
  | Satisfiable of t option * t option
      (** the assertion is satisfiable and when it is true then the lhs and rhs can be optionally
          refined to the given new intervals *)

val abduce_binop_is_true : negated:bool -> Binop.t -> t option -> t option -> abduction_result
(** given [arith_lhs_opt bop arith_rhs_opt] and if not [negated], return either

    - [Unsatisfiable] iff lhs bop rhs = ∅

    - [Satisfiable (abduced_lhs_opt,abduced_rhs_opt)] iff lhs bop rhs ≠ ∅, such that (taking
      lhs=true if lhs_opt is [None], same for rhs) [abduced_lhs_opt=Some alhs] if (lhs bop rhs ≠ ∅
      => alhs⇔lhs) (and similarly for rhs)

    If [negated] then imagine a similar explanation replacing "= ∅" with "≠ ∅" and vice-versa. *)

val binop : Binop.t -> t -> t -> t option

val unop : Unop.t -> t -> t option

val to_singleton : t -> IntLit.t option

val requires_integer_reasoning : t -> bool
(** whether the interval can be expressed by a relation over rationals; used in [PulseFormula] to
    decide whether an interval is already expressed by another part of the formula *)
