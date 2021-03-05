(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module SatUnsat = PulseSatUnsat
module ValueHistory = PulseValueHistory

type t [@@deriving compare, equal, yojson_of]

val true_ : t

val false_ : t

val pp : F.formatter -> t -> unit

type new_eqs = PulseFormula.new_eqs

(** {2 Building arithmetic constraints} *)

val and_nonnegative : AbstractValue.t -> t -> t * new_eqs
(** [and_nonnegative v phi] is [phi ∧ v≥0] *)

val and_positive : AbstractValue.t -> t -> t * new_eqs
(** [and_positive v phi] is [phi ∧ v>0] *)

val and_eq_int : AbstractValue.t -> IntLit.t -> t -> t * new_eqs
(** [and_eq_int v i phi] is [phi ∧ v=i] *)

val and_eq_vars : AbstractValue.t -> AbstractValue.t -> t -> t * new_eqs

val simplify :
     Tenv.t
  -> keep:AbstractValue.Set.t
  -> get_dynamic_type:(AbstractValue.t -> Typ.t option)
  -> t
  -> (t * new_eqs) SatUnsat.t
(** [simplify ~keep phi] attempts to get rid of as many variables in [fv phi] but not in [keep] as
    possible *)

val simplify_instanceof : Tenv.t -> get_dynamic_type:(AbstractValue.t -> Typ.t option) -> t -> t

val and_callee :
     (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
  -> t
  -> callee:t
  -> (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t * t * new_eqs

(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t
[@@deriving compare]

val pp_operand : Formatter.t -> operand -> unit

val eval_binop : AbstractValue.t -> Binop.t -> operand -> operand -> t -> t * new_eqs

val eval_unop : AbstractValue.t -> Unop.t -> AbstractValue.t -> t -> t * new_eqs

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> t * new_eqs

val and_eq_instanceof : AbstractValue.t -> AbstractValue.t -> Typ.t -> t -> t * new_eqs

(** {2 Queries} *)

val is_known_zero : t -> AbstractValue.t -> bool
(** [is_known_zero phi t] returns [true] if [phi |- t = 0], [false] if we don't know for sure *)

val is_known_not_equal_zero : t -> AbstractValue.t -> bool
(** [is_known_not_equal_zero phi t] returns [true] if [phi |- t != 0], [false] if we don't know for
    sure *)

(* this only consults the concrete intervals domain for now *)

val is_unsat_cheap : t -> bool
(** whether the state contains a contradiction, call this as often as you want *)

val is_unsat_expensive :
  Tenv.t -> get_dynamic_type:(AbstractValue.t -> Typ.t option) -> t -> t * bool * new_eqs
  [@@warning "-32"]
(** whether the state contains a contradiction, only call this when you absolutely have to *)

val as_int : t -> AbstractValue.t -> int option
(** [as_int phi t] returns an integer x such that [phi |- t = x], if known for sure; see also
    [is_known_zero] *)

val has_no_assumptions : t -> bool
(** whether the current path is independent of any calling context *)

val get_var_repr : t -> AbstractValue.t -> AbstractValue.t
(** get the canonical representative for the variable according to the equality relation *)
