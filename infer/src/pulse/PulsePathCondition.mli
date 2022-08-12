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

val and_eq_const : AbstractValue.t -> Const.t -> t -> t * new_eqs
(** [and_eq_const v c phi] is [phi ∧ v=c] *)

val and_eq_int : AbstractValue.t -> IntLit.t -> t -> t * new_eqs
(** [and_eq_int v i phi] is [and_eq_const v (Cint i) phi] *)

val and_eq_vars : AbstractValue.t -> AbstractValue.t -> t -> t * new_eqs

val simplify :
     Tenv.t
  -> can_be_pruned:AbstractValue.Set.t
  -> keep:AbstractValue.Set.t
  -> get_dynamic_type:(AbstractValue.t -> Typ.t option)
  -> t
  -> (t * AbstractValue.Set.t * new_eqs) SatUnsat.t
(** [simplify ~can_be_pruned ~keep phi] attempts to get rid of as many variables in [fv phi] but not
    in [keep] as possible, and tries to eliminate variables not in [can_be_pruned] from the "pruned"
    part of the formula *)

val and_callee_pre :
     (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
  -> t
  -> callee:t
  -> (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t * t * new_eqs

val and_callee_post :
     (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
  -> t
  -> callee:t
  -> (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t * t * new_eqs

(** {2 Operations} *)

type operand = PulseFormula.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}
[@@deriving compare, equal]

val and_equal : operand -> operand -> t -> t * new_eqs

val and_not_equal : operand -> operand -> t -> t * new_eqs

val eval_binop : AbstractValue.t -> Binop.t -> operand -> operand -> t -> t * new_eqs

val eval_binop_av :
  AbstractValue.t -> Binop.t -> AbstractValue.t -> AbstractValue.t -> t -> t * new_eqs
(** Helper function that wraps [eval_binop], to be used when both operands are abstract values *)

val eval_unop : AbstractValue.t -> Unop.t -> AbstractValue.t -> t -> t * new_eqs

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> t * new_eqs

val and_is_int : AbstractValue.t -> t -> t * new_eqs

val and_eq_instanceof : AbstractValue.t -> AbstractValue.t -> Typ.t -> t -> t * new_eqs

(** {2 Queries} *)

val is_known_zero : t -> AbstractValue.t -> bool
(** [is_known_zero phi t] returns [true] if [phi |- t = 0], [false] if we don't know for sure *)

val is_known_not_equal_zero : t -> AbstractValue.t -> bool
(** [is_known_not_equal_zero phi t] returns [true] if [phi |- t != 0], [false] if we don't know for
    sure.

    This only consults the concrete intervals domain for now *)

val is_unsat_cheap : t -> bool
(** whether the state contains a contradiction, call this as often as you want *)

val is_unsat_expensive :
  Tenv.t -> get_dynamic_type:(AbstractValue.t -> Typ.t option) -> t -> t * bool * new_eqs
(** whether the state contains a contradiction, only call this when you absolutely have to *)

val is_manifest : is_allocated:(AbstractValue.t -> bool) -> t -> bool
(** whether the current path is independent of any calling context *)

val get_both_var_repr : t -> AbstractValue.t -> AbstractValue.t
(** get the canonical representative for the variable according to the equality relation in the
    "both" (known + pruned) part of the formula *)
