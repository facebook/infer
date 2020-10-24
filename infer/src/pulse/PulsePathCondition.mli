(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module AbstractValue = PulseAbstractValue
module ValueHistory = PulseValueHistory

type t [@@deriving yojson_of]

val true_ : t

val is_true : t -> bool

val pp : F.formatter -> t -> unit

(** {2 Building arithmetic constraints} *)

val and_nonnegative : AbstractValue.t -> t -> t
(** [and_nonnegative v phi] is [phi ∧ v≥0] *)

val and_positive : AbstractValue.t -> t -> t
(** [and_positive v phi] is [phi ∧ v>0] *)

val and_eq_int : AbstractValue.t -> IntLit.t -> t -> t
(** [and_eq_int v i phi] is [phi ∧ v=i] *)

val simplify : keep:AbstractValue.Set.t -> t -> t
(** [simplify ~keep phi] attempts to get rid of as many variables in [fv phi] but not in [keep] as
    possible *)

val and_callee :
     (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t
  -> t
  -> callee:t
  -> (AbstractValue.t * ValueHistory.t) AbstractValue.Map.t * t

(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

val eval_binop : AbstractValue.t -> Binop.t -> operand -> operand -> t -> t

val eval_unop : AbstractValue.t -> Unop.t -> AbstractValue.t -> t -> t

val prune_binop : negated:bool -> Binop.t -> operand -> operand -> t -> t

(** {2 Queries} *)

val is_known_zero : t -> AbstractValue.t -> bool
(** [is_known_zero phi t] returns [true] if [phi |- t = 0], [false] if we don't know for sure *)

val is_equal_to : t -> AbstractValue.t -> IntLit.t -> bool
(** [is_equal_to phi t i] returns [true] if [phi |- t = i], [false] if we don't know for sure *)

 val get_variables : t  -> AbstractValue.Set.t

val is_known_neq_zero : t -> AbstractValue.t -> bool
(** [is_known_neq_zero phi t] returns [true] if [phi |- t != 0], [false] if we don't know for sure *)

val is_unsat_cheap : t -> bool
(** whether the state contains a contradiction, call this as often as you want *)

val is_unsat_expensive : t -> t * bool
(** whether the state contains a contradiction, only call this when you absolutely have to *)

val as_int : t -> AbstractValue.t -> int option
(** [as_int phi t] returns an integer x such that [phi |- t = x], if known for sure; see also
    [is_known_zero] *)

val has_no_assumptions : t -> bool
(** whether the current path is independent of any calling context *)
