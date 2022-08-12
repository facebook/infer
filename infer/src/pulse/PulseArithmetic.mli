(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module AccessResult = PulseAccessResult

(** Wrapper around {!PathCondition} that operates on {!AbductiveDomain.t}. *)

val and_nonnegative : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val and_positive : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val and_eq_int :
  AbstractValue.t -> IntLit.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val and_eq_const :
  AbstractValue.t -> Const.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

type operand = PathCondition.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}

val and_equal : operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val and_not_equal : operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val eval_binop :
     AbstractValue.t
  -> Binop.t
  -> operand
  -> operand
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t

val eval_binop_av :
     AbstractValue.t
  -> Binop.t
  -> AbstractValue.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t
(** Shorter than [eval_binop] when both operands are abstract values *)

val eval_unop :
     AbstractValue.t
  -> Unop.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t

val prune_binop :
     negated:bool
  -> Binop.t
  -> operand
  -> operand
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t

val prune_eq_zero : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t
(** helper function wrapping [prune_binop] *)

val prune_positive : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t
(** helper function wrapping [prune_binop] *)

val prune_gt_one : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t
(** helper function wrapping [prune_binop] *)

val prune_eq_one : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t
(** helper function wrapping [prune_binop] *)

val is_known_zero : AbductiveDomain.t -> AbstractValue.t -> bool

val is_unsat_cheap : AbductiveDomain.t -> bool

val is_manifest : AbductiveDomain.t -> bool

val and_is_int : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t

val and_equal_instanceof :
     AbstractValue.t
  -> AbstractValue.t
  -> Typ.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t
