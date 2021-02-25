(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain

(** Wrapper around {!PathCondition} that operates on {!AbductiveDomain.t}. *)

val and_nonnegative : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t

val and_positive : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t

val and_eq_int : AbstractValue.t -> IntLit.t -> AbductiveDomain.t -> AbductiveDomain.t

type operand = PathCondition.operand =
  | LiteralOperand of IntLit.t
  | AbstractValueOperand of AbstractValue.t

val eval_binop :
  AbstractValue.t -> Binop.t -> operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t

val eval_unop :
  AbstractValue.t -> Unop.t -> AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t

val prune_binop :
  negated:bool -> Binop.t -> operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t

val prune_eq_zero : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t
(** helper function wrapping [prune_binop] *)

val prune_positive : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t
(** helper function wrapping [prune_binop] *)

val is_known_zero : AbductiveDomain.t -> AbstractValue.t -> bool

val is_unsat_cheap : AbductiveDomain.t -> bool

val has_no_assumptions : AbductiveDomain.t -> bool

val and_equal_instanceof :
  AbstractValue.t -> AbstractValue.t -> Typ.t -> AbductiveDomain.t -> AbductiveDomain.t
