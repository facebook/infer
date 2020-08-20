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

val is_known_zero : AbductiveDomain.t -> AbstractValue.t -> bool

val is_equal_to : AbductiveDomain.t -> AbstractValue.t -> IntLit.t -> bool

val is_known_neq_zero : AbductiveDomain.t -> AbstractValue.t -> bool

val is_unsat_cheap : AbductiveDomain.t -> bool

val is_unsat_expensive : AbductiveDomain.t -> AbductiveDomain.t * bool
