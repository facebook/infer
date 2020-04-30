(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain

(** {2 Building arithmetic constraints} *)

val and_nonnegative : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t
(** [and_nonnegative v astate] is [astate ∧ v≥0] *)

val and_positive : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t
(** [and_positive v astate] is [astate ∧ v>0] *)

val and_eq_int : AbstractValue.t -> IntLit.t -> AbductiveDomain.t -> AbductiveDomain.t
(** [and_eq_int v i astate] is [astate ∧ v=i] *)

(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

val eval_binop :
  AbstractValue.t -> Binop.t -> operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t

val eval_unop :
  AbstractValue.t -> Unop.t -> AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t

val prune_binop :
  negated:bool -> Binop.t -> operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t * bool

(** {2 Queries} *)

val is_known_zero : AbductiveDomain.t -> AbstractValue.t -> bool
(** [is_known_zero astate t] returns [true] if [astate |- t = 0], [false] if we don't know for sure *)

val is_unsat : AbductiveDomain.t -> bool
(** returns whether the state contains a contradiction *)
