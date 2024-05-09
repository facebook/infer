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

(** Wrapper around {!PulseFormula} that operates on {!AbductiveDomain.t}. *)

val and_nonnegative :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_positive :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_eq_int :
  AbstractValue.t -> IntLit.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_eq_const :
  AbstractValue.t -> Const.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

type operand = Formula.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}

val and_equal :
  operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_not_equal :
  operand -> operand -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_equal_string_concat :
     AbstractValue.t
  -> operand
  -> operand
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val eval_binop :
     AbstractValue.t
  -> Binop.t
  -> operand
  -> operand
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t SatUnsat.t

val eval_binop_absval :
     AbstractValue.t
  -> Binop.t
  -> AbstractValue.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t SatUnsat.t
(** [eval_binop_absval ret binop lhs rhs astate] is
    [eval_binop ret binop (AbstractValueOperand lhs) (AbstractValueOperand rhs) astate] *)

val eval_unop :
     AbstractValue.t
  -> Unop.t
  -> AbstractValue.t
  -> AbductiveDomain.t
  -> (AbductiveDomain.t * AbstractValue.t) AccessResult.t SatUnsat.t

val prune_binop :
     negated:bool
  -> Binop.t
  -> operand
  -> operand
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val prune_eq_zero :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val prune_ne_zero :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val prune_nonnegative :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val prune_positive :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val prune_gt_one :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val prune_eq_one :
  AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t
(** helper function wrapping [prune_binop] *)

val is_known_zero : AbductiveDomain.t -> AbstractValue.t -> bool

val is_manifest : AbductiveDomain.Summary.t -> bool
(** whether the state is *manifest* according to {!PulseFormula.is_manifest}, with an additional
    condition that some equalities path conditions may be represented implicitly by having several
    edges pointing to the same value in the precondition; if the pre exhibits sharing that means
    some access paths in the precondition are equal, which means that equality on these access paths
    has been assumed at some point in the function so the set of assumptions is not empty (see also
    the documentation for {!PulseFormula.is_manifest} and
    {!PulseAbductiveDomain.Summary.pre_heap_has_assumptions}) *)

val and_is_int : AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_equal_instanceof :
     AbstractValue.t
  -> AbstractValue.t
  -> Typ.t
  -> ?nullable:bool
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val and_dynamic_type_is :
     AbstractValue.t
  -> Typ.t
  -> ?source_file:SourceFile.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t AccessResult.t SatUnsat.t

val get_dynamic_type : AbstractValue.t -> AbductiveDomain.t -> Formula.dynamic_type_data option

val copy_type_constraints :
  AbstractValue.t -> AbstractValue.t -> AbductiveDomain.t -> AbductiveDomain.t

(* version that dies on failure *)
val and_dynamic_type_is_unsafe :
     AbstractValue.t
  -> Typ.t
  -> ?source_file:SourceFile.t
  -> Location.t
  -> AbductiveDomain.t
  -> AbductiveDomain.t

val absval_of_int : AbductiveDomain.t -> IntLit.t -> AbductiveDomain.t * AbstractValue.t

val absval_of_string : AbductiveDomain.t -> string -> AbductiveDomain.t * AbstractValue.t

val as_constant_string : AbductiveDomain.t -> AbstractValue.t -> string option
