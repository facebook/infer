(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module AbstractValue = PulseAbstractValue
module Formula = PulseFormula
module SatUnsat = PulseSatUnsat
module ValueHistory = PulseValueHistory

type t = Formula.t [@@deriving compare, equal, yojson_of]

let pp fmt formula = Formula.pp fmt formula

let true_ = Formula.ttrue

type new_eqs = PulseFormula.new_eqs

type operand = Formula.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: Formula.function_symbol; actuals: AbstractValue.t list}
[@@deriving compare, equal]

let literal_zero = ConstOperand (Const.Cint IntLit.zero)

let and_nonnegative v phi = Formula.and_less_equal literal_zero (AbstractValueOperand v) phi

let and_positive v phi = Formula.and_less_than literal_zero (AbstractValueOperand v) phi

let and_eq_const v c phi = Formula.and_equal (AbstractValueOperand v) (ConstOperand c) phi

let and_eq_int v i phi = and_eq_const v (Cint i) phi

let and_eq_vars v1 v2 phi =
  Formula.and_equal (AbstractValueOperand v1) (AbstractValueOperand v2) phi


let and_equal op1 op2 phi = Formula.and_equal op1 op2 phi

let and_not_equal op1 op2 phi = Formula.and_not_equal op1 op2 phi

let simplify tenv ~precondition_vocabulary ~keep ~get_dynamic_type phi =
  Formula.simplify tenv ~precondition_vocabulary ~keep ~get_dynamic_type phi


let subst_find_or_new subst addr_callee =
  match AbstractValue.Map.find_opt addr_callee subst with
  | None ->
      (* map restricted (≥0) values to restricted values to preserve their semantics *)
      let addr_caller = AbstractValue.mk_fresh_same_kind addr_callee in
      L.d_printfln "new subst %a <-> %a (fresh)" AbstractValue.pp addr_callee AbstractValue.pp
        addr_caller ;
      let addr_hist_fresh = (addr_caller, ValueHistory.epoch) in
      (AbstractValue.Map.add addr_callee addr_hist_fresh subst, fst addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, fst addr_hist_caller)


let and_callee_pre subst phi ~callee:phi_callee =
  Formula.and_conditions_fold_subst_variables phi ~up_to_f:phi_callee ~f:subst_find_or_new
    ~init:subst


let and_formula_callee subst formula_caller ~callee:formula_callee =
  (* need to translate callee variables to make sense for the caller, thereby possibly extending
     the current substitution *)
  Formula.and_fold_subst_variables formula_caller ~up_to_f:formula_callee ~f:subst_find_or_new
    ~init:subst


let and_callee_post subst phi ~callee:phi_callee = and_formula_callee subst phi ~callee:phi_callee

(** {2 Operations} *)

let eval_binop binop_addr binop op_lhs op_rhs phi =
  Formula.and_equal_binop binop_addr binop op_lhs op_rhs phi


let eval_binop_av binop_addr binop av_lhs av_rhs phi =
  eval_binop binop_addr binop (AbstractValueOperand av_lhs) (AbstractValueOperand av_rhs) phi


let eval_unop unop_addr unop addr phi =
  Formula.and_equal_unop unop_addr unop (AbstractValueOperand addr) phi


let prune_binop ~negated bop lhs_op rhs_op phi = Formula.prune_binop ~negated bop lhs_op rhs_op phi

let and_is_int v phi = Formula.and_is_int v phi

let and_eq_instanceof v1 v2 t phi = Formula.and_equal_instanceof v1 v2 t phi

(** {2 Queries} *)

let is_known_zero phi v = Formula.is_known_zero phi v

let is_known_non_zero phi v = Formula.is_known_non_zero phi v

let normalize tenv ~get_dynamic_type phi = Formula.normalize tenv ~get_dynamic_type phi

let is_manifest ~is_allocated phi = Formula.is_manifest ~is_allocated phi

let get_var_repr phi v = Formula.get_var_repr phi v
