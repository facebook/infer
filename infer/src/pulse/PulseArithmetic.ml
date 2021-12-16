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

let map_path_condition_common ~f astate =
  let phi, new_eqs = f astate.AbductiveDomain.path_condition in
  let astate = AbductiveDomain.set_path_condition phi astate in
  let result =
    AbductiveDomain.incorporate_new_eqs new_eqs astate |> AccessResult.of_abductive_result
  in
  (result, new_eqs)


let map_path_condition ~f astate = map_path_condition_common ~f astate |> fst

let map_path_condition_with_ret ~f astate ret =
  let result, new_eqs = map_path_condition_common ~f astate in
  PulseResult.map result ~f:(fun result ->
      (result, AbductiveDomain.incorporate_new_eqs_on_val new_eqs ret) )


let and_nonnegative v astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_nonnegative v phi)


let and_positive v astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_positive v phi)


let and_eq_int v i astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_eq_int v i phi)


let and_eq_const v c astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_eq_const v c phi)


type operand = PathCondition.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}

let and_equal op1 op2 astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_equal op1 op2 phi)


let eval_binop binop_addr binop op_lhs op_rhs astate =
  map_path_condition_with_ret astate binop_addr ~f:(fun phi ->
      PathCondition.eval_binop binop_addr binop op_lhs op_rhs phi )


let eval_unop unop_addr unop addr astate =
  map_path_condition_with_ret astate unop_addr ~f:(fun phi ->
      PathCondition.eval_unop unop_addr unop addr phi )


let prune_binop ~negated bop lhs_op rhs_op astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.prune_binop ~negated bop lhs_op rhs_op phi)


let literal_zero = ConstOperand (Cint IntLit.zero)

let prune_eq_zero v astate =
  prune_binop ~negated:false Eq (AbstractValueOperand v) literal_zero astate


let prune_positive v astate =
  prune_binop ~negated:false Gt (AbstractValueOperand v) literal_zero astate


let is_known_zero astate v = PathCondition.is_known_zero astate.AbductiveDomain.path_condition v

let is_unsat_cheap astate = PathCondition.is_unsat_cheap astate.AbductiveDomain.path_condition

let has_no_assumptions astate =
  PathCondition.has_no_assumptions astate.AbductiveDomain.path_condition


let and_is_int v astate = map_path_condition astate ~f:(fun phi -> PathCondition.and_is_int v phi)

let and_equal_instanceof v1 v2 t astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_eq_instanceof v1 v2 t phi)
