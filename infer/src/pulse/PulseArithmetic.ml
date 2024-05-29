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
  let open SatUnsat.Import in
  let* phi, new_eqs = f astate.AbductiveDomain.path_condition in
  let astate = AbductiveDomain.set_path_condition phi astate in
  let+ result =
    AbductiveDomain.incorporate_new_eqs new_eqs astate >>| AccessResult.of_abductive_result
  in
  (result, new_eqs)


let map_path_condition ~f astate =
  let open SatUnsat.Import in
  map_path_condition_common ~f astate >>| fst


let map_path_condition_with_ret ~f astate ret =
  let open SatUnsat.Import in
  let+ result, new_eqs = map_path_condition_common ~f astate in
  PulseResult.map result ~f:(fun result ->
      (result, AbductiveDomain.incorporate_new_eqs_on_val new_eqs ret) )


let literal_zero = Formula.ConstOperand (Cint IntLit.zero)

let and_nonnegative v astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_less_equal literal_zero (AbstractValueOperand v) phi )


let and_positive v astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_less_than literal_zero (AbstractValueOperand v) phi )


let and_eq_const v c astate =
  map_path_condition astate ~f:(fun phi ->
      Formula.and_equal (AbstractValueOperand v) (ConstOperand c) phi )


let and_eq_int v i astate = and_eq_const v (Cint i) astate

type operand = Formula.operand =
  | AbstractValueOperand of AbstractValue.t
  | ConstOperand of Const.t
  | FunctionApplicationOperand of {f: PulseFormula.function_symbol; actuals: AbstractValue.t list}

let and_equal lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal lhs rhs phi)


let and_not_equal lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_not_equal lhs rhs phi)


let eval_binop ret binop lhs rhs astate =
  map_path_condition_with_ret astate ret ~f:(fun phi ->
      Formula.and_equal_binop ret binop lhs rhs phi )


let eval_binop_absval ret binop lhs rhs astate =
  eval_binop ret binop (AbstractValueOperand lhs) (AbstractValueOperand rhs) astate


let eval_unop ret unop v astate =
  map_path_condition_with_ret astate ret ~f:(fun phi ->
      Formula.and_equal_unop ret unop (AbstractValueOperand v) phi )


let prune_binop ~negated binop lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.prune_binop ~negated binop lhs rhs phi)


let and_equal_string_concat ret lhs rhs astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal_string_concat ret lhs rhs phi)


let literal_zero = ConstOperand (Cint IntLit.zero)

let literal_one = ConstOperand (Cint IntLit.one)

let prune_eq_zero v astate =
  prune_binop ~negated:false Eq (AbstractValueOperand v) literal_zero astate


let prune_ne_zero v astate =
  prune_binop ~negated:false Ne (AbstractValueOperand v) literal_zero astate


let prune_nonnegative v astate =
  prune_binop ~negated:false Ge (AbstractValueOperand v) literal_zero astate


let prune_positive v astate =
  prune_binop ~negated:false Gt (AbstractValueOperand v) literal_zero astate


let prune_gt_one v astate =
  prune_binop ~negated:false Gt (AbstractValueOperand v) literal_one astate


let prune_eq_one v astate =
  prune_binop ~negated:false Eq (AbstractValueOperand v) literal_one astate


let is_known_zero astate v = Formula.is_known_zero astate.AbductiveDomain.path_condition v

let is_manifest summary =
  Formula.is_manifest (AbductiveDomain.Summary.get_path_condition summary) ~is_allocated:(fun v ->
      AbductiveDomain.Summary.is_heap_allocated summary v
      || AbductiveDomain.Summary.get_must_be_valid v summary |> Option.is_some )
  && not (AbductiveDomain.Summary.pre_heap_has_assumptions summary)


let and_is_int v astate = map_path_condition astate ~f:(fun phi -> Formula.and_is_int v phi)

let and_equal_instanceof v1 v2 t ?(nullable = false) astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_equal_instanceof v1 v2 t ~nullable phi)


let and_dynamic_type_is v t ?source_file astate =
  map_path_condition astate ~f:(fun phi -> Formula.and_dynamic_type v t ?source_file phi)


let get_dynamic_type v astate = Formula.get_dynamic_type v astate.AbductiveDomain.path_condition

(* this is just to ease migration of previous calls to PulseOperations.add_dynamic_type, which can't fail *)
let and_dynamic_type_is_unsafe v t ?source_file location astate =
  let phi =
    Formula.add_dynamic_type_unsafe v t ?source_file location astate.AbductiveDomain.path_condition
  in
  AbductiveDomain.set_path_condition phi astate


let copy_type_constraints v_src v_target astate =
  let phi = Formula.copy_type_constraints v_src v_target astate.AbductiveDomain.path_condition in
  AbductiveDomain.set_path_condition phi astate


let absval_of_int astate i =
  let phi, v = Formula.absval_of_int astate.AbductiveDomain.path_condition i in
  let astate = AbductiveDomain.set_path_condition phi astate in
  (astate, v)


let absval_of_string astate s =
  let phi, v = Formula.absval_of_string astate.AbductiveDomain.path_condition s in
  let astate = AbductiveDomain.set_path_condition phi astate in
  (astate, v)


let as_constant_string astate v = Formula.as_constant_string astate.AbductiveDomain.path_condition v
