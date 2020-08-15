(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain

let map_path_condition ~f astate =
  AbductiveDomain.set_path_condition (f astate.AbductiveDomain.path_condition) astate


let and_nonnegative v astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_nonnegative v phi)


let and_positive v astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_positive v phi)


let and_eq_int v i astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.and_eq_int v i phi)


type operand = PathCondition.operand =
  | LiteralOperand of IntLit.t
  | AbstractValueOperand of AbstractValue.t

let eval_binop binop_addr binop op_lhs op_rhs astate =
  map_path_condition astate ~f:(fun phi ->
      PathCondition.eval_binop binop_addr binop op_lhs op_rhs phi )


let eval_unop unop_addr unop addr astate =
  map_path_condition astate ~f:(fun phi -> PathCondition.eval_unop unop_addr unop addr phi)


let prune_binop ~negated bop lhs_op rhs_op astate =
  let phi' =
    PathCondition.prune_binop ~negated bop lhs_op rhs_op astate.AbductiveDomain.path_condition
  in
  AbductiveDomain.set_path_condition phi' astate


let is_known_zero astate v = PathCondition.is_known_zero astate.AbductiveDomain.path_condition v

let is_unsat_cheap astate = PathCondition.is_unsat_cheap astate.AbductiveDomain.path_condition

let is_unsat_expensive astate =
  let phi', is_unsat = PathCondition.is_unsat_expensive astate.AbductiveDomain.path_condition in
  (AbductiveDomain.set_path_condition phi' astate, is_unsat)
