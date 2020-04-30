(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module AbductiveDomain = PulseAbductiveDomain
module AddressAttributes = AbductiveDomain.AddressAttributes

(** {2 Building arithmetic constraints} *)

let and_eq_terms t1 t2 astate =
  let phi = PathCondition.and_eq t1 t2 astate.AbductiveDomain.path_condition in
  AbductiveDomain.set_path_condition phi astate


let and_term t astate =
  let phi = PathCondition.and_term t astate.AbductiveDomain.path_condition in
  AbductiveDomain.set_path_condition phi astate


let and_nonnegative v astate =
  AddressAttributes.add_one v (BoItv Itv.ItvPure.nat) astate
  |> AddressAttributes.add_one v (CItv CItv.zero_inf)
  |> and_term PathCondition.Term.(le zero (of_absval v))


let and_positive v astate =
  AddressAttributes.add_one v (BoItv Itv.ItvPure.pos) astate
  |> AddressAttributes.add_one v (CItv (CItv.ge_to IntLit.one))
  |> and_term PathCondition.Term.(lt zero (of_absval v))


let and_eq_int v i astate =
  AddressAttributes.add_one v (BoItv (Itv.ItvPure.of_int_lit i)) astate
  |> AddressAttributes.add_one v (CItv (CItv.equal_to i))
  |> and_eq_terms (PathCondition.Term.of_absval v) (PathCondition.Term.of_intlit i)


(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

let eval_citv_operand binop_addr bop op_lhs op_rhs astate =
  let citv_of_op op astate =
    match op with
    | LiteralOperand i ->
        Some (CItv.equal_to i)
    | AbstractValueOperand v ->
        AddressAttributes.get_citv v astate
  in
  match
    Option.both (citv_of_op op_lhs astate) (citv_of_op op_rhs astate)
    |> Option.bind ~f:(fun (addr_lhs, addr_rhs) -> CItv.binop bop addr_lhs addr_rhs)
  with
  | None ->
      astate
  | Some binop_a ->
      let astate = AddressAttributes.add_one binop_addr (CItv binop_a) astate in
      astate


let eval_bo_itv_binop binop_addr bop op_lhs op_rhs astate =
  let bo_itv_of_op op astate =
    match op with
    | LiteralOperand i ->
        Itv.ItvPure.of_int_lit i
    | AbstractValueOperand v ->
        AddressAttributes.get_bo_itv v astate
  in
  let bo_itv =
    Itv.ItvPure.arith_binop bop (bo_itv_of_op op_lhs astate) (bo_itv_of_op op_rhs astate)
  in
  AddressAttributes.add_one binop_addr (BoItv bo_itv) astate


let eval_path_condition_binop binop_addr binop op_lhs op_rhs astate =
  let term_of_op = function
    | LiteralOperand i ->
        PathCondition.Term.of_intlit i
    | AbstractValueOperand v ->
        PathCondition.Term.of_absval v
  in
  and_eq_terms
    (PathCondition.Term.of_absval binop_addr)
    (PathCondition.Term.of_binop binop (term_of_op op_lhs) (term_of_op op_rhs))
    astate


let eval_binop binop_addr binop op_lhs op_rhs astate =
  eval_path_condition_binop binop_addr binop op_lhs op_rhs astate
  |> eval_citv_operand binop_addr binop op_lhs op_rhs
  |> eval_bo_itv_binop binop_addr binop op_lhs op_rhs


let eval_unop_citv unop_addr unop operand_addr astate =
  match
    AddressAttributes.get_citv operand_addr astate |> Option.bind ~f:(fun a -> CItv.unop unop a)
  with
  | None ->
      astate
  | Some unop_a ->
      AddressAttributes.add_one unop_addr (CItv unop_a) astate


let eval_unop_bo_itv unop_addr unop operand_addr astate =
  match Itv.ItvPure.arith_unop unop (AddressAttributes.get_bo_itv operand_addr astate) with
  | None ->
      astate
  | Some itv ->
      AddressAttributes.add_one unop_addr (BoItv itv) astate


let eval_path_condition_unop unop_addr unop addr astate =
  and_eq_terms
    (PathCondition.Term.of_absval unop_addr)
    PathCondition.Term.(of_unop unop (of_absval addr))
    astate


let eval_unop unop_addr unop addr astate =
  eval_path_condition_unop unop_addr unop addr astate
  |> eval_unop_citv unop_addr unop addr
  |> eval_unop_bo_itv unop_addr unop addr


let prune_with_bop ~negated v_opt arith bop arith' astate =
  match
    Option.both v_opt (if negated then Binop.negate bop else Some bop)
    |> Option.map ~f:(fun (v, positive_bop) -> (v, Itv.ItvPure.prune_binop positive_bop arith arith')
       )
  with
  | None ->
      (astate, true)
  | Some (_, Bottom) ->
      (astate, false)
  | Some (v, NonBottom arith_pruned) ->
      let attr_arith = Attribute.BoItv arith_pruned in
      let astate =
        AddressAttributes.abduce_attribute v attr_arith astate
        |> AddressAttributes.add_one v attr_arith
      in
      (astate, true)


let eval_operand astate = function
  | LiteralOperand i ->
      (None, Some (CItv.equal_to i), Itv.ItvPure.of_int_lit i, PathCondition.Term.of_intlit i)
  | AbstractValueOperand v ->
      ( Some v
      , AddressAttributes.get_citv v astate
      , AddressAttributes.get_bo_itv v astate
      , PathCondition.Term.of_absval v )


let record_abduced addr_opt arith_opt astate =
  match Option.both addr_opt arith_opt with
  | None ->
      astate
  | Some (addr, arith) ->
      let attribute = Attribute.CItv arith in
      AddressAttributes.abduce_attribute addr attribute astate
      |> AddressAttributes.add_one addr attribute


let bind_satisfiable ~satisfiable astate ~f = if satisfiable then f astate else (astate, false)

let prune_binop ~negated bop lhs_op rhs_op astate =
  let value_lhs_opt, arith_lhs_opt, bo_itv_lhs, path_cond_lhs = eval_operand astate lhs_op in
  let value_rhs_opt, arith_rhs_opt, bo_itv_rhs, path_cond_rhs = eval_operand astate rhs_op in
  let astate =
    let path_condition =
      let t_positive = PathCondition.Term.of_binop bop path_cond_lhs path_cond_rhs in
      let t = if negated then PathCondition.Term.not_ t_positive else t_positive in
      PathCondition.and_term t astate.AbductiveDomain.path_condition
    in
    AbductiveDomain.set_path_condition path_condition astate
  in
  match CItv.abduce_binop_is_true ~negated bop arith_lhs_opt arith_rhs_opt with
  | Unsatisfiable ->
      (astate, false)
  | Satisfiable (abduced_lhs, abduced_rhs) ->
      let astate =
        record_abduced value_lhs_opt abduced_lhs astate |> record_abduced value_rhs_opt abduced_rhs
      in
      let satisfiable =
        match Itv.ItvPure.arith_binop bop bo_itv_lhs bo_itv_rhs |> Itv.ItvPure.to_boolean with
        | False ->
            negated
        | True ->
            not negated
        | Top ->
            true
        | Bottom ->
            false
      in
      let astate, satisfiable =
        bind_satisfiable ~satisfiable astate ~f:(fun astate ->
            prune_with_bop ~negated value_lhs_opt bo_itv_lhs bop bo_itv_rhs astate )
      in
      Option.value_map (Binop.symmetric bop) ~default:(astate, satisfiable) ~f:(fun bop' ->
          bind_satisfiable ~satisfiable astate ~f:(fun astate ->
              prune_with_bop ~negated value_rhs_opt bo_itv_rhs bop' bo_itv_lhs astate ) )


(** {2 Queries} *)

let is_known_zero astate v =
  AddressAttributes.get_citv v astate |> Option.value_map ~default:false ~f:CItv.is_equal_to_zero
  || (let phi = astate.AbductiveDomain.path_condition in
      PathCondition.is_known_zero (PathCondition.Term.of_absval v) phi )
  || Itv.ItvPure.is_zero (AddressAttributes.get_bo_itv v astate)


let is_unsat astate =
  (* note: contradictions are detected eagerly for all domains except path conditions, so just
     evaluate that one *)
  PathCondition.is_unsat astate.AbductiveDomain.path_condition
