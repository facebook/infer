(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

(** {2 Building arithmetic constraints} *)

let and_nonnegative trace v astate =
  AddressAttributes.add_one v (BoItv Itv.ItvPure.nat) astate
  |> AddressAttributes.add_one v (CItv (CItv.zero_inf, trace))


let and_positive trace v astate =
  AddressAttributes.add_one v (BoItv Itv.ItvPure.pos) astate
  |> AddressAttributes.add_one v (CItv (CItv.ge_to IntLit.one, trace))


let and_eq_int trace v i astate =
  AddressAttributes.add_one v (BoItv (Itv.ItvPure.of_int_lit i)) astate
  |> AddressAttributes.add_one v (CItv (CItv.equal_to i, trace))


(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

let eval_arith_operand location binop_addr binop_hist bop op_lhs op_rhs astate =
  let arith_of_op op astate =
    match op with
    | LiteralOperand i ->
        Some (CItv.equal_to i)
    | AbstractValueOperand v ->
        AddressAttributes.get_citv v astate |> Option.map ~f:fst
  in
  match
    Option.both (arith_of_op op_lhs astate) (arith_of_op op_rhs astate)
    |> Option.bind ~f:(fun (addr_lhs, addr_rhs) -> CItv.binop bop addr_lhs addr_rhs)
  with
  | None ->
      astate
  | Some binop_a ->
      let binop_trace = Trace.Immediate {location; history= binop_hist} in
      let astate = AddressAttributes.add_one binop_addr (CItv (binop_a, binop_trace)) astate in
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


let eval_binop location binop op_lhs op_rhs binop_hist astate =
  let binop_addr = AbstractValue.mk_fresh () in
  let astate =
    eval_arith_operand location binop_addr binop_hist binop op_lhs op_rhs astate
    |> eval_bo_itv_binop binop_addr binop op_lhs op_rhs
  in
  (astate, (binop_addr, binop_hist))


let eval_unop_arith location unop_addr unop operand_addr unop_hist astate =
  match
    AddressAttributes.get_citv operand_addr astate
    |> Option.bind ~f:(function a, _ -> CItv.unop unop a)
  with
  | None ->
      astate
  | Some unop_a ->
      let unop_trace = Trace.Immediate {location; history= unop_hist} in
      AddressAttributes.add_one unop_addr (CItv (unop_a, unop_trace)) astate


let eval_unop_bo_itv unop_addr unop operand_addr astate =
  match Itv.ItvPure.arith_unop unop (AddressAttributes.get_bo_itv operand_addr astate) with
  | None ->
      astate
  | Some itv ->
      AddressAttributes.add_one unop_addr (BoItv itv) astate


let eval_unop location unop addr unop_hist astate =
  let unop_addr = AbstractValue.mk_fresh () in
  let astate =
    eval_unop_arith location unop_addr unop addr unop_hist astate
    |> eval_unop_bo_itv unop_addr unop addr
  in
  (astate, (unop_addr, unop_hist))


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


let eval_operand location astate = function
  | LiteralOperand i ->
      ( None
      , Some
          (CItv.equal_to i, Trace.Immediate {location; history= [ValueHistory.Assignment location]})
      , Itv.ItvPure.of_int_lit i )
  | AbstractValueOperand v ->
      (Some v, AddressAttributes.get_citv v astate, AddressAttributes.get_bo_itv v astate)


let record_abduced event location addr_opt orig_arith_hist_opt arith_opt astate =
  match Option.both addr_opt arith_opt with
  | None ->
      astate
  | Some (addr, arith) ->
      let trace =
        match orig_arith_hist_opt with
        | None ->
            Trace.Immediate {location; history= [event]}
        | Some (_, trace) ->
            Trace.add_event event trace
      in
      let attribute = Attribute.CItv (arith, trace) in
      AddressAttributes.abduce_attribute addr attribute astate
      |> AddressAttributes.add_one addr attribute


let bind_satisfiable ~satisfiable astate ~f = if satisfiable then f astate else (astate, false)

let prune_binop ~is_then_branch if_kind location ~negated bop lhs_op rhs_op astate =
  let value_lhs_opt, arith_lhs_opt, bo_itv_lhs = eval_operand location astate lhs_op in
  let value_rhs_opt, arith_rhs_opt, bo_itv_rhs = eval_operand location astate rhs_op in
  match
    CItv.abduce_binop_is_true ~negated bop (Option.map ~f:fst arith_lhs_opt)
      (Option.map ~f:fst arith_rhs_opt)
  with
  | Unsatisfiable ->
      (astate, false)
  | Satisfiable (abduced_lhs, abduced_rhs) ->
      let event = ValueHistory.Conditional {is_then_branch; if_kind; location} in
      let astate =
        record_abduced event location value_lhs_opt arith_lhs_opt abduced_lhs astate
        |> record_abduced event location value_rhs_opt arith_rhs_opt abduced_rhs
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
  ( AddressAttributes.get_citv v astate
  |> function Some (arith, _) -> CItv.is_equal_to_zero arith | None -> false )
  || Itv.ItvPure.is_zero (AddressAttributes.get_bo_itv v astate)
