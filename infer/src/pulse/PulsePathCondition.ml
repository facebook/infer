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
module CItv = PulseCItv
module Formula = PulseFormula
module ValueHistory = PulseValueHistory

module BoItvs = struct
  include PrettyPrintable.MakePPMonoMap (AbstractValue) (Itv.ItvPure)

  let find_or_default (v : AbstractValue.t) bo_itvs =
    match find_opt v bo_itvs with
    | Some bo_itv ->
        bo_itv
    | None ->
        Itv.ItvPure.of_foreign_id (v :> int)
end

module CItvs = PrettyPrintable.MakePPMonoMap (AbstractValue) (CItv)

(** A mash-up of several arithmetic domains. At the moment they are independent, i.e. we don't use
    facts deduced by one domain to inform another. *)
type t =
  { is_unsat: bool
        (** If [false] then [formula] could still be unsatisfiable (asking that question is
            expensive).

            If [true] then the other components of the record can be arbitrary. *)
  ; bo_itvs: BoItvs.t
  ; citvs: CItvs.t
  ; formula: Formula.t }

let pp fmt {is_unsat; bo_itvs; citvs; formula} =
  F.fprintf fmt "@[<hv>unsat:%b,@;bo: @[%a@],@;citv: @[%a@],@;formula: @[%a@]@]" is_unsat BoItvs.pp
    bo_itvs CItvs.pp citvs Formula.pp formula


let true_ = {is_unsat= false; bo_itvs= BoItvs.empty; citvs= CItvs.empty; formula= Formula.ttrue}

let false_ = {is_unsat= true; bo_itvs= BoItvs.empty; citvs= CItvs.empty; formula= Formula.ttrue}

let and_nonnegative v ({is_unsat; bo_itvs; citvs; formula} as phi) =
  if is_unsat then phi
  else
    { is_unsat
    ; bo_itvs= BoItvs.add v Itv.ItvPure.nat bo_itvs
    ; citvs= CItvs.add v CItv.zero_inf citvs
    ; formula= Formula.(aand (mk_less_equal Term.zero (Term.of_absval v)) formula) }


let and_positive v ({is_unsat; bo_itvs; citvs; formula} as phi) =
  if is_unsat then phi
  else
    { is_unsat
    ; bo_itvs= BoItvs.add v Itv.ItvPure.pos bo_itvs
    ; citvs= CItvs.add v (CItv.ge_to IntLit.one) citvs
    ; formula= Formula.(aand (mk_less_than Term.zero (Term.of_absval v)) formula) }


let and_eq_int v i ({is_unsat; bo_itvs; citvs; formula} as phi) =
  if is_unsat then phi
  else
    { is_unsat
    ; bo_itvs= BoItvs.add v (Itv.ItvPure.of_int_lit i) bo_itvs
    ; citvs= CItvs.add v (CItv.equal_to i) citvs
    ; formula= Formula.(aand (mk_equal (Term.of_absval v) (Term.of_intlit i)) formula) }


let simplify ~keep {is_unsat; bo_itvs; citvs; formula} =
  if is_unsat then false_
  else
    let is_in_keep v _ = AbstractValue.Set.mem v keep in
    let formula = Formula.simplify ~keep formula in
    { is_unsat= is_unsat || Formula.is_literal_false formula
    ; bo_itvs= BoItvs.filter is_in_keep bo_itvs
    ; citvs= CItvs.filter is_in_keep citvs
    ; formula }


let subst_find_or_new subst addr_callee =
  match AbstractValue.Map.find_opt addr_callee subst with
  | None ->
      let addr_hist_fresh = (AbstractValue.mk_fresh (), []) in
      (AbstractValue.Map.add addr_callee addr_hist_fresh subst, fst addr_hist_fresh)
  | Some addr_hist_caller ->
      (subst, fst addr_hist_caller)


let eval_sym_of_subst bo_itvs subst s bound_end =
  let v = Symb.Symbol.get_foreign_id_exn s |> AbstractValue.of_id in
  match AbstractValue.Map.find_opt v !subst with
  | Some (v', _) ->
      Itv.ItvPure.get_bound (BoItvs.find_or_default v' bo_itvs) bound_end
  | None ->
      let v' = AbstractValue.mk_fresh () in
      subst := AbstractValue.Map.add v (v', []) !subst ;
      Bounds.Bound.of_foreign_id (v' :> int)


exception Contradiction

(* TODO: this doesn't actually do "and" (it doesn't even take the caller interval into account) *)
let and_bo_itv_callee bo_itvs subst_ref itv_callee =
  match
    Itv.ItvPure.subst itv_callee (fun symb bound ->
        AbstractDomain.Types.NonBottom (eval_sym_of_subst bo_itvs subst_ref symb bound) )
  with
  | NonBottom itv' ->
      itv'
  | Bottom ->
      raise Contradiction


let and_bo_itvs_callee subst bo_itvs_caller bo_itvs_callee =
  (* first translate callee keys into caller values *)
  let subst, bo_itvs_callee_renamed =
    BoItvs.fold
      (fun v_callee bo_itv (subst, bo_itvs) ->
        let subst, v_caller = subst_find_or_new subst v_callee in
        (* TODO: it could be that the same value already had a binding; in that case we want to
           "and" the intervals *)
        let bo_itvs = BoItvs.add v_caller bo_itv bo_itvs in
        (subst, bo_itvs) )
      bo_itvs_callee (subst, BoItvs.empty)
  in
  let subst_ref = ref subst in
  let bo_itvs' =
    BoItvs.merge
      (fun _v_caller bo_itv bo_itv_callee ->
        match (bo_itv, bo_itv_callee) with
        | None, None ->
            None
        | Some _, None ->
            bo_itv
        | _, Some bo_itv_callee ->
            Some (and_bo_itv_callee bo_itvs_caller subst_ref bo_itv_callee) )
      bo_itvs_caller bo_itvs_callee_renamed
  in
  (!subst_ref, bo_itvs')


let and_citv_callee citv_caller citv_callee =
  match CItv.abduce_binop_is_true ~negated:false Eq (Some citv_caller) (Some citv_callee) with
  | Unsatisfiable ->
      raise Contradiction
  | Satisfiable (Some abduce_caller, _abduce_callee) ->
      abduce_caller
  | Satisfiable (None, _) ->
      citv_caller


let and_citvs_callee subst citvs_caller citvs_callee =
  let subst, citvs_callee_renamed =
    CItvs.fold
      (fun v_callee citv (subst, citvs) ->
        let subst, v_caller = subst_find_or_new subst v_callee in
        (* TODO: it could be that the same value already had a binding if several variables from the
           callee map to the same caller variable; in that case we want to "and" the intervals *)
        let citvs = CItvs.add v_caller citv citvs in
        (subst, citvs) )
      citvs_callee (subst, CItvs.empty)
  in
  let citvs' =
    CItvs.union
      (fun _v citv citv_callee -> Some (and_citv_callee citv citv_callee))
      citvs_caller citvs_callee_renamed
  in
  (subst, citvs')


let and_formula_callee subst formula_caller ~callee:formula_callee =
  (* need to translate callee variables to make sense for the caller, thereby possibly extending
     the current substitution *)
  let subst, formula_callee_translated =
    Formula.fold_map_variables formula_callee ~init:subst ~f:subst_find_or_new
  in
  L.d_printfln "translated callee formula: %a@\n" Formula.pp formula_callee_translated ;
  (subst, Formula.aand formula_caller formula_callee_translated)


let and_callee subst phi ~callee:phi_callee =
  if phi.is_unsat || phi_callee.is_unsat then (subst, false_)
  else
    match and_bo_itvs_callee subst phi.bo_itvs phi_callee.bo_itvs with
    | exception Contradiction ->
        L.d_printfln "contradiction found by inferbo intervals" ;
        (subst, false_)
    | subst, bo_itvs' -> (
      match and_citvs_callee subst phi.citvs phi_callee.citvs with
      | exception Contradiction ->
          L.d_printfln "contradiction found by concrete intervals" ;
          (subst, false_)
      | subst, citvs' ->
          let subst, formula' = and_formula_callee subst phi.formula ~callee:phi_callee.formula in
          L.d_printfln "conjoined formula post call: %a@\n" Formula.pp formula' ;
          let formula' = Formula.normalize formula' in
          let is_unsat = Formula.is_literal_false formula' in
          if is_unsat then L.d_printfln "contradiction found by formulas" ;
          (subst, {is_unsat; bo_itvs= bo_itvs'; citvs= citvs'; formula= formula'}) )


(** {2 Operations} *)

type operand = LiteralOperand of IntLit.t | AbstractValueOperand of AbstractValue.t

let eval_citv_binop binop_addr bop op_lhs op_rhs citvs =
  let citv_of_op op citvs =
    match op with
    | LiteralOperand i ->
        Some (CItv.equal_to i)
    | AbstractValueOperand v ->
        CItvs.find_opt v citvs
  in
  match
    Option.both (citv_of_op op_lhs citvs) (citv_of_op op_rhs citvs)
    |> Option.bind ~f:(fun (addr_lhs, addr_rhs) -> CItv.binop bop addr_lhs addr_rhs)
  with
  | None ->
      citvs
  | Some binop_a ->
      CItvs.add binop_addr binop_a citvs


let eval_bo_itv_binop binop_addr bop op_lhs op_rhs bo_itvs =
  let bo_itv_of_op op bo_itvs =
    match op with
    | LiteralOperand i ->
        Itv.ItvPure.of_int_lit i
    | AbstractValueOperand v ->
        BoItvs.find_or_default v bo_itvs
  in
  let bo_itv =
    Itv.ItvPure.arith_binop bop (bo_itv_of_op op_lhs bo_itvs) (bo_itv_of_op op_rhs bo_itvs)
  in
  BoItvs.add binop_addr bo_itv bo_itvs


let eval_formula_binop binop_addr binop op_lhs op_rhs formula =
  let open Formula in
  let term_of_op = function
    | LiteralOperand i ->
        Term.of_intlit i
    | AbstractValueOperand v ->
        Term.of_absval v
  in
  let t_binop = Term.of_binop binop (term_of_op op_lhs) (term_of_op op_rhs) in
  aand (mk_equal (Term.of_absval binop_addr) t_binop) formula


let eval_binop binop_addr binop op_lhs op_rhs ({is_unsat; bo_itvs; citvs; formula} as phi) =
  if phi.is_unsat then phi
  else
    { is_unsat
    ; bo_itvs= eval_bo_itv_binop binop_addr binop op_lhs op_rhs bo_itvs
    ; citvs= eval_citv_binop binop_addr binop op_lhs op_rhs citvs
    ; formula= eval_formula_binop binop_addr binop op_lhs op_rhs formula }


let eval_citv_unop unop_addr unop operand_addr citvs =
  match CItvs.find_opt operand_addr citvs |> Option.bind ~f:(fun a -> CItv.unop unop a) with
  | None ->
      citvs
  | Some unop_a ->
      CItvs.add unop_addr unop_a citvs


let eval_bo_itv_unop unop_addr unop operand_addr bo_itvs =
  let op_itv = BoItvs.find_or_default operand_addr bo_itvs in
  match Itv.ItvPure.arith_unop unop op_itv with
  | None ->
      bo_itvs
  | Some itv ->
      BoItvs.add unop_addr itv bo_itvs


let eval_formula_unop unop_addr (unop : Unop.t) addr formula =
  let open Formula in
  let t_unop = Term.of_unop unop (Term.of_absval addr) in
  aand (mk_equal (Term.of_absval unop_addr) t_unop) formula


let eval_unop unop_addr unop addr ({is_unsat; bo_itvs; citvs; formula} as phi) =
  if phi.is_unsat then phi
  else
    { is_unsat
    ; bo_itvs= eval_bo_itv_unop unop_addr unop addr bo_itvs
    ; citvs= eval_citv_unop unop_addr unop addr citvs
    ; formula= eval_formula_unop unop_addr unop addr formula }


let prune_bo_with_bop ~negated v_opt arith bop arith' phi =
  match
    Option.both v_opt (if negated then Binop.negate bop else Some bop)
    |> Option.map ~f:(fun (v, positive_bop) ->
           (v, Itv.ItvPure.prune_binop positive_bop arith arith') )
  with
  | None ->
      phi
  | Some (_, Bottom) ->
      {phi with is_unsat= true}
  | Some (v, NonBottom arith_pruned) ->
      {phi with bo_itvs= BoItvs.add v arith_pruned phi.bo_itvs}


let eval_operand phi = function
  | LiteralOperand i ->
      (None, Some (CItv.equal_to i), Itv.ItvPure.of_int_lit i, Formula.Term.of_intlit i)
  | AbstractValueOperand v ->
      ( Some v
      , CItvs.find_opt v phi.citvs
      , BoItvs.find_or_default v phi.bo_itvs
      , Formula.Term.of_absval v )


let record_citv_abduced addr_opt arith_opt citvs =
  match Option.both addr_opt arith_opt with
  | None ->
      citvs
  | Some (addr, arith) ->
      CItvs.add addr arith citvs


let bind_is_unsat phi ~f = if phi.is_unsat then phi else f phi

let prune_binop ~negated bop lhs_op rhs_op ({is_unsat; bo_itvs= _; citvs; formula} as phi) =
  if is_unsat then phi
  else
    let value_lhs_opt, arith_lhs_opt, bo_itv_lhs, t_lhs = eval_operand phi lhs_op in
    let value_rhs_opt, arith_rhs_opt, bo_itv_rhs, t_rhs = eval_operand phi rhs_op in
    match CItv.abduce_binop_is_true ~negated bop arith_lhs_opt arith_rhs_opt with
    | Unsatisfiable ->
        L.d_printfln "contradiction detected by concrete intervals" ;
        false_
    | Satisfiable (abduced_lhs, abduced_rhs) ->
        let phi =
          let citvs =
            record_citv_abduced value_lhs_opt abduced_lhs citvs
            |> record_citv_abduced value_rhs_opt abduced_rhs
          in
          {phi with citvs}
        in
        let is_unsat =
          match Itv.ItvPure.arith_binop bop bo_itv_lhs bo_itv_rhs |> Itv.ItvPure.to_boolean with
          | False ->
              not negated
          | True ->
              negated
          | Top ->
              false
          | Bottom ->
              true
        in
        if is_unsat then L.d_printfln "contradiction detected by inferbo intervals" ;
        let phi = {phi with is_unsat} in
        let phi =
          bind_is_unsat phi ~f:(fun phi ->
              prune_bo_with_bop ~negated value_lhs_opt bo_itv_lhs bop bo_itv_rhs phi )
        in
        let phi =
          Option.value_map (Binop.symmetric bop) ~default:phi ~f:(fun bop' ->
              bind_is_unsat phi ~f:(fun phi ->
                  prune_bo_with_bop ~negated value_rhs_opt bo_itv_rhs bop' bo_itv_lhs phi ) )
        in
        if phi.is_unsat then phi
        else
          let f_positive = Formula.of_term_binop bop t_lhs t_rhs in
          let formula =
            let f = if negated then Formula.nnot f_positive else f_positive in
            Formula.aand f formula |> Formula.normalize
          in
          let is_unsat = Formula.is_literal_false formula in
          if is_unsat then L.d_printfln "contradiction detected by formulas" ;
          {phi with is_unsat; formula}


(** {2 Queries} *)

let is_known_zero phi v =
  (* TODO: ask [Formula] too *)
  CItvs.find_opt v phi.citvs |> Option.value_map ~default:false ~f:CItv.is_equal_to_zero
  || BoItvs.find_opt v phi.bo_itvs |> Option.value_map ~default:false ~f:Itv.ItvPure.is_zero


let is_unsat_cheap phi = phi.is_unsat || Formula.is_literal_false phi.formula

let is_unsat_expensive phi =
  (* note: contradictions are detected eagerly for all sub-domains except formula, so just
     evaluate that one *)
  if is_unsat_cheap phi then (phi, true)
  else
    let formula = Formula.normalize phi.formula in
    let is_unsat = Formula.is_literal_false formula in
    let phi = {phi with is_unsat; formula} in
    (phi, is_unsat)
