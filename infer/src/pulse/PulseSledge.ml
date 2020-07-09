(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module AbstractValue = PulseAbstractValue

[@@@warning "+9"]

module Var = struct
  module Var = Sledge.Fol.Var

  let of_absval (v : AbstractValue.t) = Var.identified ~name:"v" ~id:(v :> int)

  let to_absval v =
    assert (String.equal (Var.name v) "v") ;
    Var.id v |> AbstractValue.of_id


  include Var
end

module Term = struct
  module Term = Sledge.Fol.Term

  let of_intlit i = Term.integer (IntLit.to_big_int i)

  let of_absval v = Term.var (Var.of_absval v)

  let of_unop (unop : Unop.t) t = match unop with Neg -> Some (Term.neg t) | BNot | LNot -> None

  let of_binop (binop : Binop.t) t1 t2 =
    let open Term in
    match binop with
    | PlusA _ | PlusPI ->
        Some (add t1 t2)
    | MinusA _ | MinusPI | MinusPP ->
        Some (sub t1 t2)
    | Mult _ ->
        Some (mul t1 t2)
    | Div | Mod | Shiftlt | Shiftrt | Lt | Gt | Le | Ge | Eq | Ne | BAnd | LAnd | BOr | LOr | BXor
      ->
        None


  include Term
end

module Formula = struct
  module Formula = Sledge.Fol.Formula

  let term_binop (binop : Binop.t) t1 t2 =
    match binop with
    | BAnd
    | BOr
    | BXor
    | PlusA _
    | PlusPI
    | MinusA _
    | MinusPI
    | MinusPP
    | Mult _
    | Div
    | Mod
    | Shiftlt
    | Shiftrt ->
        Term.of_binop binop t1 t2 |> Option.map ~f:(fun t -> Formula.dq t Term.zero)
    | Lt ->
        Some (Formula.lt t1 t2)
    | Gt ->
        Some (Formula.lt t2 t1)
    | Le ->
        Some (Formula.le t1 t2)
    | Ge ->
        Some (Formula.le t2 t1)
    | Eq ->
        Some (Formula.eq t1 t2)
    | Ne ->
        Some (Formula.dq t1 t2)
    | LAnd ->
        Option.both (Formula.project t1) (Formula.project t2)
        |> Option.map ~f:(fun (f1, f2) -> Formula.and_ f1 f2)
    | LOr ->
        Option.both (Formula.project t1) (Formula.project t2)
        |> Option.map ~f:(fun (f1, f2) -> Formula.or_ f1 f2)


  include Formula
end

module Context = struct
  include Sledge.Fol.Context

  let assert_no_new_vars api new_vars =
    if not (Var.Set.is_empty new_vars) then
      L.die InternalError "Huho, %s generated fresh new variables %a" api Var.Set.pp new_vars


  let and_formula phi r =
    let new_vars, r' = Sledge.Fol.Context.and_formula Var.Set.empty phi r in
    assert_no_new_vars "Context.and_formula" new_vars ;
    r'


  let and_ r1 r2 =
    let new_vars, r' = Sledge.Fol.Context.and_ Var.Set.empty r1 r2 in
    assert_no_new_vars "Context.and_" new_vars ;
    r'


  let apply_subst subst r =
    let new_vars, r' = Sledge.Fol.Context.apply_subst Var.Set.empty subst r in
    assert_no_new_vars "Context.apply_subst" new_vars ;
    r'
end

type t = Context.t lazy_t

let pp fmt (lazy phi) = Context.pp fmt phi

let true_ = Lazy.from_val Context.true_

let and_formula f phi = lazy (Context.and_formula f (Lazy.force phi))

let and_ phi1 phi2 = lazy (Context.and_ (Lazy.force phi1) (Lazy.force phi2))

let is_known_zero t phi = Context.entails_eq (Lazy.force phi) t Term.zero

let is_unsat phi = Context.is_false (Lazy.force phi)

let fv (lazy phi) = Context.fv phi

let fold_map_variables phi ~init ~f =
  let acc, phi' =
    Context.classes (Lazy.force phi)
    |> Term.Map.fold ~init:(init, Context.true_) ~f:(fun ~key:t ~data:equal_ts (acc, phi') ->
           let acc, t' = Term.fold_map_vars ~init:acc ~f t in
           List.fold equal_ts ~init:(acc, phi') ~f:(fun (acc, phi') equal_t ->
               let acc, t_mapped = Term.fold_map_vars ~init:acc ~f equal_t in
               (acc, Context.and_formula (Formula.eq t' t_mapped) phi') ) )
  in
  (acc, Lazy.from_val phi')


let simplify ~keep phi =
  let all_vs = fv phi in
  let keep_vs =
    AbstractValue.Set.fold
      (fun v keep_vs -> Var.Set.add keep_vs (Var.of_absval v))
      keep Var.Set.empty
  in
  let simpl_subst = Context.solve_for_vars [keep_vs; all_vs] (Lazy.force phi) in
  Lazy.from_val (Context.apply_subst simpl_subst (Lazy.force phi))
