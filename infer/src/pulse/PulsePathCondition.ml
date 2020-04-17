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

[@@@warning "+9"]

module Var = struct
  module Var = Sledge.Var

  let of_absval (v : AbstractValue.t) = Var.identified ~name:"v" ~id:(v :> int)

  let to_absval v =
    assert (String.equal (Var.name v) "v") ;
    Var.id v |> AbstractValue.of_id


  include Var
end

module Term = struct
  module Term = Sledge.Term

  let of_intlit i = Term.integer (IntLit.to_big_int i)

  let of_absval v = Term.var (Var.of_absval v)

  let of_unop (unop : Unop.t) t = match unop with Neg -> Term.neg t | BNot | LNot -> Term.not_ t

  let of_binop (binop : Binop.t) t1 t2 =
    let open Term in
    match binop with
    | PlusA _ | PlusPI ->
        add t1 t2
    | MinusA _ | MinusPI | MinusPP ->
        sub t1 t2
    | Mult _ ->
        mul t1 t2
    | Div ->
        div t1 t2
    | Mod ->
        rem t1 t2
    | Shiftlt ->
        shl t1 t2
    | Shiftrt ->
        lshr t1 t2
    | Lt ->
        lt t1 t2
    | Gt ->
        lt t2 t1
    | Le ->
        le t1 t2
    | Ge ->
        le t2 t1
    | Eq ->
        eq t1 t2
    | Ne ->
        dq t1 t2
    | BAnd | LAnd ->
        and_ t1 t2
    | BOr | LOr ->
        or_ t1 t2
    | BXor ->
        xor t1 t2


  include Term
end

module Equality = struct
  include Sledge.Equality

  let assert_no_new_vars api new_vars =
    if not (Var.Set.is_empty new_vars) then
      L.die InternalError "Huho, %s generated fresh new variables %a" api Var.Set.pp new_vars


  let and_eq t1 t2 r =
    let new_vars, r' = Sledge.Equality.and_eq Var.Set.empty t1 t2 r in
    assert_no_new_vars "Equality.and_eq" new_vars ;
    r'


  let and_term t r =
    let new_vars, r' = Sledge.Equality.and_term Var.Set.empty t r in
    assert_no_new_vars "Equality.and_term" new_vars ;
    r'


  let and_ r1 r2 =
    let new_vars, r' = Sledge.Equality.and_ Var.Set.empty r1 r2 in
    assert_no_new_vars "Equality.and_" new_vars ;
    r'


  let apply_subst subst r =
    let new_vars, r' = Sledge.Equality.apply_subst Var.Set.empty subst r in
    assert_no_new_vars "Equality.and_" new_vars ;
    r'
end

(** We distinguish between what the equality relation of sledge can express and the "non-equalities"
    terms that this relation ignores. We keep the latter around for completeness: we can still
    substitute known equalities into these and sometimes get contradictions back. *)
type t = {eqs: Equality.t; non_eqs: Term.t}

let pp fmt {eqs; non_eqs} = F.fprintf fmt "%a∧%a" Equality.pp eqs Term.pp non_eqs

let true_ = {eqs= Equality.true_; non_eqs= Term.true_}

let and_eq t1 t2 phi = {phi with eqs= Equality.and_eq t1 t2 phi.eqs}

let and_term (t : Term.t) phi =
  (* add the term to the relation *)
  let eqs = Equality.and_term t phi.eqs in
  (* [t] normalizes to [true_] so [non_eqs] never changes, do this regardless for now *)
  let non_eqs = Term.and_ phi.non_eqs (Equality.normalize eqs t) in
  {eqs; non_eqs}


let and_ phi1 phi2 =
  {eqs= Equality.and_ phi1.eqs phi2.eqs; non_eqs= Term.and_ phi1.non_eqs phi2.non_eqs}


let is_known_zero t phi = Equality.entails_eq phi.eqs t Term.zero

(* NOTE: not normalizing non_eqs here gives imprecise results but is cheaper *)
let is_unsat {eqs; non_eqs} = Equality.is_false eqs || Term.is_false non_eqs

let fv {eqs; non_eqs} = Term.Var.Set.union (Equality.fv eqs) (Term.fv non_eqs)

let fold_map_variables phi ~init ~f =
  let term_fold_map t ~init ~f =
    Term.fold_map_rec_pre t ~init ~f:(fun acc t ->
        Var.of_term t
        |> Option.map ~f:(fun v ->
               let acc', v' = f acc v in
               (acc', Term.var v') ) )
  in
  let acc, eqs' =
    Equality.classes phi.eqs
    |> Term.Map.fold ~init:(init, Equality.true_) ~f:(fun ~key:t ~data:equal_ts (acc, eqs') ->
           let acc, t' = term_fold_map ~init:acc ~f t in
           List.fold equal_ts ~init:(acc, eqs') ~f:(fun (acc, eqs') equal_t ->
               let acc, t_mapped = term_fold_map ~init:acc ~f equal_t in
               (acc, Equality.and_eq t' t_mapped eqs') ) )
  in
  let acc, non_eqs' = term_fold_map ~init:acc ~f phi.non_eqs in
  (acc, {eqs= eqs'; non_eqs= non_eqs'})


let simplify ~keep phi =
  let all_vs = fv phi in
  let keep_vs =
    AbstractValue.Set.fold
      (fun v keep_vs -> Term.Var.Set.add keep_vs (Var.of_absval v))
      keep Term.Var.Set.empty
  in
  let simpl_subst = Equality.solve_for_vars [keep_vs; all_vs] phi.eqs in
  {phi with eqs= Equality.apply_subst simpl_subst phi.eqs}
