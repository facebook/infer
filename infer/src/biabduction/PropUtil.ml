(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let get_name_of_local (curr_f : Procdesc.t) (var_data : ProcAttributes.var_data) =
  Pvar.mk var_data.name (Procdesc.get_proc_name curr_f)


(* returns a list of local static variables (ie local variables defined static) in a proposition *)
let get_name_of_objc_static_locals (curr_f : Procdesc.t) p =
  let pname = Procname.to_string (Procdesc.get_proc_name curr_f) in
  let local_static e =
    match e with
    | Exp.Lvar pvar
      when Pvar.is_global pvar && Pvar.is_objc_static_local_of_proc_name pname pvar
           (* is a local static if it's a global and it has a static local name *) ->
        [pvar]
    | _ ->
        []
  in
  let hpred_local_static hpred =
    match hpred with Predicates.Hpointsto (e, _, _) -> [local_static e] | _ -> []
  in
  let vars_sigma = List.map ~f:hpred_local_static p.Prop.sigma in
  List.concat (List.concat vars_sigma)


(* returns a list of local variables that points to an objc block in a proposition *)
let get_name_of_objc_block_locals p =
  let local_blocks e =
    match e with Exp.Lvar pvar when Pvar.is_block_pvar pvar -> [pvar] | _ -> []
  in
  let hpred_local_blocks hpred =
    match hpred with Predicates.Hpointsto (e, _, _) -> [local_blocks e] | _ -> []
  in
  let vars_sigma = List.map ~f:hpred_local_blocks p.Prop.sigma in
  List.concat (List.concat vars_sigma)


let remove_abduced_retvars tenv p =
  (* compute the hpreds and pure atoms reachable from the set of seed expressions in [exps] *)
  let compute_reachable p seed_exps =
    let sigma, pi = (p.Prop.sigma, p.Prop.pi) in
    let rec collect_exps exps (sexp : Predicates.strexp) =
      match sexp with
      | Eexp (Exp.Exn e, _) ->
          Exp.Set.add e exps
      | Eexp (e, _) ->
          Exp.Set.add e exps
      | Estruct (flds, _) ->
          List.fold ~f:(fun exps (_, strexp) -> collect_exps exps strexp) ~init:exps flds
      | Earray (_, elems, _) ->
          List.fold ~f:(fun exps (_, strexp) -> collect_exps exps strexp) ~init:exps elems
    in
    let rec compute_reachable_hpreds_rec sigma (reach, exps) =
      let add_hpred_if_reachable (reach, exps) (hpred : Predicates.hpred) =
        match hpred with
        | Hpointsto (lhs, rhs, _) as hpred when Exp.Set.mem lhs exps ->
            let reach' = Predicates.HpredSet.add hpred reach in
            let exps' = collect_exps exps rhs in
            (reach', exps')
        | Hlseg (_, _, exp1, exp2, exp_l) as hpred ->
            let reach' = Predicates.HpredSet.add hpred reach in
            let exps' =
              List.fold
                ~f:(fun exps_acc exp -> Exp.Set.add exp exps_acc)
                ~init:exps (exp1 :: exp2 :: exp_l)
            in
            (reach', exps')
        | Hdllseg (_, _, exp1, exp2, exp3, exp4, exp_l) as hpred ->
            let reach' = Predicates.HpredSet.add hpred reach in
            let exps' =
              List.fold
                ~f:(fun exps_acc exp -> Exp.Set.add exp exps_acc)
                ~init:exps
                (exp1 :: exp2 :: exp3 :: exp4 :: exp_l)
            in
            (reach', exps')
        | _ ->
            (reach, exps)
      in
      let reach', exps' = List.fold ~f:add_hpred_if_reachable ~init:(reach, exps) sigma in
      if Int.equal (Predicates.HpredSet.cardinal reach) (Predicates.HpredSet.cardinal reach') then
        (reach, exps)
      else compute_reachable_hpreds_rec sigma (reach', exps')
    in
    let reach_hpreds, reach_exps =
      compute_reachable_hpreds_rec sigma (Predicates.HpredSet.empty, seed_exps)
    in
    (* filter away the pure atoms without reachable exps *)
    let reach_pi =
      let rec exp_contains = function
        | exp when Exp.Set.mem exp reach_exps ->
            true
        | Exp.UnOp (_, e, _) | Exp.Cast (_, e) | Exp.Lfield (e, _, _) ->
            exp_contains e
        | Exp.BinOp (_, e0, e1) | Exp.Lindex (e0, e1) ->
            exp_contains e0 || exp_contains e1
        | _ ->
            false
      in
      List.filter
        ~f:(function
          | Predicates.Aeq (lhs, rhs) | Predicates.Aneq (lhs, rhs) ->
              exp_contains lhs || exp_contains rhs
          | Predicates.Apred (_, es) | Predicates.Anpred (_, es) ->
              List.exists ~f:exp_contains es )
        pi
    in
    (Predicates.HpredSet.elements reach_hpreds, reach_pi)
  in
  (* separate the abduced pvars from the normal ones, deallocate the abduced ones*)
  let abduceds, normal_pvars =
    List.fold
      ~f:(fun pvars hpred ->
        match hpred with
        | Predicates.Hpointsto (Exp.Lvar pvar, _, _) ->
            let abduceds, normal_pvars = pvars in
            if Pvar.is_abduced pvar then (pvar :: abduceds, normal_pvars)
            else (abduceds, pvar :: normal_pvars)
        | _ ->
            pvars )
      ~init:([], []) p.Prop.sigma
  in
  let _, p' = Attribute.deallocate_stack_vars tenv p abduceds in
  let normal_pvar_set =
    List.fold
      ~f:(fun normal_pvar_set pvar -> Exp.Set.add (Exp.Lvar pvar) normal_pvar_set)
      ~init:Exp.Set.empty normal_pvars
  in
  (* walk forward from non-abduced pvars, keep everything reachable. remove everything else *)
  let sigma_reach, pi_reach = compute_reachable p' normal_pvar_set in
  Prop.normalize tenv (Prop.set p' ~pi:pi_reach ~sigma:sigma_reach)


let remove_locals tenv (curr_f : Procdesc.t) p =
  let names_of_locals = List.map ~f:(get_name_of_local curr_f) (Procdesc.get_locals curr_f) in
  let names_of_locals' =
    match !Language.curr_language with
    | Language.Clang ->
        (* in ObjC to deal with block we need to remove static locals *)
        let names_of_static_locals = get_name_of_objc_static_locals curr_f p in
        let names_of_block_locals = get_name_of_objc_block_locals p in
        names_of_block_locals @ names_of_locals @ names_of_static_locals
    | _ ->
        names_of_locals
  in
  let removed, p' = Attribute.deallocate_stack_vars tenv p names_of_locals' in
  (removed, remove_abduced_retvars tenv p')


let remove_formals tenv (curr_f : Procdesc.t) p =
  let pname = Procdesc.get_proc_name curr_f in
  let formal_vars = List.map ~f:(fun (n, _, _) -> Pvar.mk n pname) (Procdesc.get_formals curr_f) in
  Attribute.deallocate_stack_vars tenv p formal_vars


(** remove the return variable from the prop *)
let remove_ret tenv (curr_f : Procdesc.t) (p : Prop.normal Prop.t) =
  let pname = Procdesc.get_proc_name curr_f in
  let name_of_ret = Procdesc.get_ret_var curr_f in
  let _, p' = Attribute.deallocate_stack_vars tenv p [Pvar.to_callee pname name_of_ret] in
  p'


(** remove locals and return variable from the prop *)
let remove_locals_ret tenv (curr_f : Procdesc.t) p =
  snd (remove_locals tenv curr_f (remove_ret tenv curr_f p))


(** Remove locals and formal parameters from the prop. Return the list of stack variables whose
    address was still present after deallocation. *)
let remove_locals_formals tenv (curr_f : Procdesc.t) p =
  let pvars1, p1 = remove_locals tenv curr_f p in
  let pvars2, p2 = remove_formals tenv curr_f p1 in
  (pvars1 @ pvars2, p2)


(** remove seed vars from a prop *)
let remove_seed_vars tenv (prop : 'a Prop.t) : Prop.normal Prop.t =
  let hpred_not_seed = function
    | Predicates.Hpointsto (Exp.Lvar pv, _, _) ->
        not (Pvar.is_seed pv)
    | _ ->
        true
  in
  let sigma = prop.sigma in
  let sigma' = List.filter ~f:hpred_not_seed sigma in
  Prop.normalize tenv (Prop.set prop ~sigma:sigma')
