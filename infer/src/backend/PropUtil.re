/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;

let get_name_of_local (curr_f: Procdesc.t) (x, _) => Pvar.mk x (Procdesc.get_proc_name curr_f);

/* returns a list of local static variables (ie local variables defined static) in a proposition */
let get_name_of_objc_static_locals (curr_f: Procdesc.t) p => {
  let pname = Procname.to_string (Procdesc.get_proc_name curr_f);
  let local_static e =>
    switch e {
    /* is a local static if it's a global and it has a static local name */
    | Exp.Lvar pvar when Pvar.is_global pvar && Sil.is_static_local_name pname pvar => [pvar]
    | _ => []
    };
  let hpred_local_static hpred =>
    switch hpred {
    | Sil.Hpointsto e _ _ => [local_static e]
    | _ => []
    };
  let vars_sigma = List.map f::hpred_local_static p.Prop.sigma;
  List.concat (List.concat vars_sigma)
};

/* returns a list of local variables that points to an objc block in a proposition */
let get_name_of_objc_block_locals p => {
  let local_blocks e =>
    switch e {
    | Exp.Lvar pvar when Sil.is_block_pvar pvar => [pvar]
    | _ => []
    };
  let hpred_local_blocks hpred =>
    switch hpred {
    | Sil.Hpointsto e _ _ => [local_blocks e]
    | _ => []
    };
  let vars_sigma = List.map f::hpred_local_blocks p.Prop.sigma;
  List.concat (List.concat vars_sigma)
};

let remove_abduced_retvars tenv p => {
  /* compute the hpreds and pure atoms reachable from the set of seed expressions in [exps] */
  let compute_reachable p seed_exps => {
    let (sigma, pi) = (p.Prop.sigma, p.Prop.pi);
    let rec collect_exps exps =>
      fun
      | Sil.Eexp (Exp.Exn e) _ => Exp.Set.add e exps
      | Sil.Eexp e _ => Exp.Set.add e exps
      | Sil.Estruct flds _ =>
        List.fold f::(fun exps (_, strexp) => collect_exps exps strexp) init::exps flds
      | Sil.Earray _ elems _ =>
        List.fold f::(fun exps (_, strexp) => collect_exps exps strexp) init::exps elems;
    let rec compute_reachable_hpreds_rec sigma (reach, exps) => {
      let add_hpred_if_reachable (reach, exps) =>
        fun
        | Sil.Hpointsto lhs rhs _ as hpred when Exp.Set.mem lhs exps => {
            let reach' = Sil.HpredSet.add hpred reach;
            let exps' = collect_exps exps rhs;
            (reach', exps')
          }
        | Sil.Hlseg _ _ exp1 exp2 exp_l as hpred => {
            let reach' = Sil.HpredSet.add hpred reach;
            let exps' =
              List.fold
                f::(fun exps_acc exp => Exp.Set.add exp exps_acc)
                init::exps
                [exp1, exp2, ...exp_l];
            (reach', exps')
          }
        | Sil.Hdllseg _ _ exp1 exp2 exp3 exp4 exp_l as hpred => {
            let reach' = Sil.HpredSet.add hpred reach;
            let exps' =
              List.fold
                f::(fun exps_acc exp => Exp.Set.add exp exps_acc)
                init::exps
                [exp1, exp2, exp3, exp4, ...exp_l];
            (reach', exps')
          }
        | _ => (reach, exps);
      let (reach', exps') = List.fold f::add_hpred_if_reachable init::(reach, exps) sigma;
      if (Int.equal (Sil.HpredSet.cardinal reach) (Sil.HpredSet.cardinal reach')) {
        (reach, exps)
      } else {
        compute_reachable_hpreds_rec sigma (reach', exps')
      }
    };
    let (reach_hpreds, reach_exps) =
      compute_reachable_hpreds_rec sigma (Sil.HpredSet.empty, seed_exps);
    /* filter away the pure atoms without reachable exps */
    let reach_pi = {
      let rec exp_contains =
        fun
        | exp when Exp.Set.mem exp reach_exps => true
        | Exp.UnOp _ e _
        | Exp.Cast _ e
        | Exp.Lfield e _ _ => exp_contains e
        | Exp.BinOp _ e0 e1
        | Exp.Lindex e0 e1 => exp_contains e0 || exp_contains e1
        | _ => false;
      List.filter
        f::(
          fun
          | Sil.Aeq lhs rhs
          | Sil.Aneq lhs rhs => exp_contains lhs || exp_contains rhs
          | Sil.Apred _ es
          | Sil.Anpred _ es => List.exists f::exp_contains es
        )
        pi
    };
    (Sil.HpredSet.elements reach_hpreds, reach_pi)
  };
  /* separate the abduced pvars from the normal ones, deallocate the abduced ones*/
  let (abduceds, normal_pvars) =
    List.fold
      f::(
        fun pvars hpred =>
          switch hpred {
          | Sil.Hpointsto (Exp.Lvar pvar) _ _ =>
            let (abduceds, normal_pvars) = pvars;
            if (Pvar.is_abduced pvar) {
              ([pvar, ...abduceds], normal_pvars)
            } else {
              (abduceds, [pvar, ...normal_pvars])
            }
          | _ => pvars
          }
      )
      init::([], [])
      p.Prop.sigma;
  let (_, p') = Attribute.deallocate_stack_vars tenv p abduceds;
  let normal_pvar_set =
    List.fold
      f::(fun normal_pvar_set pvar => Exp.Set.add (Exp.Lvar pvar) normal_pvar_set)
      init::Exp.Set.empty
      normal_pvars;
  /* walk forward from non-abduced pvars, keep everything reachable. remove everything else */
  let (sigma_reach, pi_reach) = compute_reachable p' normal_pvar_set;
  Prop.normalize tenv (Prop.set p' pi::pi_reach sigma::sigma_reach)
};

let remove_locals tenv (curr_f: Procdesc.t) p => {
  let names_of_locals = List.map f::(get_name_of_local curr_f) (Procdesc.get_locals curr_f);
  let names_of_locals' =
    switch !Config.curr_language {
    | Config.Clang =>
      /* in ObjC to deal with block we need to remove static locals */
      let names_of_static_locals = get_name_of_objc_static_locals curr_f p;
      let names_of_block_locals = get_name_of_objc_block_locals p;
      names_of_block_locals @ names_of_locals @ names_of_static_locals
    | _ => names_of_locals
    };
  let (removed, p') = Attribute.deallocate_stack_vars tenv p names_of_locals';
  (
    removed,
    if Config.angelic_execution {
      remove_abduced_retvars tenv p'
    } else {
      p'
    }
  )
};

let remove_formals tenv (curr_f: Procdesc.t) p => {
  let pname = Procdesc.get_proc_name curr_f;
  let formal_vars = List.map f::(fun (n, _) => Pvar.mk n pname) (Procdesc.get_formals curr_f);
  Attribute.deallocate_stack_vars tenv p formal_vars
};


/** remove the return variable from the prop */
let remove_ret tenv (curr_f: Procdesc.t) (p: Prop.t Prop.normal) => {
  let pname = Procdesc.get_proc_name curr_f;
  let name_of_ret = Procdesc.get_ret_var curr_f;
  let (_, p') = Attribute.deallocate_stack_vars tenv p [Pvar.to_callee pname name_of_ret];
  p'
};


/** remove locals and return variable from the prop */
let remove_locals_ret tenv (curr_f: Procdesc.t) p => snd (
  remove_locals tenv curr_f (remove_ret tenv curr_f p)
);


/** Remove locals and formal parameters from the prop.
    Return the list of stack variables whose address was still present after deallocation. */
let remove_locals_formals tenv (curr_f: Procdesc.t) p => {
  let (pvars1, p1) = remove_formals tenv curr_f p;
  let (pvars2, p2) = remove_locals tenv curr_f p1;
  (pvars1 @ pvars2, p2)
};


/** remove seed vars from a prop */
let remove_seed_vars tenv (prop: Prop.t 'a) :Prop.t Prop.normal => {
  let hpred_not_seed =
    fun
    | Sil.Hpointsto (Exp.Lvar pv) _ _ => not (Pvar.is_seed pv)
    | _ => true;
  let sigma = prop.sigma;
  let sigma' = List.filter f::hpred_not_seed sigma;
  Prop.normalize tenv (Prop.set prop sigma::sigma')
};
