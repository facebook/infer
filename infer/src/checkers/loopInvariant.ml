(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module InvariantVars = AbstractDomain.FiniteSet (Var)
module VarsInLoop = AbstractDomain.FiniteSet (Var)
module InvalidatedVars = AbstractDomain.FiniteSet (Var)
module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module Models = InvariantModels

let debug fmt = L.(debug Analysis Medium) fmt

(** Map loop header node -> all nodes in the loop *)
module LoopHeadToLoopNodes = Procdesc.NodeMap

let is_defined_outside loop_nodes reaching_defs var =
  ReachingDefs.ReachingDefsMap.find_opt var reaching_defs
  |> Option.map ~f:(fun def_nodes -> LoopNodes.inter def_nodes loop_nodes |> LoopNodes.is_empty)
  |> Option.value ~default:true


let is_fun_pure tenv ~is_inv_by_default callee_pname params =
  (* Take into account purity behavior of modeled functions *)
  match Models.Call.dispatch tenv callee_pname params with
  | Some inv ->
      InvariantModels.is_invariant inv
  | None -> (
    (* If there is no model, invoke purity analysis to see if function is pure *)
    match Ondemand.analyze_proc_name callee_pname with
    | Some {Summary.payloads= {Payloads.purity= Some is_pure}} ->
        is_pure
    | _ ->
        is_inv_by_default )


(* check if the def of var is unique and invariant *)
let is_def_unique_and_satisfy tenv var (loop_nodes : LoopNodes.t) ~is_inv_by_default
    is_exp_invariant =
  let equals_var id = Var.equal var (Var.of_id id) in
  (* Use O(1) is_singleton check *)
  (* tedious parameter wrangling to make IContainer's fold interface happy *)
  IContainer.is_singleton
    ~fold:(fun s ~init ~f -> LoopNodes.fold (fun node acc -> f acc node) s init)
    loop_nodes
  && LoopNodes.for_all
       (fun node ->
         Procdesc.Node.get_instrs node
         |> Instrs.exists ~f:(function
              | Sil.Load (id, exp_rhs, _, _) when equals_var id && is_exp_invariant exp_rhs ->
                  true
              | Sil.Store (exp_lhs, _, exp_rhs, _)
                when Exp.equal exp_lhs (Var.to_exp var) && is_exp_invariant exp_rhs ->
                  true
              | Sil.Call ((id, _), Const (Cfun callee_pname), params, _, _) when equals_var id ->
                  is_fun_pure tenv ~is_inv_by_default callee_pname params
                  && (* check if all params are invariant *)
                     List.for_all ~f:(fun (exp, _) -> is_exp_invariant exp) params
              | _ ->
                  false ) )
       loop_nodes


let is_exp_invariant inv_vars invalidated_vars loop_nodes reaching_defs exp =
  Var.get_all_vars_in_exp exp
  |> Sequence.for_all ~f:(fun var ->
         (not (InvalidatedVars.mem var invalidated_vars))
         && (InvariantVars.mem var inv_vars || is_defined_outside loop_nodes reaching_defs var) )


let get_vars_in_loop loop_nodes =
  LoopNodes.fold
    (fun node acc ->
      Procdesc.Node.get_instrs node
      |> Instrs.fold
           ~f:(fun acc instr ->
             match instr with
             | Sil.Load (id, exp_rhs, _, _) ->
                 Var.get_all_vars_in_exp exp_rhs
                 |> Sequence.fold
                      ~init:(VarsInLoop.add (Var.of_id id) acc)
                      ~f:(fun acc var -> VarsInLoop.add var acc)
             | Sil.Store (exp_lhs, _, exp_rhs, _) ->
                 Var.get_all_vars_in_exp exp_rhs
                 |> Sequence.append (Var.get_all_vars_in_exp exp_lhs)
                 |> Sequence.fold ~init:acc ~f:(fun acc var -> VarsInLoop.add var acc)
             | Sil.Call ((ret_id, _), _, args, _, _) ->
                 List.fold
                   ~init:(VarsInLoop.add (Var.of_id ret_id) acc)
                   ~f:(fun acc (arg_exp, _) ->
                     Var.get_all_vars_in_exp arg_exp
                     |> Sequence.fold ~init:acc ~f:(fun acc var -> VarsInLoop.add var acc) )
                   args
             | _ ->
                 acc )
           ~init:acc )
    loop_nodes VarsInLoop.empty


(* get all the ptr variables (and their dependencies) occurring on the
   RHS of the definition of a given variable. *)
let get_ptr_vars_in_defn_path node var =
  let rec aux node var =
    let invalidate_exp exp_rhs init =
      Var.get_all_vars_in_exp exp_rhs
      |> Sequence.fold ~init ~f:(fun acc var ->
             List.fold_left ~init:(InvalidatedVars.add var acc)
               ~f:(fun acc node -> InvalidatedVars.union (aux node var) acc)
               (node :: Procdesc.Node.get_preds node) )
    in
    Procdesc.Node.get_instrs node
    |> Instrs.fold ~init:InvalidatedVars.empty ~f:(fun acc instr ->
           match instr with
           | Sil.Load (id, exp_rhs, typ, _) when Var.equal var (Var.of_id id) && Typ.is_pointer typ
             ->
               invalidate_exp exp_rhs acc
           | Sil.Store (Exp.Lvar pvar, typ, exp_rhs, _)
             when Var.equal var (Var.of_pvar pvar) && Typ.is_pointer typ ->
               invalidate_exp exp_rhs acc
           | _ ->
               acc )
  in
  aux node var


let get_vars_to_invalidate node params invalidated_vars : InvalidatedVars.t =
  List.fold ~init:invalidated_vars
    ~f:(fun acc (arg_exp, typ) ->
      Var.get_all_vars_in_exp arg_exp
      |> Sequence.fold ~init:acc ~f:(fun acc var ->
             if Typ.is_pointer typ then
               let dep_vars = get_ptr_vars_in_defn_path node var in
               InvalidatedVars.union dep_vars (InvalidatedVars.add var acc)
             else acc ) )
    params


(* If there is a call to an impure function in the loop, invalidate
   all its non-primitive arguments. Once invalidated, it should be
   never added again. *)
let get_invalidated_vars_in_loop tenv ~is_inv_by_default loop_nodes =
  LoopNodes.fold
    (fun node acc ->
      Procdesc.Node.get_instrs node
      |> Instrs.fold ~init:acc ~f:(fun acc instr ->
             match instr with
             | Sil.Call ((id, _), Const (Cfun callee_pname), params, _, _)
               when not (is_fun_pure tenv ~is_inv_by_default callee_pname params) ->
                 get_vars_to_invalidate node params (InvalidatedVars.add (Var.of_id id) acc)
             | _ ->
                 acc ) )
    loop_nodes InvalidatedVars.empty


(* A variable is invariant if
     - its reaching definition is outside of the loop
     - o.w. its definition is constant or invariant itself *)
let get_inv_vars_in_loop tenv reaching_defs_invariant_map ~is_inv_by_default loop_head loop_nodes =
  let process_var_once var inv_vars invalidated_vars =
    (* if a variable is marked invariant once, it can't be invalidated
       (i.e. invariance is monotonic) *)
    if
      InvariantVars.mem var inv_vars || Var.is_none var || InvalidatedVars.mem var invalidated_vars
    then (inv_vars, false)
    else
      let loop_head_id = Procdesc.Node.get_id loop_head in
      ReachingDefs.Analyzer.extract_post loop_head_id reaching_defs_invariant_map
      |> Option.map ~f:(fun reaching_defs ->
             ReachingDefs.ReachingDefsMap.find_opt var reaching_defs
             |> Option.map ~f:(fun def_nodes ->
                    let in_loop_defs = LoopNodes.inter loop_nodes def_nodes in
                    (* reaching definition is outside of the loop *)
                    if LoopNodes.is_empty in_loop_defs then (InvariantVars.add var inv_vars, true)
                    else if
                      (* its definition is unique and invariant *)
                      is_def_unique_and_satisfy tenv var def_nodes ~is_inv_by_default
                        (is_exp_invariant inv_vars invalidated_vars loop_nodes reaching_defs)
                    then (InvariantVars.add var inv_vars, true)
                    else (inv_vars, false) )
             |> Option.value (* if a var is not declared, it must be invariant *)
                  ~default:(inv_vars, false) )
      |> Option.value ~default:(inv_vars, false)
  in
  let vars_in_loop = get_vars_in_loop loop_nodes in
  (* until there are no changes to inv_vars, keep repeatedly
     processing all the variables that occur in the loop nodes *)
  let invalidated_vars = get_invalidated_vars_in_loop tenv ~is_inv_by_default loop_nodes in
  let rec find_fixpoint inv_vars =
    let inv_vars', modified =
      InvariantVars.fold
        (fun var (inv_vars, is_mod) ->
          let inv_vars', is_mod' = process_var_once var inv_vars invalidated_vars in
          (inv_vars', is_mod || is_mod') )
        vars_in_loop (inv_vars, false)
    in
    if modified then find_fixpoint inv_vars' else inv_vars'
  in
  debug "\n>>> Invalidated vars: %a\n" InvalidatedVars.pp invalidated_vars ;
  find_fixpoint InvariantVars.empty


(** Map loop head ->  invariant vars in loop  *)
module LoopHeadToInvVars = Procdesc.NodeMap

let get_loop_inv_var_map tenv reaching_defs_invariant_map loop_head_to_loop_nodes =
  LoopHeadToLoopNodes.fold
    (fun loop_head loop_nodes inv_map ->
      let inv_vars_in_loop =
        get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes
          ~is_inv_by_default:Config.cost_invariant_by_default
      in
      L.(debug Analysis Medium)
        "@\n>>> loop head: %a --> inv vars: %a @\n" Procdesc.Node.pp loop_head InvariantVars.pp
        inv_vars_in_loop ;
      LoopHeadToInvVars.add loop_head inv_vars_in_loop inv_map )
    loop_head_to_loop_nodes LoopHeadToInvVars.empty
