(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module InvariantVars = AbstractDomain.FiniteSet (Var)
module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module Models = InvariantModels

(** Map loop header node -> all nodes in the loop *)
module LoopHeadToLoopNodes = Procdesc.NodeMap

let is_defined_outside loop_nodes reaching_defs var =
  ReachingDefs.ReachingDefsMap.find_opt var reaching_defs
  |> Option.map ~f:(fun def_nodes -> LoopNodes.inter def_nodes loop_nodes |> LoopNodes.is_empty)
  |> Option.value ~default:true


(* check if the def of var is unique and satisfies function f_exp *)
let is_def_unique_and_satisfy tenv var (loop_nodes: LoopNodes.t) f_exp =
  let equals_var id = Var.equal var (Var.of_id id) in
  (* Use O(1) is_singleton check *)
  (* tedious parameter wrangling to make IContainer's fold interface happy *)
  IContainer.is_singleton
    ~fold:(fun s ~init ~f -> LoopNodes.fold (fun acc x -> f x acc) s init)
    loop_nodes
  && LoopNodes.for_all
       (fun node ->
         Procdesc.Node.get_instrs node
         |> Instrs.exists ~f:(function
              | Sil.Load (id, exp_rhs, _, _) when equals_var id && f_exp exp_rhs ->
                  true
              | Sil.Store (exp_lhs, _, exp_rhs, _)
                when Exp.equal exp_lhs (Var.to_exp var) && f_exp exp_rhs ->
                  true
              | Sil.Call ((id, _), Const (Cfun callee_pname), params, _, _) when equals_var id -> (
                match
                  (* Take into account invariance behavior of modeled
                    functions *)
                  Models.Call.dispatch tenv callee_pname params
                with
                | Some inv ->
                    InvariantModels.is_invariant inv
                    && List.for_all ~f:(fun (exp, _) -> f_exp exp) params
                | None ->
                    Config.cost_invariant_by_default )
              | _ ->
                  false ) )
       loop_nodes


let is_exp_invariant inv_vars loop_nodes reaching_defs exp =
  Var.get_all_vars_in_exp exp
  |> Sequence.for_all ~f:(fun var ->
         InvariantVars.mem var inv_vars || is_defined_outside loop_nodes reaching_defs var )


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
                      ~init:(InvariantVars.add (Var.of_id id) acc)
                      ~f:(fun acc var -> InvariantVars.add var acc)
             | Sil.Store (exp_lhs, _, exp_rhs, _) ->
                 Var.get_all_vars_in_exp exp_rhs
                 |> Sequence.append (Var.get_all_vars_in_exp exp_lhs)
                 |> Sequence.fold ~init:acc ~f:(fun acc var -> InvariantVars.add var acc)
             (* function calls are ignored at the moment  *)
             | _ ->
                 acc )
           ~init:acc )
    loop_nodes InvariantVars.empty


(* A variable is invariant if
     - its reaching definition is outside of the loop
     - o.w. its definition is constant or invariant itself *)
let get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes =
  let process_var_once var inv_vars =
    (* if a variable is marked invariant once, it can't be invalidated
       (i.e. invariance is monotonic) *)
    if InvariantVars.mem var inv_vars || Var.is_none var then (inv_vars, false)
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
                      is_def_unique_and_satisfy tenv var def_nodes
                        (is_exp_invariant inv_vars loop_nodes reaching_defs)
                    then (InvariantVars.add var inv_vars, true)
                    else (inv_vars, false) )
             |> Option.value (* if a var is not declared, it must be invariant *)
                  ~default:(inv_vars, false) )
      |> Option.value ~default:(inv_vars, false)
  in
  let vars_in_loop = get_vars_in_loop loop_nodes in
  (* until there are no changes to inv_vars, keep repeatedly
     processing all the variables that occur in the loop nodes *)
  let rec find_fixpoint inv_vars =
    let inv_vars', modified =
      InvariantVars.fold
        (fun var (inv_vars, is_mod) ->
          let inv_vars', is_mod' = process_var_once var inv_vars in
          (inv_vars', is_mod || is_mod') )
        vars_in_loop (inv_vars, false)
    in
    if modified then find_fixpoint inv_vars' else inv_vars'
  in
  find_fixpoint InvariantVars.empty


(** Map loop head ->  invariant vars in loop  *)
module LoopHeadToInvVars = Procdesc.NodeMap

let get_loop_inv_var_map tenv reaching_defs_invariant_map loop_head_to_loop_nodes =
  LoopHeadToLoopNodes.fold
    (fun loop_head loop_nodes inv_map ->
      let inv_vars_in_loop =
        get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes
      in
      L.(debug Analysis Medium)
        "@\n>>> loop head: %a --> inv vars: %a @\n" Procdesc.Node.pp loop_head InvariantVars.pp
        inv_vars_in_loop ;
      LoopHeadToInvVars.add loop_head inv_vars_in_loop inv_map )
    loop_head_to_loop_nodes LoopHeadToInvVars.empty
