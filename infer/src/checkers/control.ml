(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
(* forward dependency analysis for computing set of variables that
   affect the looping behavior of the program 

   1. perform a control flow dependency analysis by getting all the
   variables that appear in the guards of the loops.

   2. for each control dependency per node, find its respective data
   dependency

   3. remove invariant vars in the loop from control vars 
 *)

module VarSet = AbstractDomain.FiniteSet (Var)
module CFG = ProcCfg.Normal
module ControlDepSet = AbstractDomain.FiniteSet (Var)
module GuardNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module LoopHeads = Procdesc.NodeSet

(** Map exit node -> loop head set  *)
module ExitNodeToLoopHeads = Procdesc.NodeMap

(** Map loop head -> prune nodes in the loop guard  *)
module LoopHeadToGuardNodes =
Procdesc.NodeMap

type loop_control_maps =
  { exit_map: LoopHeads.t ExitNodeToLoopHeads.t
  ; loop_head_to_guard_nodes: GuardNodes.t LoopHeadToGuardNodes.t }

(* forward transfer function for control dependencies *)
module TransferFunctionsControlDeps (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ControlDepSet

  type extras = loop_control_maps

  let collect_vars_in_exp exp =
    Var.get_all_vars_in_exp exp
    |> Sequence.fold ~init:ControlDepSet.empty ~f:(fun acc cvar -> ControlDepSet.add cvar acc)


  let find_vars_in_decl id _ = function
    | Sil.Load (lhs_id, exp, _, _) when Ident.equal lhs_id id ->
        collect_vars_in_exp exp |> Option.some
    | Sil.Call ((lhs_id, _), _, arg_list, _, _) when Ident.equal lhs_id id ->
        List.fold_left arg_list ~init:ControlDepSet.empty ~f:(fun deps (exp, _) ->
            collect_vars_in_exp exp |> ControlDepSet.union deps )
        |> Option.some
    | _ ->
        None


  let get_vars_in_exp exp prune_node =
    assert (Exp.program_vars exp |> Sequence.is_empty) ;
    (* We should never have program variables in prune nodes *)
    Exp.free_vars exp
    |> Sequence.fold ~init:ControlDepSet.empty ~f:(fun acc id ->
           match Procdesc.Node.find_in_node_or_preds prune_node ~f:(find_vars_in_decl id) with
           | Some deps ->
               ControlDepSet.union deps acc
           | None ->
               L.internal_error "Failed to get the definition of the control variable %a" Ident.pp
                 id ;
               assert false )


  (* extract vars from the prune instructions in the node *)
  let get_control_vars loop_nodes =
    GuardNodes.fold
      (fun prune_node acc ->
        let instrs = Procdesc.Node.get_instrs prune_node in
        Instrs.fold ~init:acc
          ~f:(fun astate instr ->
            match instr with
            | Sil.Prune (exp, _, _, _) ->
                Domain.union (get_vars_in_exp exp prune_node) astate
            | _ ->
                (* prune nodes include other instructions like REMOVE_TEMPS or loads *)
                astate )
          instrs )
      loop_nodes Domain.empty


  (* Each time we pass through
     - a loop header, add control variables of its guard nodes,
     - a loop exit node, remove control variables of its guard nodes
     This is correct because the CVs are only going to be temporaries. *)
  let exec_instr astate {ProcData.extras= {exit_map; loop_head_to_guard_nodes}} (node: CFG.Node.t)
      _ =
    let node = CFG.Node.underlying_node node in
    let astate' =
      match LoopHeadToGuardNodes.find_opt node loop_head_to_guard_nodes with
      | Some loop_nodes ->
          Domain.union astate (get_control_vars loop_nodes)
      | _ ->
          astate
    in
    match Procdesc.Node.get_kind node with
    | Procdesc.Node.Prune_node _ -> (
      match ExitNodeToLoopHeads.find_opt node exit_map with
      | Some loop_heads ->
          LoopHeads.fold
            (fun loop_head astate_acc ->
              match LoopHeadToGuardNodes.find_opt loop_head loop_head_to_guard_nodes with
              | Some guard_nodes ->
                  L.(debug Analysis Medium)
                    "@\n>>>Exit node %a, Loop head %a, guard nodes=%a  @\n\n" Procdesc.Node.pp node
                    Procdesc.Node.pp loop_head GuardNodes.pp guard_nodes ;
                  get_control_vars guard_nodes |> Domain.diff astate_acc
              | _ ->
                  (* Every loop head must have a guard node *)
                  assert false )
            loop_heads astate'
      | _ ->
          astate' )
    | _ ->
        (* Exit node must be a prune node *)
        assert (not (ExitNodeToLoopHeads.mem node exit_map)) ;
        astate'


  let pp_session_name node fmt =
    F.fprintf fmt "control dependency analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

module ControlDepAnalyzer = AbstractInterpreter.Make (CFG) (TransferFunctionsControlDeps)

let compute_control_vars control_invariant_map node =
  let node_id = CFG.Node.id node in
  let deps = VarSet.empty in
  ControlDepAnalyzer.extract_post node_id control_invariant_map
  |> Option.map ~f:(fun control_deps ->
         (* loop_inv_map: loop head -> variables that are invariant in the loop *)
         L.(debug Analysis Medium)
           "@\n>>> Control dependencies of node %a : %a @\n" Procdesc.Node.pp node ControlDepSet.pp
           control_deps ;
         control_deps )
  |> Option.value ~default:deps
