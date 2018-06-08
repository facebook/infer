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
   affect the control flow at each program point

   1. perform a control flow dependency analysis by getting all the
   variables that appear in the control flow path up to that node.

   2. for each control dependency per node, find its respective data
   dependency
 *)

module VarSet = AbstractDomain.FiniteSet (Var)
module ControlDepSet = VarSet
module GuardNodes = AbstractDomain.FiniteSet (Procdesc.Node)

(** Map exit node -> prune nodes in the loop guard  *)
module ExitNodeToGuardNodes = AbstractDomain.Map (Procdesc.Node) (GuardNodes)

(** Map loop header node -> prune nodes in the loop guard  *)
module LoopHeaderToGuardNodes = AbstractDomain.Map (Procdesc.Node) (GuardNodes)

type loop_control_maps =
  {exit_map: ExitNodeToGuardNodes.astate; loop_header_map: LoopHeaderToGuardNodes.astate}

(* forward transfer function for control dependencies *)
module TransferFunctionsControlDeps (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ControlDepSet

  type extras = loop_control_maps

  let collect_vars_in_exp exp =
    Var.get_all_vars_in_exp exp
    |> Sequence.fold ~init:ControlDepSet.empty ~f:(fun acc var -> ControlDepSet.add var acc)


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
     - a loop header, add control variables of its guard nodes 
     - a loop exit node, remove control variables of its guard nodes
     This is correct because the CVs are only going to be temporaries. *)
  let exec_instr astate {ProcData.extras= {exit_map; loop_header_map}} (node: CFG.Node.t) _ =
    let node = CFG.Node.underlying_node node in
    let astate' =
      match LoopHeaderToGuardNodes.find_opt node loop_header_map with
      | Some loop_nodes ->
          L.(debug Analysis Medium) "@\n>>> Loop header %a \n" Procdesc.Node.pp node ;
          Domain.union astate (get_control_vars loop_nodes)
      | _ ->
          astate
    in
    match ExitNodeToGuardNodes.find_opt node exit_map with
    | Some loop_nodes ->
        L.(debug Analysis Medium)
          "@\n>>>Exit node %a loop nodes=%a  @\n\n" Procdesc.Node.pp node GuardNodes.pp loop_nodes ;
        get_control_vars loop_nodes |> Domain.diff astate'
    | _ ->
        astate'


  let pp_session_name node fmt =
    F.fprintf fmt "control dependency analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

module CFG = ProcCfg.Normal
module ControlDepAnalyzer = AbstractInterpreter.Make (CFG) (TransferFunctionsControlDeps)

let report_control_deps control_map node =
  Instrs.iter (Procdesc.Node.get_instrs node) ~f:(fun instr ->
      L.(debug Analysis Medium) "@\n>>>Control dependencies of node = %a @\n" Procdesc.Node.pp node ;
      List.iter (Sil.instr_get_exps instr) ~f:(fun exp ->
          L.(debug Analysis Medium)
            "@\n>>>for exp = %a : %a @\n\n" Exp.pp exp ControlDepSet.pp control_map ) )


let compute_all_deps control_invariant_map node =
  let node_id = CFG.Node.id node in
  let deps = VarSet.empty in
  ControlDepAnalyzer.extract_post node_id control_invariant_map
  |> Option.map ~f:(fun control_deps ->
         report_control_deps control_deps node ;
         control_deps )
  |> Option.value ~default:deps
