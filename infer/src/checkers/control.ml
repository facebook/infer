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

   2. perform a data dependency analysis

   3. for each control dependency per node, find its respective data
   dependencies *)

module VarSet = AbstractDomain.FiniteSet (Var)
module DataDepSet = VarSet
module DataDepMap = AbstractDomain.Map (Var) (DataDepSet)

(* forward transfer function for collecting data dependencies of a variable *)
module TransferFunctionsDataDeps (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = DataDepMap

  type extras = ProcData.no_extras

  (* compute data the dependencies in an eager way, i.e. get the transitive closure *)
  let add_data_dep_exp lhs_var exp astate =
    let add_dependency_to_var lhs_var free_var astate_acc =
      (* add lhs_var -> {free var} *)
      let deps_of_free_var =
        DataDepMap.find_opt free_var astate_acc |> Option.value ~default:DataDepSet.empty
      in
      DataDepMap.add lhs_var (DataDepSet.add free_var deps_of_free_var) astate_acc
    in
    let astate' =
      Exp.free_vars exp
      |> Sequence.fold ~init:astate ~f:(fun astate_acc id_var ->
             add_dependency_to_var lhs_var (Var.of_id id_var) astate_acc )
    in
    Exp.program_vars exp
    |> Sequence.fold ~init:astate' ~f:(fun astate_acc pvar ->
           add_dependency_to_var lhs_var (Var.of_pvar pvar) astate_acc )


  let exec_instr astate _ _ = function
    | Sil.Load (lhs_id, _, _, _) when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a read *)
        astate
    | Sil.Load (id, exp, _, _) ->
        add_data_dep_exp (Var.of_id id) exp astate
    | Sil.Store (exp_lhs, _, exp_rhs, _) ->
        let astate' =
          Exp.free_vars exp_lhs
          |> Sequence.fold ~init:astate ~f:(fun astate_acc id ->
                 add_data_dep_exp (Var.of_id id) exp_rhs astate_acc )
        in
        Exp.program_vars exp_lhs
        |> Sequence.fold ~init:astate' ~f:(fun astate_acc pvar ->
               add_data_dep_exp (Var.of_pvar pvar) exp_rhs astate_acc )
    | Sil.Call ((id, _), _, arg_list, _, _) ->
        List.fold_left arg_list ~init:astate ~f:(fun astate_acc (exp, _) ->
            add_data_dep_exp (Var.of_id id) exp astate_acc )
    | Sil.Prune _ | Declare_locals _ | Remove_temps _ | Abstract _ | Nullify _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "data dependency analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

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

  let get_vars_in_exp exp =
    let aux f_free_vars f_to_var =
      f_free_vars exp |> Sequence.map ~f:f_to_var |> Sequence.to_list |> ControlDepSet.of_list
    in
    assert (Domain.is_empty (aux Exp.program_vars Var.of_pvar)) ;
    (* We should never have program variables in prune nodes *)
    aux Exp.free_vars Var.of_id


  (* extract vars from the prune instructions in the node *)
  let get_control_vars loop_nodes =
    GuardNodes.fold
      (fun loop_node acc ->
        let instrs = Procdesc.Node.get_instrs loop_node in
        Instrs.fold ~init:acc
          ~f:(fun astate instr ->
            match instr with
            | Sil.Prune (exp, _, _, _) ->
                Domain.union (get_vars_in_exp exp) astate
            | _ ->
                (* prune nodes include other instructions like REMOVE_TEMPS *)
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
module DataDepAnalyzer = AbstractInterpreter.Make (CFG) (TransferFunctionsDataDeps)
module ControlDepAnalyzer = AbstractInterpreter.Make (CFG) (TransferFunctionsControlDeps)

let report_deps data_map =
  Sequence.iter ~f:(fun x ->
      match DataDepMap.find_opt x data_map with
      | Some d_vars ->
          L.(debug Analysis Medium) "@\n>>> var = %a --> %a  @\n\n" Var.pp x DataDepSet.pp d_vars
      | None ->
          () )


let report_data_deps data_map node =
  Instrs.iter (Procdesc.Node.get_instrs node) ~f:(fun instr ->
      List.iter (Sil.instr_get_exps instr) ~f:(fun exp ->
          L.(debug Analysis Medium)
            "@\n>>>Data dependencies of node = %a @\n" Procdesc.Node.pp node ;
          let free_vars = Exp.free_vars exp |> Sequence.map ~f:Var.of_id in
          let program_vars = Exp.program_vars exp |> Sequence.map ~f:Var.of_pvar in
          L.(debug Analysis Medium) "@\n>>>for exp = %a : @\n\n" Exp.pp exp ;
          report_deps data_map free_vars ;
          report_deps data_map program_vars ) )


let report_control_deps control_map node =
  Instrs.iter (Procdesc.Node.get_instrs node) ~f:(fun instr ->
      L.(debug Analysis Medium) "@\n>>>Control dependencies of node = %a @\n" Procdesc.Node.pp node ;
      List.iter (Sil.instr_get_exps instr) ~f:(fun exp ->
          L.(debug Analysis Medium)
            "@\n>>>for exp = %a : %a @\n\n" Exp.pp exp ControlDepSet.pp control_map ) )


let gather_all_deps control_map data_map =
  ControlDepSet.fold
    (fun x deps ->
      DataDepMap.find_opt x data_map
      |> Option.map ~f:(fun data_deps ->
             DataDepSet.filter Var.appears_in_source_code data_deps |> DataDepSet.union deps )
      |> Option.value ~default:deps )
    control_map VarSet.empty


let compute_all_deps data_invariant_map control_invariant_map node =
  let node_id = CFG.Node.id node in
  let deps = VarSet.empty in
  ControlDepAnalyzer.extract_post node_id control_invariant_map
  |> Option.map ~f:(fun control_deps ->
         DataDepAnalyzer.extract_post node_id data_invariant_map
         |> Option.map ~f:(fun data_deps ->
                report_data_deps data_deps node ;
                report_control_deps control_deps node ;
                gather_all_deps control_deps data_deps )
         |> Option.value ~default:deps )
  |> Option.value ~default:deps
