(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
(* forward dependency analysis for computing set of variables that affect the control flow at each program point
   1. perform a control flow dependency analysis CF dependency analysis by
      getting all the variables that appear in the control flow path up to that node.
   2. perform a data dependency analysis
   3. for each control dependency per node, find its respective data dependencies *)

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
    F.fprintf fmt "data dependency analysis %a" CFG.pp_id (CFG.id node)
end

module ControlDepSet = VarSet

(* forward transfer function for control dependencies *)
module TransferFunctionsControlDeps (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ControlDepSet

  type extras = ProcData.no_extras

  let exec_instr astate _ _ instr =
    match instr with
    | Sil.Prune (exp, _, true_branch, if_kind) ->
        (* Only keep track of control flow variables for loop prune nodes with the true branch *)
        (* For false branches, remove the var from the set for fixpoint *)
        let astate' =
          Exp.free_vars exp |> Sequence.map ~f:Var.of_id |> Sequence.to_list
          |> ControlDepSet.of_list
          |>
          if (true_branch && Sil.is_loop if_kind) || Language.curr_language_is Java then
            Domain.union astate
          else Domain.diff astate
        in
        Exp.program_vars exp |> Sequence.map ~f:Var.of_pvar |> Sequence.to_list
        |> ControlDepSet.of_list
        |>
        if (true_branch && Sil.is_loop if_kind) || Language.curr_language_is Java then
          Domain.union astate'
        else Domain.diff astate'
    | Sil.Load _
    | Sil.Store _
    | Sil.Call _
    | Declare_locals _
    | Remove_temps _
    | Abstract _
    | Nullify _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "control dependency analysis %a" CFG.pp_id (CFG.id node)
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
  let und_node = CFG.underlying_node node in
  let node_id = Procdesc.Node.get_id und_node in
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
