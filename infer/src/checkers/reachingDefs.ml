(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module NodeCFG = ProcCfg.Normal

(** The node in which the reaching definition x := e is defined.

    A definition x :=e, declared at node N, reaches the current node if there is a path from node N
    to the current node such that x is not modified along the path **)
module Defs = AbstractDomain.FiniteSet (Procdesc.Node)

(* even though we only add singletons (defs), the set is needed for joins *)

(** Map var -> its reaching definition *)
module ReachingDefsMap = AbstractDomain.Map (Var) (Defs)

(* forward transfer function for reaching definitions *)
module TransferFunctionsReachingDefs (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ReachingDefsMap

  type analysis_data = unit

  (* for each  x := e at node n, remove x's definitions and introduce x -> n *)
  let exec_instr astate () (node : CFG.Node.t) _ instr =
    let node = CFG.Node.underlying_node node in
    let strong_update_def astate var = Domain.add var (Defs.singleton node) astate in
    let weak_update_def astate var =
      Domain.update var
        (function Some defs -> Some (Defs.add node defs) | None -> Some (Defs.singleton node))
        astate
    in
    match instr with
    | Sil.Load {id= lhs_id} when Ident.is_none lhs_id ->
        (* dummy deref inserted by frontend--don't count as a read *)
        astate
    | Sil.Load {id} | Sil.Call ((id, _), _, _, _, _) ->
        strong_update_def astate (Var.of_id id)
    (* only strong update for assigning to a pvar *)
    | Sil.Store {e1= Lvar pvar} ->
        strong_update_def astate (Var.of_pvar pvar)
    (* by default use weak update *)
    | Sil.Store {e1= exp_lhs} ->
        let vars = Var.get_all_vars_in_exp exp_lhs in
        Sequence.fold ~init:astate ~f:weak_update_def vars
    | _ ->
        astate


  let pp_session_name node fmt =
    F.fprintf fmt "reaching defs analysis %a" CFG.Node.pp_id (CFG.Node.id node)
end

(* initialize formal parameters to have start node as reaching def *)
let init_reaching_defs_with_formals pdesc =
  let start_node_defs = Defs.singleton (Procdesc.get_start_node pdesc) in
  Procdesc.get_pvar_formals pdesc
  |> List.fold_left ~init:ReachingDefsMap.empty ~f:(fun acc (pvar, _) ->
         ReachingDefsMap.add (Var.of_pvar pvar) start_node_defs acc )


module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctionsReachingDefs (NodeCFG))

type invariant_map = Analyzer.invariant_map

let compute_invariant_map proc_desc =
  let node_cfg = NodeCFG.from_pdesc proc_desc in
  Analyzer.exec_cfg node_cfg () ~initial:(init_reaching_defs_with_formals proc_desc)


let extract_post = Analyzer.extract_post
