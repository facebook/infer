(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module InstrCFG = ProcCfg.NormalOneInstrPerNode
module BasicCost = CostDomain.BasicCost
module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module HoistCalls = AbstractDomain.FiniteSet (CostInstantiate.Call)

(** Map loop_header -> instrs that can be hoisted out of the loop *)
module LoopHeadToHoistInstrs = Procdesc.NodeMap

(* A loop-invariant function call C(args) at node N can be hoisted out of the loop if
 *
 *     1. C is guaranteed to execute, i.e. N dominates all loop sources
 *     2. args are loop invariant *)

let is_hoistable_call inv_vars node source_nodes idom ret_id =
  (* Check condition (1); N dominates all loop sources *)
  List.for_all ~f:(fun source -> Dominators.dominates idom node source) source_nodes
  && (* Check condition (2); id should be invariant already *)
  LoopInvariant.InvariantVars.mem (Var.of_id ret_id) inv_vars


let add_if_hoistable inv_vars instr node idx source_nodes idom hoistable_calls =
  match instr with
  | Sil.Call (((ret_id, _) as ret), Const (Cfun pname), args, loc, _)
    when is_hoistable_call inv_vars node source_nodes idom ret_id ->
      HoistCalls.add
        {pname; loc; node= ProcCfg.DefaultNode.to_instr idx node; args; captured_vars= []; ret}
        hoistable_calls
  | Sil.Call (((ret_id, _) as ret), Closure {name= pname; captured_vars}, args, loc, _)
    when is_hoistable_call inv_vars node source_nodes idom ret_id ->
      HoistCalls.add
        {pname; loc; node= ProcCfg.DefaultNode.to_instr idx node; args; captured_vars; ret}
        hoistable_calls
  | _ ->
      hoistable_calls


let get_hoistable_calls inv_vars loop_nodes source_nodes idom =
  LoopNodes.fold
    (fun node hoist_calls ->
      let instr_in_node = Procdesc.Node.get_instrs node in
      Instrs.foldi ~init:hoist_calls
        ~f:(fun idx acc instr -> add_if_hoistable inv_vars instr node idx source_nodes idom acc)
        instr_in_node )
    loop_nodes HoistCalls.empty


let get_hoist_inv_map tenv ~get_callee_purity reaching_defs_invariant_map loop_head_to_source_nodes
    idom =
  Procdesc.NodeMap.fold
    (fun loop_head source_nodes inv_map ->
      (* get all the nodes in the loop *)
      let loop_nodes = Loop_control.get_all_nodes_upwards_until loop_head source_nodes in
      let inv_vars_in_loop =
        LoopInvariant.get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes
          ~is_pure_by_default:Config.pure_by_default ~get_callee_purity
      in
      let hoist_instrs = get_hoistable_calls inv_vars_in_loop loop_nodes source_nodes idom in
      LoopHeadToHoistInstrs.add loop_head hoist_instrs inv_map )
    loop_head_to_source_nodes LoopHeadToHoistInstrs.empty


let do_report extract_cost_if_expensive proc_desc err_log
    (CostInstantiate.Call.{pname; loc} as call) loop_head_loc =
  let exp_desc =
    F.asprintf "The call to %a at %a is loop-invariant" Procname.pp pname Location.pp loc
  in
  let loop_inv_trace_elem = Errlog.make_trace_element 0 loc exp_desc [] in
  let issue, cost_msg, ltr =
    match extract_cost_if_expensive call with
    | Some cost ->
        let degree_str = BasicCost.degree_str cost in
        let cost_trace_elem =
          let cost_desc = F.asprintf "with estimated cost %a%s" BasicCost.pp_hum cost degree_str in
          Errlog.make_trace_element 0 loc cost_desc []
        in
        ( IssueType.expensive_loop_invariant_call
        , F.asprintf " and expensive (has complexity %a)"
            (BasicCost.pp_degree ~only_bigO:true)
            (BasicCost.get_degree_with_term cost)
        , loop_inv_trace_elem :: cost_trace_elem :: BasicCost.polynomial_traces cost )
    | None ->
        (IssueType.invariant_call, "", [loop_inv_trace_elem])
  in
  let message = F.asprintf "%s%s." exp_desc cost_msg in
  let suggestion = F.asprintf "Move it out of the loop at %a." Location.pp loop_head_loc in
  Reporting.log_issue ~suggestion proc_desc err_log ~loc ~ltr LoopHoisting issue message


let report_errors proc_desc tenv err_log get_callee_purity reaching_defs_invariant_map
    loop_head_to_source_nodes extract_cost_if_expensive =
  (* get dominators *)
  let idom = Dominators.get_idoms proc_desc in
  (* get a map,  loop head -> instrs that can be hoisted out of the loop *)
  let loop_head_to_inv_instrs =
    get_hoist_inv_map tenv ~get_callee_purity reaching_defs_invariant_map loop_head_to_source_nodes
      idom
  in
  (* report function calls to hoist (per loop) *)
  (* Note: we report the innermost loop for hoisting out. TODO: Future
     optimization, take out as further up as possible.*)
  LoopHeadToHoistInstrs.iter
    (fun loop_head inv_instrs ->
      let loop_head_loc = Procdesc.Node.get_loc loop_head in
      HoistCalls.iter
        (fun call -> do_report extract_cost_if_expensive proc_desc err_log call loop_head_loc)
        inv_instrs )
    loop_head_to_inv_instrs


let checker ({InterproceduralAnalysis.proc_desc; err_log; analyze_dependency; tenv} as analysis_data)
    =
  let cfg = InstrCFG.from_pdesc proc_desc in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map = ReachingDefs.compute_invariant_map proc_desc in
  let loop_head_to_source_nodes = Loop_control.get_loop_head_to_source_nodes cfg in
  let extract_cost_if_expensive =
    if Config.hoisting_report_only_expensive then
      CostInstantiate.get_cost_if_expensive analysis_data
    else fun _ -> None
  in
  let get_callee_purity callee_pname =
    analyze_dependency callee_pname |> AnalysisResult.to_option |> Option.bind ~f:snd3
  in
  report_errors proc_desc tenv err_log get_callee_purity reaching_defs_invariant_map
    loop_head_to_source_nodes extract_cost_if_expensive ;
  ()
