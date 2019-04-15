(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module InstrCFG = ProcCfg.NormalOneInstrPerNode

module Call = struct
  type t =
    { loc: Location.t
    ; pname: Typ.Procname.t
    ; node: Procdesc.Node.t
    ; params: (Exp.t * Typ.t) list
    ; ret: Ident.t * Typ.t }
  [@@deriving compare]

  let pp fmt {pname; loc} =
    F.fprintf fmt "loop-invariant call to %a, at %a " Typ.Procname.pp pname Location.pp loc
end

module LoopNodes = AbstractDomain.FiniteSet (Procdesc.Node)
module HoistCalls = AbstractDomain.FiniteSet (Call)

(** Map loop_header -> instrs that can be hoisted out of the loop *)
module LoopHeadToHoistInstrs = Procdesc.NodeMap

(* A loop-invariant function call C(args) at node N can be hoisted out of the loop if
 *
 *     1. C is guaranteed to execute, i.e. N dominates all loop sources
 *     2. args are loop invariant *)

let add_if_hoistable inv_vars instr node source_nodes idom hoistable_calls =
  match instr with
  | Sil.Call (((ret_id, _) as ret), Exp.Const (Const.Cfun pname), params, loc, _)
    when (* Check condition (1); N dominates all loop sources *)
         List.for_all ~f:(fun source -> Dominators.dominates idom node source) source_nodes
         && (* Check condition (2); id should be invariant already *)
            LoopInvariant.InvariantVars.mem (Var.of_id ret_id) inv_vars ->
      HoistCalls.add {pname; loc; node; params; ret} hoistable_calls
  | _ ->
      hoistable_calls


let get_hoistable_calls inv_vars loop_nodes source_nodes idom =
  LoopNodes.fold
    (fun node hoist_calls ->
      let instr_in_node = Procdesc.Node.get_instrs node in
      Instrs.fold ~init:hoist_calls
        ~f:(fun acc instr -> add_if_hoistable inv_vars instr node source_nodes idom acc)
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
          ~is_inv_by_default:Config.cost_invariant_by_default ~get_callee_purity
      in
      let hoist_instrs = get_hoistable_calls inv_vars_in_loop loop_nodes source_nodes idom in
      LoopHeadToHoistInstrs.add loop_head hoist_instrs inv_map )
    loop_head_to_source_nodes LoopHeadToHoistInstrs.empty


let do_report summary Call.{pname; loc} ~issue loop_head_loc =
  let exp_desc =
    F.asprintf "The call to %a at %a is loop-invariant" Typ.Procname.pp pname Location.pp loc
  in
  let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
  let message =
    F.asprintf "%s and can be moved out of the loop at %a." exp_desc Location.pp loop_head_loc
  in
  Reporting.log_error summary ~loc ~ltr issue message


let is_call_expensive tenv integer_type_widths get_callee_cost_summary_and_formals
    inferbo_invariant_map (Call.{pname; node; ret; params} as call) =
  let last_node = InstrCFG.last_of_underlying_node node in
  let instr_node_id = InstrCFG.Node.id last_node in
  let inferbo_mem =
    Option.value_exn (BufferOverrunAnalysis.extract_pre instr_node_id inferbo_invariant_map)
  in
  (* only report if function call has expensive/symbolic cost *)
  match get_callee_cost_summary_and_formals call inferbo_mem with
  | Some (CostDomain.{post= cost_record}, callee_formals)
    when CostDomain.BasicCost.is_symbolic (CostDomain.get_operation_cost cost_record) -> (
      let loc = InstrCFG.Node.loc last_node in
      let node_hash = InstrCFG.Node.hash last_node in
      let model_env =
        BufferOverrunUtils.ModelEnv.mk_model_env pname ~node_hash loc tenv integer_type_widths
      in
      (* get the cost of the function call *)
      match CostModels.Call.dispatch tenv pname params with
      | Some model ->
          model model_env ~ret inferbo_mem |> CostDomain.BasicCost.is_symbolic
      | None ->
          Cost.instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
            ~callee_pname:pname ~callee_formals ~params
            ~callee_cost:(CostDomain.get_operation_cost cost_record)
            ~loc
          |> CostDomain.BasicCost.is_symbolic )
  | _ ->
      false


let get_issue_to_report should_report_expensive_invariant call =
  if should_report_expensive_invariant call then Some IssueType.expensive_loop_invariant_call
  else Some IssueType.invariant_call


let report_errors proc_desc tenv get_callee_purity reaching_defs_invariant_map
    loop_head_to_source_nodes should_report_expensive_invariant summary =
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
        (fun call ->
          get_issue_to_report should_report_expensive_invariant call
          |> Option.iter ~f:(fun issue -> do_report summary call ~issue loop_head_loc) )
        inv_instrs )
    loop_head_to_inv_instrs


let checker Callbacks.{tenv; summary; proc_desc; integer_type_widths} : Summary.t =
  let cfg = InstrCFG.from_pdesc proc_desc in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map = ReachingDefs.compute_invariant_map proc_desc tenv in
  let loop_head_to_source_nodes = Loop_control.get_loop_head_to_source_nodes cfg in
  let should_report_expensive_invariant =
    if Config.hoisting_report_only_expensive then
      let inferbo_invariant_map =
        BufferOverrunAnalysis.cached_compute_invariant_map proc_desc tenv integer_type_widths
      in
      let get_callee_cost_summary_and_formals Call.{loc; pname; params; node; ret} inferbo_mem =
        let callee_pname = pname in
        match Ondemand.analyze_proc_name ~caller_pdesc:proc_desc callee_pname with
        | Some summary ->
            summary.Summary.payloads.Payloads.cost
            |> Option.map ~f:(fun cost_summary ->
                   (cost_summary, Summary.get_proc_desc summary |> Procdesc.get_pvar_formals) )
        | None -> (
          match CostModels.Call.dispatch tenv callee_pname params with
          | Some model ->
              let last_node = InstrCFG.last_of_underlying_node node in
              let node_hash = InstrCFG.Node.hash last_node in
              let model_env =
                BufferOverrunUtils.ModelEnv.mk_model_env pname ~node_hash loc tenv
                  integer_type_widths
              in
              let callee_cost = CostDomain.of_operation_cost (model model_env ~ret inferbo_mem) in
              Some
                ( CostDomain.{post= callee_cost}
                , Summary.get_proc_desc summary |> Procdesc.get_pvar_formals )
          | None ->
              None )
      in
      is_call_expensive tenv integer_type_widths get_callee_cost_summary_and_formals
        inferbo_invariant_map
    else fun _ -> false
  in
  let get_callee_purity callee_pname =
    match Ondemand.analyze_proc_name ~caller_pdesc:proc_desc callee_pname with
    | Some {Summary.payloads= {Payloads.purity}} ->
        purity
    | _ ->
        None
  in
  report_errors proc_desc tenv get_callee_purity reaching_defs_invariant_map
    loop_head_to_source_nodes should_report_expensive_invariant summary ;
  summary
