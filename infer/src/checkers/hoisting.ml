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
    {loc: Location.t; pname: Typ.Procname.t; node: Procdesc.Node.t; params: (Exp.t * Typ.t) list}
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
  | Sil.Call ((ret_id, _), Exp.Const (Const.Cfun pname), params, loc, _)
    when (* Check condition (1); N dominates all loop sources *)
         List.for_all ~f:(fun source -> Dominators.dominates idom node source) source_nodes
         && (* Check condition (2); id should be invariant already *)
            LoopInvariant.InvariantVars.mem (Var.of_id ret_id) inv_vars ->
      HoistCalls.add {pname; loc; node; params} hoistable_calls
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


let get_hoist_inv_map tenv reaching_defs_invariant_map loop_head_to_source_nodes idom =
  Procdesc.NodeMap.fold
    (fun loop_head source_nodes inv_map ->
      (* get all the nodes in the loop *)
      let loop_nodes = Loop_control.get_all_nodes_upwards_until loop_head source_nodes in
      let inv_vars_in_loop =
        LoopInvariant.get_inv_vars_in_loop tenv reaching_defs_invariant_map loop_head loop_nodes
          ~is_inv_by_default:Config.cost_invariant_by_default
      in
      let hoist_instrs = get_hoistable_calls inv_vars_in_loop loop_nodes source_nodes idom in
      LoopHeadToHoistInstrs.add loop_head hoist_instrs inv_map )
    loop_head_to_source_nodes LoopHeadToHoistInstrs.empty


let do_report summary Call.({pname; loc}) ~issue loop_head_loc =
  let exp_desc =
    F.asprintf "The call to %a at %a is loop-invariant" Typ.Procname.pp pname Location.pp loc
  in
  let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
  let message =
    F.asprintf "%s and can be moved out of the loop at %a." exp_desc Location.pp loop_head_loc
  in
  Reporting.log_error summary ~loc ~ltr issue message


let model_satisfies ~f tenv pname = InvariantModels.Call.dispatch tenv pname [] |> Option.exists ~f

let get_issue_to_report tenv Call.({pname; node; params}) integer_type_widths inferbo_invariant_map
    =
  (* If a function is modeled as variant for hoisting (like
     List.size or __cast ), we don't want to report it *)
  let is_variant_for_hoisting =
    model_satisfies ~f:InvariantModels.is_variant_for_hoisting tenv pname
  in
  let report_invariant =
    ((not is_variant_for_hoisting) && not Config.hoisting_report_only_expensive)
    ||
    (* only report if function call has expensive/symbolic cost *)
    match Ondemand.analyze_proc_name pname with
    | Some {Summary.payloads= {Payloads.cost= Some {CostDomain.post= cost}}}
      when CostDomain.BasicCost.is_symbolic cost ->
        let instr_node_id = InstrCFG.last_of_underlying_node node |> InstrCFG.Node.id in
        let inferbo_invariant_map = Lazy.force inferbo_invariant_map in
        let inferbo_mem =
          Option.value_exn (BufferOverrunChecker.extract_pre instr_node_id inferbo_invariant_map)
        in
        (* get the cost of the function call *)
        Cost.instantiate_cost integer_type_widths ~inferbo_caller_mem:inferbo_mem
          ~callee_pname:pname ~params ~callee_cost:cost
        |> CostDomain.BasicCost.is_symbolic
    | _ ->
        false
  in
  if report_invariant then
    if model_satisfies ~f:InvariantModels.is_invariant tenv pname then
      Some IssueType.loop_invariant_call
    else Some IssueType.invariant_call
  else None


let checker ({Callbacks.tenv; summary; proc_desc; integer_type_widths} as callback_args) :
    Summary.t =
  let cfg = InstrCFG.from_pdesc proc_desc in
  let proc_data = ProcData.make_default proc_desc tenv in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map =
    ReachingDefs.Analyzer.exec_cfg cfg proc_data
      ~initial:(ReachingDefs.init_reaching_defs_with_formals proc_desc)
  in
  let pname = Procdesc.get_proc_name proc_desc in
  let inferbo_invariant_map =
    lazy (BufferOverrunChecker.lookup_inv_map_cache callback_args pname)
  in
  (* get dominators *)
  let idom = Dominators.get_idoms proc_desc in
  let loop_head_to_source_nodes = Loop_control.get_loop_head_to_source_nodes cfg in
  (* get a map,  loop head -> instrs that can be hoisted out of the loop *)
  let loop_head_to_inv_instrs =
    get_hoist_inv_map tenv reaching_defs_invariant_map loop_head_to_source_nodes idom
  in
  (* report function calls to hoist (per loop) *)
  (* Note: we report the innermost loop for hoisting out. TODO: Future
     optimization, take out as further up as possible.*)
  LoopHeadToHoistInstrs.iter
    (fun loop_head inv_instrs ->
      let loop_head_loc = Procdesc.Node.get_loc loop_head in
      HoistCalls.iter
        (fun call ->
          get_issue_to_report tenv call integer_type_widths inferbo_invariant_map
          |> Option.iter ~f:(fun issue -> do_report summary call ~issue loop_head_loc) )
        inv_instrs )
    loop_head_to_inv_instrs ;
  summary
