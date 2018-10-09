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
  type t = {pname: Typ.Procname.t; loc: Location.t} [@@deriving compare]

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
  | Sil.Call ((ret_id, _), Exp.Const (Const.Cfun pname), _, loc, _)
    when (* Check condition (1); N dominates all loop sources *)
         List.for_all ~f:(fun source -> Dominators.dominates idom node source) source_nodes
         && (* Check condition (2); id should be invariant already *)
            LoopInvariant.InvariantVars.mem (Var.of_id ret_id) inv_vars ->
      HoistCalls.add {pname; loc} hoistable_calls
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


let do_report summary Call.({pname; loc}) loop_head_loc =
  let exp_desc =
    F.asprintf "Loop-invariant call to %a at %a" Typ.Procname.pp pname Location.pp loc
  in
  let ltr = [Errlog.make_trace_element 0 loc exp_desc []] in
  let message =
    F.asprintf "%s can be moved out of the loop at %a." exp_desc Location.pp loop_head_loc
  in
  Reporting.log_error summary ~loc ~ltr IssueType.invariant_call message


let checker {Callbacks.tenv; summary; proc_desc} : Summary.t =
  let cfg = InstrCFG.from_pdesc proc_desc in
  let proc_data = ProcData.make_default proc_desc tenv in
  (* computes reaching defs: node -> (var -> node set) *)
  let reaching_defs_invariant_map =
    ReachingDefs.Analyzer.exec_cfg cfg proc_data
      ~initial:(ReachingDefs.init_reaching_defs_with_formals proc_desc)
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
      HoistCalls.iter (fun call -> do_report summary call loop_head_loc) inv_instrs )
    loop_head_to_inv_instrs ;
  summary
