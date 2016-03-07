(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging

open AbstractDomain
open TransferFunctions

module Make
    (C : ProcCfg.Wrapper)
    (Sched : Scheduler.S)
    (A : AbstractDomain)
    (T : TransferFunctions with type astate = A.astate) = struct

  module S = Sched (C)
  module M = ProcCfg.NodeIdMap (C)

  type state = { post: A.astate; visit_count: int; }
  (* invariant map from node id -> abstract state representing postcondition for node id *)
  type inv_map = state M.t

  let exec_node node astate_pre work_queue inv_map =
    let node_id = C.node_id node in
    L.out "Doing analysis of node %a from pre %a@." C.pp_node_id node_id A.pp astate_pre;
    let instrs = C.instrs node in
    let astate_post =
      IList.fold_left (fun astate_acc instr -> T.exec_instr astate_acc instr) astate_pre instrs in
    L.out "Post for node %a is %a@." C.pp_node_id node_id A.pp astate_post;
    if M.mem node_id inv_map then
      let old_state = M.find node_id inv_map in
      let widened_post =
        A.widen ~prev:old_state.post ~next:astate_post ~num_iters:old_state.visit_count in
      if A.lteq ~lhs:widened_post ~rhs:old_state.post
      then
        begin
          L.out "Old state contains new, not adding succs@.";
          inv_map, work_queue
        end
      else
        begin
          L.out "Widening: %a <widen> %a = %a@."
            A.pp astate_post A.pp old_state.post A.pp widened_post;
          let inv_map' =
            let visit_count = old_state.visit_count + 1 in
            M.add node_id { post=widened_post; visit_count; } inv_map in
          inv_map', S.schedule_succs work_queue node
        end
    else
      (* first time visiting this node *)
      let inv_map' =
        let visit_count = 0 in
        M.add node_id { post=astate_post; visit_count; } inv_map in
      inv_map', S.schedule_succs work_queue node

  let rec exec_worklist work_queue inv_map =
    match S.pop work_queue with
    | Some (node, visited_preds, work_queue') ->
        (* compute the precondition for node by joining post of all visited preds *)
        let join_pred astate_acc pred_id =
          let pred_state = M.find pred_id inv_map in
          A.join pred_state.post astate_acc in
        let astate_pre = IList.fold_left join_pred A.bot visited_preds in
        let inv_map', work_queue'' = exec_node node astate_pre work_queue' inv_map in
        exec_worklist work_queue'' inv_map'
    | None -> inv_map

  let exec_pdesc pdesc =
    L.out "Starting analysis of %a@." Procname.pp (Cfg.Procdesc.get_proc_name pdesc);
    let cfg = C.from_pdesc pdesc in
    let start_node = C.start_node cfg in
    let inv_map', work_queue' = exec_node start_node A.init (S.empty cfg) M.empty in
    exec_worklist work_queue' inv_map'

end

module UnitTests = struct

  let tests =
    let open OUnit2 in
    "abstract_interpreter_suite">:::[]

end
