(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging

module Make
    (C : ProcCfg.Wrapper)
    (Sched : Scheduler.S)
    (A : AbstractDomain.S)
    (T : TransferFunctions.S with type astate = A.astate) = struct

  module S = Sched (C)
  module M = ProcCfg.NodeIdMap (C)

  type state = { post: A.astate; visit_count: int; }
  (* invariant map from node id -> abstract state representing postcondition for node id *)
  type inv_map = state M.t

  let exec_node node astate_pre work_queue inv_map pdesc =
    let exec_instrs astate_acc instr =
      if A.is_bottom astate_acc
      then astate_acc
      else T.exec_instr astate_acc pdesc instr in
    let node_id = C.node_id node in
    L.out "Doing analysis of node %a from pre %a@." C.pp_node_id node_id A.pp astate_pre;
    let instrs = C.instrs node in
    let astate_post =
      IList.fold_left exec_instrs astate_pre instrs in
    L.out "Post for node %a is %a@." C.pp_node_id node_id A.pp astate_post;
    if M.mem node_id inv_map then
      let old_state = M.find node_id inv_map in
      let widened_post =
        A.widen ~prev:old_state.post ~next:astate_post ~num_iters:old_state.visit_count in
      if A.(<=) ~lhs:widened_post ~rhs:old_state.post
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

  let rec exec_worklist work_queue inv_map pdesc =
    match S.pop work_queue with
    | Some (_, [], work_queue') ->
        exec_worklist work_queue' inv_map pdesc
    | Some (node, visited_pred :: visited_preds, work_queue') ->
        let get_post pred_id =
          (M.find pred_id inv_map).post in
        (* compute the precondition for node by joining post of all visited preds *)
        let join_pred_posts astate_acc pred_id =
          A.join (get_post pred_id) astate_acc in
        let astate_pre = IList.fold_left join_pred_posts (get_post visited_pred) visited_preds in
        let inv_map', work_queue'' = exec_node node astate_pre work_queue' inv_map pdesc in
        exec_worklist work_queue'' inv_map' pdesc
    | None -> inv_map

  (* compute and return an invariant map for [cfg] *)
  let exec_cfg cfg pdesc =
    let start_node = C.start_node cfg in
    let inv_map', work_queue' = exec_node start_node A.initial (S.empty cfg) M.empty pdesc in
    exec_worklist work_queue' inv_map' pdesc

  (* compute and return an invariant map for [pdesc] *)
  let exec_pdesc pdesc =
    L.out "Starting analysis of %a@." Procname.pp (Cfg.Procdesc.get_proc_name pdesc);
    exec_cfg (C.from_pdesc pdesc) pdesc

  (* compute and return the postcondition of [pdesc] *)
  let compute_post pdesc =
    let cfg = C.from_pdesc pdesc in
    let inv_map = exec_cfg cfg pdesc in
    try
      let end_state = M.find (C.node_id (C.exit_node cfg)) inv_map in
      Some (end_state.post)
    with Not_found ->
      (* TODO: this happens when we get a malformed CFG due to a frontend failure. can eliminate in
         the future once we fix the frontends. *)
      L.err
        "Warning: No postcondition found for exit node of %a"
        Procname.pp (Cfg.Procdesc.get_proc_name pdesc);
      None

  module Interprocedural (Summ : Summary.S with type summary = A.astate) = struct

    let checker { Callbacks.get_proc_desc; proc_desc; proc_name; } =
      let analyze_ondemand pdesc =
        match compute_post pdesc with
        | Some post ->
            Summ.write_summary (Cfg.Procdesc.get_proc_name pdesc) post
        | None -> () in
      let callbacks =
        {
          Ondemand.analyze_ondemand;
          get_proc_desc;
        } in
      if Ondemand.procedure_should_be_analyzed proc_name
      then
        begin
          Ondemand.set_callbacks callbacks;
          analyze_ondemand proc_desc;
          Ondemand.unset_callbacks ()
        end
  end
end

