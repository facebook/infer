(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging

module Make
    (C : ProcCfg.Wrapper)
    (Sched : Scheduler.S)
    (A : AbstractDomain.S)
    (T : TransferFunctions.S with type astate = A.astate) = struct

  module S = Sched (C)
  module M = ProcCfg.NodeIdMap (C)

  type state = { pre: A.astate; post: A.astate; visit_count: int; }
  (* invariant map from node id -> abstract state representing postcondition for node id *)
  type inv_map = state M.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map =
    try
      Some (M.find node_id inv_map)
    with Not_found ->
      L.err "Warning: No state found for node %a" C.pp_node_id node_id;
      None

  (** extract the postcondition of node [n] from [inv_map] *)
  let extract_post node_id inv_map =
    match extract_state node_id inv_map with
    | Some state -> Some state.post
    | None -> None

  (** extract the precondition of node [n] from [inv_map] *)
  let extract_pre node_id inv_map =
    match extract_state node_id inv_map with
    | Some state -> Some state.pre
    | None -> None

  let exec_node node astate_pre work_queue inv_map proc_data =
    let exec_instrs astate_acc instr =
      if A.is_bottom astate_acc
      then astate_acc
      else T.exec_instr astate_acc proc_data instr in
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
            M.add node_id { pre=astate_pre; post=widened_post; visit_count; } inv_map in
          inv_map', S.schedule_succs work_queue node
        end
    else
      (* first time visiting this node *)
      let inv_map' =
        let visit_count = 0 in
        M.add node_id { pre=astate_pre; post=astate_post; visit_count; } inv_map in
      inv_map', S.schedule_succs work_queue node

  let rec exec_worklist cfg work_queue inv_map proc_data =
    let compute_pre node inv_map =
      (* if the [pred] -> [node] transition was normal, use post([pred]) *)
      let extract_post_ pred = extract_post (C.node_id pred) inv_map in
      let normal_posts = IList.map extract_post_ (C.normal_preds cfg node) in
      (* if the [pred] -> [node] transition was exceptional, use pre([pred]) *)
      let extract_pre_f acc pred = extract_pre (C.node_id pred) inv_map :: acc in
      let all_posts = IList.fold_left extract_pre_f normal_posts (C.exceptional_preds cfg node) in
      match IList.flatten_options all_posts with
      | post :: posts -> Some (IList.fold_left A.join post posts)
      | [] -> None in
    match S.pop work_queue with
    | Some (_, [], work_queue') ->
        exec_worklist cfg work_queue' inv_map proc_data
    | Some (node, _, work_queue') ->
        let inv_map_post, work_queue_post = match compute_pre node inv_map with
          | Some astate_pre -> exec_node node astate_pre work_queue' inv_map proc_data
          | None -> inv_map, work_queue' in
        exec_worklist cfg work_queue_post inv_map_post proc_data
    | None ->
        inv_map

  (* compute and return an invariant map for [cfg] *)
  let exec_cfg cfg proc_data =
    let start_node = C.start_node cfg in
    let inv_map', work_queue' = exec_node start_node A.initial (S.empty cfg) M.empty proc_data in
    exec_worklist cfg work_queue' inv_map' proc_data

  (* compute and return an invariant map for [pdesc] *)
  let exec_pdesc ({ ProcData.pdesc; } as proc_data) =
    L.out "Starting analysis of %a@." Procname.pp (Cfg.Procdesc.get_proc_name pdesc);
    exec_cfg (C.from_pdesc pdesc) proc_data

  (* compute and return the postcondition of [pdesc] *)
  let compute_post ({ ProcData.pdesc; } as proc_data) =
    let cfg = C.from_pdesc pdesc in
    let inv_map = exec_cfg cfg proc_data in
    extract_post (C.node_id (C.exit_node cfg)) inv_map

  module Interprocedural (Summ : Summary.S with type summary = A.astate) = struct

    let checker { Callbacks.get_proc_desc; proc_desc; proc_name; tenv; } =
      let post_opt = ref None in
      let analyze_ondemand pdesc =
        match compute_post (ProcData.make pdesc tenv) with
        | Some post ->
            Summ.write_summary (Cfg.Procdesc.get_proc_name pdesc) post;
            post_opt := Some post
        | None ->
            post_opt := None in
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
          Ondemand.unset_callbacks ();
        end;
      !post_opt
  end
end

