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

type 'a state = { pre: 'a; post: 'a; visit_count: int; }

module type S = sig
  module TF : TransferFunctions.S
  module M : Map.S with type key = TF.CFG.id
  type inv_map = TF.Domain.astate state M.t

  val exec_pdesc : TF.extras ProcData.t -> inv_map
  val compute_post : TF.extras ProcData.t -> TF.Domain.astate option
end

module MakeNoCFG
    (Sched : Scheduler.S) (TF : TransferFunctions.S with module CFG = Sched.CFG) = struct

  module CFG = Sched.CFG
  module M = ProcCfg.NodeIdMap (Sched.CFG)
  module A = TF.Domain

  (* invariant map from node id -> abstract state representing postcondition for node id *)
  type inv_map = A.astate state M.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map =
    try
      Some (M.find node_id inv_map)
    with Not_found ->
      L.err "Warning: No state found for node %a" CFG.pp_id node_id;
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
    let node_id = CFG.id node in
    L.out "Doing analysis of node %a from pre %a@." CFG.pp_id node_id A.pp astate_pre;
    let update_inv_map pre visit_count =
      let compute_post (pre, inv_map) (instr, id_opt) =
        let post = TF.exec_instr pre proc_data node instr in
        match id_opt with
        | Some id -> post, M.add id { pre; post; visit_count; } inv_map
        | None -> post, inv_map in
      (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
      let instr_ids = match CFG.instr_ids node with
        | [] -> [Sil.skip_instr, None]
        | l -> l in
      let astate_post, inv_map_post = IList.fold_left compute_post (pre, inv_map) instr_ids in
      L.out "Post for node %a is %a@." CFG.pp_id node_id A.pp astate_post;
      let inv_map'' = M.add node_id { pre; post=astate_post; visit_count; } inv_map_post in
      inv_map'', Sched.schedule_succs work_queue node in
    if M.mem node_id inv_map then
      let old_state = M.find node_id inv_map in
      let widened_pre =
        A.widen ~prev:old_state.pre ~next:astate_pre ~num_iters:old_state.visit_count in
      if A.(<=) ~lhs:widened_pre ~rhs:old_state.pre
      then
        begin
          L.out "Old state contains new, not updating invariant or scheduling succs@.";
          inv_map, work_queue
        end
      else
        begin
          L.out "Widening: %a <widen> %a = %a@."
            A.pp astate_pre A.pp old_state.post A.pp widened_pre;
          update_inv_map widened_pre (old_state.visit_count + 1)
        end
    else
      (* first time visiting this node *)
      let visit_count = 1 in
      update_inv_map astate_pre visit_count

  let rec exec_worklist cfg work_queue inv_map proc_data =
    let compute_pre node inv_map =
      (* if the [pred] -> [node] transition was normal, use post([pred]) *)
      let extract_post_ pred = extract_post (CFG.id pred) inv_map in
      let normal_posts = IList.map extract_post_ (CFG.normal_preds cfg node) in
      (* if the [pred] -> [node] transition was exceptional, use pre([pred]) *)
      let extract_pre_f acc pred = extract_pre (CFG.id pred) inv_map :: acc in
      let all_posts = IList.fold_left extract_pre_f normal_posts (CFG.exceptional_preds cfg node) in
      match IList.flatten_options all_posts with
      | post :: posts -> Some (IList.fold_left A.join post posts)
      | [] -> None in
    match Sched.pop work_queue with
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
    let start_node = CFG.start_node cfg in
    let inv_map', work_queue' =
      exec_node start_node A.initial (Sched.empty cfg) M.empty proc_data in
    exec_worklist cfg work_queue' inv_map' proc_data

  (* compute and return an invariant map for [pdesc] *)
  let exec_pdesc ({ ProcData.pdesc; } as proc_data) =
    L.out "Starting analysis of %a@." Procname.pp (Cfg.Procdesc.get_proc_name pdesc);
    exec_cfg (CFG.from_pdesc pdesc) proc_data

  (* compute and return the postcondition of [pdesc] *)
  let compute_post ({ ProcData.pdesc; } as proc_data) =
    let cfg = CFG.from_pdesc pdesc in
    let inv_map = exec_cfg cfg proc_data in
    extract_post (CFG.id (CFG.exit_node cfg)) inv_map

  module Interprocedural (Summ : Summary.S with type summary = A.astate) = struct

    let checker { Callbacks.get_proc_desc; proc_desc; proc_name; tenv; } extras =
      let post_opt = ref None in
      let analyze_ondemand pdesc =
        match compute_post (ProcData.make pdesc tenv extras) with
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

module Make (C : ProcCfg.S) (S : Scheduler.Make) (T : TransferFunctions.Make) =
  MakeNoCFG (S (C)) (T (C))
