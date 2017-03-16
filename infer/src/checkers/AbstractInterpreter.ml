(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging

type 'a state = { pre: 'a; post: 'a; visit_count: int; }

module type S = sig
  module TransferFunctions : TransferFunctions.S

  module InvariantMap : Caml.Map.S with type key = TransferFunctions.CFG.id

  type invariant_map = TransferFunctions.Domain.astate state InvariantMap.t

  val compute_post :
    TransferFunctions.extras ProcData.t ->
    initial:TransferFunctions.Domain.astate ->
    TransferFunctions.Domain.astate option

  val exec_cfg :
    TransferFunctions.CFG.t ->
    TransferFunctions.extras ProcData.t ->
    initial:TransferFunctions.Domain.astate ->
    invariant_map

  val exec_pdesc :
    TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate -> invariant_map

  val extract_post : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  val extract_pre : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
end

module MakeNoCFG
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.S with module CFG = Scheduler.CFG) = struct

  module CFG = Scheduler.CFG
  module InvariantMap = ProcCfg.NodeIdMap (CFG)
  module TransferFunctions = TransferFunctions
  module Domain = TransferFunctions.Domain

  type invariant_map = Domain.astate state InvariantMap.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map =
    try Some (InvariantMap.find node_id inv_map)
    with Not_found -> None

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

  let exec_node node astate_pre work_queue inv_map ({ ProcData.pdesc; } as proc_data) =
    let node_id = CFG.id node in
    let update_inv_map pre visit_count =
      let compute_post (pre, inv_map) (instr, id_opt) =
        let post = TransferFunctions.exec_instr pre proc_data node instr in
        match id_opt with
        | Some id -> post, InvariantMap.add id { pre; post; visit_count; } inv_map
        | None -> post, inv_map in
      (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
      let instr_ids = match CFG.instr_ids node with
        | [] -> [Sil.skip_instr, None]
        | l -> l in
      let underlying_node = CFG.underlying_node node in
      NodePrinter.start_session underlying_node;
      let astate_post, inv_map_post =
        List.fold ~f:compute_post ~init:(pre, inv_map) instr_ids in
      if Config.write_html
      then
        begin
          let str =
            let instrs = List.map ~f:fst instr_ids in
            Format.asprintf "PRE: %a@.INSTRS: %aPOST: %a@."
              Domain.pp pre (Sil.pp_instr_list Pp.text) instrs Domain.pp astate_post in
          L.d_strln str
        end;
      NodePrinter.finish_session underlying_node;
      let inv_map'' =
        InvariantMap.add node_id { pre; post=astate_post; visit_count; } inv_map_post in
      inv_map'', Scheduler.schedule_succs work_queue node in
    if InvariantMap.mem node_id inv_map
    then
      let old_state = InvariantMap.find node_id inv_map in
      let widened_pre =
        if CFG.is_loop_head pdesc node
        then Domain.widen ~prev:old_state.pre ~next:astate_pre ~num_iters:old_state.visit_count
        else astate_pre
      in
      if Domain.(<=) ~lhs:widened_pre ~rhs:old_state.pre
      then inv_map, work_queue
      else update_inv_map widened_pre (old_state.visit_count + 1)
    else
      (* first time visiting this node *)
      let visit_count = 1 in
      update_inv_map astate_pre visit_count

  let rec exec_worklist cfg work_queue inv_map proc_data =
    let compute_pre node inv_map =
      (* if the [pred] -> [node] transition was normal, use post([pred]) *)
      let extract_post_ pred = extract_post (CFG.id pred) inv_map in
      let normal_posts = List.map ~f:extract_post_ (CFG.normal_preds cfg node) in
      (* if the [pred] -> [node] transition was exceptional, use pre([pred]) *)
      let extract_pre_f acc pred = extract_pre (CFG.id pred) inv_map :: acc in
      let all_posts =
        List.fold ~f:extract_pre_f ~init:normal_posts (CFG.exceptional_preds cfg node) in
      match List.filter_opt all_posts with
      | post :: posts -> Some (List.fold ~f:Domain.join ~init:post posts)
      | [] -> None in
    match Scheduler.pop work_queue with
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
  let exec_cfg cfg proc_data ~initial =
    let start_node = CFG.start_node cfg in
    let inv_map', work_queue' =
      exec_node start_node initial (Scheduler.empty cfg) InvariantMap.empty proc_data in
    exec_worklist cfg work_queue' inv_map' proc_data

  (* compute and return an invariant map for [pdesc] *)
  let exec_pdesc ({ ProcData.pdesc; } as proc_data) =
    exec_cfg (CFG.from_pdesc pdesc) proc_data

  (* compute and return the postcondition of [pdesc] *)
  let compute_post ({ ProcData.pdesc; } as proc_data) ~initial =
    let cfg = CFG.from_pdesc pdesc in
    let inv_map = exec_cfg cfg proc_data ~initial in
    extract_post (CFG.id (CFG.exit_node cfg)) inv_map

end

module Interprocedural (Summ : Summary.S) = struct

  let compute_and_store_post
      ~compute_post ~make_extras { Callbacks.proc_desc; tenv; } =
    match compute_post (ProcData.make proc_desc tenv (make_extras proc_desc)) with
    | Some post ->
        Summ.write_summary (Procdesc.get_proc_name proc_desc) post;
        Some post
    | None ->
        None

end

module MakeWithScheduler (C : ProcCfg.S) (S : Scheduler.Make) (T : TransferFunctions.Make) =
  MakeNoCFG (S (C)) (T (C))

module Make (C : ProcCfg.S) (T : TransferFunctions.Make) =
  MakeWithScheduler (C) (Scheduler.ReversePostorder) (T)
