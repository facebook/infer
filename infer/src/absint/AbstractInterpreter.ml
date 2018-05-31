(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type 'a state = {pre: 'a; post: 'a; visit_count: int}

module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap : Caml.Map.S with type key = TransferFunctions.CFG.id

  type invariant_map = TransferFunctions.Domain.astate state InvariantMap.t

  val compute_post :
    ?debug:bool -> TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate
    -> TransferFunctions.Domain.astate option

  val exec_cfg :
    TransferFunctions.CFG.t -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate -> debug:bool -> invariant_map

  val exec_pdesc :
    TransferFunctions.extras ProcData.t -> initial:TransferFunctions.Domain.astate -> invariant_map

  val extract_post : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  val extract_pre : InvariantMap.key -> 'a state InvariantMap.t -> 'a option

  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
end

module MakeNoCFG
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.SIL with module CFG = Scheduler.CFG) =
struct
  module CFG = Scheduler.CFG
  module InvariantMap = CFG.IdMap
  module TransferFunctions = TransferFunctions
  module Domain = TransferFunctions.Domain

  type invariant_map = Domain.astate state InvariantMap.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map = InvariantMap.find_opt node_id inv_map

  (** extract the postcondition of node [n] from [inv_map] *)
  let extract_post node_id inv_map =
    match extract_state node_id inv_map with Some state -> Some state.post | None -> None


  (** extract the precondition of node [n] from [inv_map] *)
  let extract_pre node_id inv_map =
    match extract_state node_id inv_map with Some state -> Some state.pre | None -> None


  let exec_node node astate_pre work_queue inv_map ({ProcData.pdesc} as proc_data) ~debug =
    let node_id = CFG.id node in
    let update_inv_map pre ~visit_count =
      let compute_post pre instr = TransferFunctions.exec_instr pre proc_data node instr in
      (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
      let instrs =
        let instrs = CFG.instrs node in
        if Instrs.is_empty instrs then Instrs.single Sil.skip_instr else instrs
      in
      if debug then
        NodePrinter.start_session
          ~pp_name:(TransferFunctions.pp_session_name node)
          (CFG.underlying_node node) ;
      let astate_post = Instrs.fold ~f:compute_post ~init:pre instrs in
      if debug then (
        L.d_strln
          (Format.asprintf "PRE: %a@.INSTRS: %aPOST: %a@." Domain.pp pre
             (Instrs.pp Pp.(html Green))
             instrs Domain.pp astate_post) ;
        NodePrinter.finish_session (CFG.underlying_node node) ) ;
      let inv_map' = InvariantMap.add node_id {pre; post= astate_post; visit_count} inv_map in
      (inv_map', Scheduler.schedule_succs work_queue node)
    in
    if InvariantMap.mem node_id inv_map then (
      let old_state = InvariantMap.find node_id inv_map in
      let widened_pre =
        if CFG.is_loop_head pdesc node then
          Domain.widen ~prev:old_state.pre ~next:astate_pre ~num_iters:old_state.visit_count
        else astate_pre
      in
      if Domain.( <= ) ~lhs:widened_pre ~rhs:old_state.pre then (inv_map, work_queue)
      else
        let visit_count' = old_state.visit_count + 1 in
        if visit_count' > Config.max_widens then
          L.(die InternalError)
            "Exceeded max widening threshold %d while analyzing %a. Please check your widening \
             operator or increase the threshold"
            Config.max_widens Typ.Procname.pp (Procdesc.get_proc_name pdesc) ;
        update_inv_map widened_pre ~visit_count:visit_count' )
    else (* first time visiting this node *)
      update_inv_map astate_pre ~visit_count:1


  let rec exec_worklist cfg work_queue inv_map proc_data ~debug =
    let compute_pre node inv_map =
      let extract_post_ pred = extract_post (CFG.id pred) inv_map in
      CFG.fold_preds cfg node ~init:None ~f:(fun joined_post_opt pred ->
          match extract_post_ pred with
          | None ->
              joined_post_opt
          | Some post as some_post ->
            match joined_post_opt with
            | None ->
                some_post
            | Some joined_post ->
                Some (Domain.join joined_post post) )
    in
    match Scheduler.pop work_queue with
    | Some (_, [], work_queue') ->
        exec_worklist cfg work_queue' inv_map proc_data ~debug
    | Some (node, _, work_queue') ->
        let inv_map_post, work_queue_post =
          match compute_pre node inv_map with
          | Some astate_pre ->
              exec_node node astate_pre work_queue' inv_map proc_data ~debug
          | None ->
              (inv_map, work_queue')
        in
        exec_worklist cfg work_queue_post inv_map_post proc_data ~debug
    | None ->
        inv_map


  (* compute and return an invariant map for [cfg] *)
  let exec_cfg cfg proc_data ~initial ~debug =
    let start_node = CFG.start_node cfg in
    let inv_map', work_queue' =
      exec_node start_node initial (Scheduler.empty cfg) InvariantMap.empty proc_data ~debug
    in
    exec_worklist cfg work_queue' inv_map' proc_data ~debug


  (* compute and return an invariant map for [pdesc] *)
  let exec_pdesc ({ProcData.pdesc} as proc_data) =
    exec_cfg (CFG.from_pdesc pdesc) proc_data ~debug:Config.write_html


  (* compute and return the postcondition of [pdesc] *)
  let compute_post ?(debug= Config.write_html) ({ProcData.pdesc} as proc_data) ~initial =
    let cfg = CFG.from_pdesc pdesc in
    let inv_map = exec_cfg cfg proc_data ~initial ~debug in
    extract_post (CFG.id (CFG.exit_node cfg)) inv_map
end

module MakeWithScheduler (C : ProcCfg.S) (S : Scheduler.Make) (T : TransferFunctions.MakeSIL) =
  MakeNoCFG (S (C)) (T (C))
module Make (C : ProcCfg.S) (T : TransferFunctions.MakeSIL) =
  MakeWithScheduler (C) (Scheduler.ReversePostorder) (T)
