(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type 'a state = {pre: 'a; post: 'a; visit_count: int}

module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap = TransferFunctions.CFG.Node.IdMap

  type invariant_map = TransferFunctions.Domain.astate state InvariantMap.t

  val compute_post :
       ?debug:bool
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate
    -> TransferFunctions.Domain.astate option

  val exec_cfg :
       TransferFunctions.CFG.t
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate
    -> debug:bool
    -> invariant_map

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
  module Node = CFG.Node
  module TransferFunctions = TransferFunctions
  module InvariantMap = TransferFunctions.CFG.Node.IdMap
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


  let debug_absint_operation op node =
    let pp_name fmt =
      TransferFunctions.pp_session_name node fmt ;
      match op with
      | `Join _ ->
          F.pp_print_string fmt " JOIN"
      | `Widen (num_iters, _) ->
          F.fprintf fmt " WIDEN(num_iters= %d)" num_iters
    in
    let underlying_node = Node.underlying_node node in
    NodePrinter.start_session ~pp_name underlying_node ;
    let left, right, result = match op with `Join lrr | `Widen (_, lrr) -> lrr in
    L.d_strln
      ( Format.asprintf "LEFT: %a@.RIGHT: %a@.RESULT: %a@." Domain.pp left Domain.pp right
          Domain.pp result
      |> Escape.escape_xml ) ;
    NodePrinter.finish_session underlying_node


  let exec_node node astate_pre work_queue inv_map ({ProcData.pdesc} as proc_data) ~debug =
    let node_id = Node.id node in
    let update_inv_map pre ~visit_count =
      let exec_instrs instrs =
        if debug then
          NodePrinter.start_session
            ~pp_name:(TransferFunctions.pp_session_name node)
            (Node.underlying_node node) ;
        let astate_post =
          let compute_post pre instr = TransferFunctions.exec_instr pre proc_data node instr in
          Instrs.fold ~f:compute_post ~init:pre instrs
        in
        if debug then (
          L.d_strln
            ( Format.asprintf "@[PRE: %a@]@\n@[INSTRS: %a@]@[POST: %a@]@." Domain.pp pre
                (Instrs.pp Pp.(html Green))
                instrs Domain.pp astate_post
            |> Escape.escape_xml ) ;
          NodePrinter.finish_session (Node.underlying_node node) ) ;
        let inv_map' = InvariantMap.add node_id {pre; post= astate_post; visit_count} inv_map in
        (inv_map', Scheduler.schedule_succs work_queue node)
      in
      (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
      let instrs = CFG.instrs node in
      if Instrs.is_empty instrs then exec_instrs (Instrs.singleton Sil.skip_instr)
      else exec_instrs instrs
    in
    if InvariantMap.mem node_id inv_map then (
      let old_state = InvariantMap.find node_id inv_map in
      let widened_pre =
        if CFG.is_loop_head pdesc node then (
          let num_iters = old_state.visit_count in
          let prev = old_state.pre in
          let next = astate_pre in
          let res = Domain.widen ~prev ~next ~num_iters in
          if debug then debug_absint_operation (`Widen (num_iters, (prev, next, res))) node ;
          res )
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
      let extract_post_ pred = extract_post (Node.id pred) inv_map in
      CFG.fold_preds cfg node ~init:None ~f:(fun joined_post_opt pred ->
          match extract_post_ pred with
          | None ->
              joined_post_opt
          | Some post as some_post -> (
            match joined_post_opt with
            | None ->
                some_post
            | Some joined_post ->
                let res = Domain.join joined_post post in
                if debug then debug_absint_operation (`Join (joined_post, post, res)) node ;
                Some res ) )
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
  let compute_post ?(debug = Config.write_html) ({ProcData.pdesc} as proc_data) ~initial =
    let cfg = CFG.from_pdesc pdesc in
    let inv_map = exec_cfg cfg proc_data ~initial ~debug in
    extract_post (Node.id (CFG.exit_node cfg)) inv_map
end

module MakeWithScheduler (C : ProcCfg.S) (S : Scheduler.Make) (T : TransferFunctions.MakeSIL) =
  MakeNoCFG (S (C)) (T (C))
module Make (C : ProcCfg.S) (T : TransferFunctions.MakeSIL) =
  MakeWithScheduler (C) (Scheduler.ReversePostorder) (T)
