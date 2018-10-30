(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module AnalysisState = State

type debug =
  | Default
  | DefaultNoExecInstr_UseFromLowerHilAbstractInterpreterOnly
      (** If Default is used from LowerHil, debug html files will be opened twice and closed twice (boom!),
          because both LowerHil-AI and SIL-AI want to print instructions and pre/post states.
          When using LowerHil-AI, we're not interested in the underlying SIL instructions,
          it's the only case where want to disable it. *)

type exec_node_schedule_result = ReachedFixPoint | DidNotReachFixPoint

module VisitCount : sig
  type t = private int

  val first_time : t

  val succ : pdesc:Procdesc.t -> t -> t
end = struct
  type t = int

  let first_time = 1

  let succ ~pdesc visit_count =
    let visit_count' = visit_count + 1 in
    if visit_count' > Config.max_widens then
      L.(die InternalError)
        "Exceeded max widening threshold %d while analyzing %a. Please check your widening \
         operator or increase the threshold"
        Config.max_widens Typ.Procname.pp (Procdesc.get_proc_name pdesc) ;
    visit_count'
end

module State = struct
  type 'a t = {pre: 'a; post: 'a; visit_count: VisitCount.t}

  let pre {pre} = pre

  let post {post} = post
end

module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap = TransferFunctions.CFG.Node.IdMap

  type invariant_map = TransferFunctions.Domain.astate State.t InvariantMap.t

  val compute_post :
       ?do_narrowing:bool
    -> ?debug:debug
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate
    -> TransferFunctions.Domain.astate option

  val exec_cfg :
       ?do_narrowing:bool
    -> TransferFunctions.CFG.t
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate
    -> invariant_map

  val exec_pdesc :
       ?do_narrowing:bool
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.astate
    -> invariant_map

  val extract_post : InvariantMap.key -> 'a State.t InvariantMap.t -> 'a option

  val extract_pre : InvariantMap.key -> 'a State.t InvariantMap.t -> 'a option

  val extract_state : InvariantMap.key -> 'a InvariantMap.t -> 'a option
end

module AbstractInterpreterCommon (TransferFunctions : TransferFunctions.SIL) = struct
  module CFG = TransferFunctions.CFG
  module Node = CFG.Node
  module TransferFunctions = TransferFunctions
  module InvariantMap = TransferFunctions.CFG.Node.IdMap
  module Domain = TransferFunctions.Domain

  type invariant_map = Domain.astate State.t InvariantMap.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map = InvariantMap.find_opt node_id inv_map

  (** extract the postcondition of node [n] from [inv_map] *)
  let extract_post node_id inv_map = extract_state node_id inv_map |> Option.map ~f:State.post

  (** extract the precondition of node [n] from [inv_map] *)
  let extract_pre node_id inv_map = extract_state node_id inv_map |> Option.map ~f:State.pre

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
    let pp_right f =
      if phys_equal right left then F.pp_print_string f "= LEFT" else Domain.pp f right
    in
    let pp_result f =
      if phys_equal result left then F.pp_print_string f "= LEFT"
      else if phys_equal result right then F.pp_print_string f "= RIGHT"
      else Domain.pp f result
    in
    L.d_printfln_escaped "LEFT: %a@.RIGHT: %t@.RESULT: %t@." Domain.pp left pp_right pp_result ;
    NodePrinter.finish_session underlying_node


  (** reference to log errors only at the innermost recursive call *)
  let logged_error = ref false

  let exec_instrs ~debug proc_data node node_id ~visit_count pre inv_map =
    let on_instrs instrs =
      if Config.write_html && debug <> DefaultNoExecInstr_UseFromLowerHilAbstractInterpreterOnly
      then
        NodePrinter.start_session
          ~pp_name:(TransferFunctions.pp_session_name node)
          (Node.underlying_node node) ;
      let astate_post =
        let compute_post pre instr =
          AnalysisState.set_instr instr ;
          try
            let post = TransferFunctions.exec_instr pre proc_data node instr in
            (* don't forget to reset this so we output messages for future errors too *)
            logged_error := false ;
            post
          with
          | AbstractDomain.Stop_analysis as exn ->
              raise_notrace exn
          | exn ->
              IExn.reraise_after exn ~f:(fun () ->
                  if not !logged_error then (
                    L.internal_error "In instruction %a@\n"
                      (Sil.pp_instr ~print_types:true Pp.text)
                      instr ;
                    logged_error := true ) )
        in
        Instrs.fold ~f:compute_post ~init:pre instrs
      in
      if Config.write_html && debug <> DefaultNoExecInstr_UseFromLowerHilAbstractInterpreterOnly
      then (
        let pp_post f =
          if phys_equal astate_post pre then F.pp_print_string f "= PRE"
          else Domain.pp f astate_post
        in
        L.d_printfln_escaped "@[PRE: %a@]@\n@[INSTRS: %a@]@[POST: %t@]@." Domain.pp pre
          (Instrs.pp Pp.text) instrs pp_post ;
        NodePrinter.finish_session (Node.underlying_node node) ) ;
      InvariantMap.add node_id {State.pre; post= astate_post; visit_count} inv_map
    in
    let instrs = CFG.instrs node in
    if Instrs.is_empty instrs then
      (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
      on_instrs (Instrs.singleton Sil.skip_instr)
    else on_instrs instrs


  (* Note on narrowing operations: we defines the narrowing operations simply to take a smaller one.
     So, as of now, the termination of narrowing is not guaranteed in general. *)
  let exec_node ~debug ({ProcData.pdesc} as proc_data) node ~is_loop_head ~is_narrowing astate_pre
      inv_map =
    let node_id = Node.id node in
    let update_inv_map pre ~visit_count =
      let inv_map' = exec_instrs ~debug proc_data node node_id ~visit_count pre inv_map in
      (inv_map', DidNotReachFixPoint)
    in
    if InvariantMap.mem node_id inv_map then
      let old_state = InvariantMap.find node_id inv_map in
      let new_pre =
        if is_loop_head && not is_narrowing then (
          let num_iters = (old_state.State.visit_count :> int) in
          let prev = old_state.State.pre in
          let next = astate_pre in
          let res = Domain.widen ~prev ~next ~num_iters in
          if Config.write_html then
            debug_absint_operation (`Widen (num_iters, (prev, next, res))) node ;
          res )
        else astate_pre
      in
      if
        if is_narrowing then
          (old_state.State.visit_count :> int) > Config.max_narrows
          || Domain.( <= ) ~lhs:old_state.State.pre ~rhs:new_pre
        else Domain.( <= ) ~lhs:new_pre ~rhs:old_state.State.pre
      then (inv_map, ReachedFixPoint)
      else if is_narrowing && not (Domain.( <= ) ~lhs:new_pre ~rhs:old_state.State.pre) then (
        L.(debug Analysis Verbose)
          "Terminate narrowing because old and new states are not comparable at %a:%a@."
          Typ.Procname.pp (Procdesc.get_proc_name pdesc) Node.pp_id node_id ;
        (inv_map, ReachedFixPoint) )
      else
        let visit_count' = VisitCount.succ ~pdesc old_state.State.visit_count in
        update_inv_map new_pre ~visit_count:visit_count'
    else
      (* first time visiting this node *)
      update_inv_map astate_pre ~visit_count:VisitCount.first_time


  let compute_pre cfg node inv_map =
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
              if Config.write_html then
                debug_absint_operation (`Join (joined_post, post, res)) node ;
              Some res ) )


  (** compute and return an invariant map for [pdesc] *)
  let make_exec_pdesc ~exec_cfg_internal ({ProcData.pdesc} as proc_data) ~do_narrowing ~initial =
    exec_cfg_internal ~debug:Default (CFG.from_pdesc pdesc) proc_data ~do_narrowing ~initial


  (** compute and return the postcondition of [pdesc] *)
  let make_compute_post ~exec_cfg_internal ?(debug = Default) ({ProcData.pdesc} as proc_data)
      ~do_narrowing ~initial =
    let cfg = CFG.from_pdesc pdesc in
    let inv_map = exec_cfg_internal ~debug cfg proc_data ~do_narrowing ~initial in
    extract_post (Node.id (CFG.exit_node cfg)) inv_map
end

module MakeWithScheduler
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.SIL with module CFG = Scheduler.CFG) =
struct
  include AbstractInterpreterCommon (TransferFunctions)

  let rec exec_worklist ~debug cfg ({ProcData.pdesc} as proc_data) work_queue inv_map =
    match Scheduler.pop work_queue with
    | Some (_, [], work_queue') ->
        exec_worklist ~debug cfg proc_data work_queue' inv_map
    | Some (node, _, work_queue') ->
        let inv_map_post, work_queue_post =
          match compute_pre cfg node inv_map with
          | Some astate_pre -> (
              let is_loop_head = CFG.is_loop_head pdesc node in
              match
                exec_node ~debug proc_data node ~is_loop_head ~is_narrowing:false astate_pre
                  inv_map
              with
              | inv_map, ReachedFixPoint ->
                  (inv_map, work_queue')
              | inv_map, DidNotReachFixPoint ->
                  (inv_map, Scheduler.schedule_succs work_queue' node) )
          | None ->
              (inv_map, work_queue')
        in
        exec_worklist ~debug cfg proc_data work_queue_post inv_map_post
    | None ->
        inv_map


  (* compute and return an invariant map for [cfg] *)
  let exec_cfg_internal ~debug cfg proc_data ~do_narrowing:_ ~initial =
    let start_node = CFG.start_node cfg in
    let inv_map, _did_not_reach_fix_point =
      exec_node ~debug proc_data start_node ~is_loop_head:false ~is_narrowing:false initial
        InvariantMap.empty
    in
    let work_queue = Scheduler.schedule_succs (Scheduler.empty cfg) start_node in
    exec_worklist ~debug cfg proc_data work_queue inv_map


  let exec_cfg ?do_narrowing:_ = exec_cfg_internal ~debug:Default ~do_narrowing:false

  let exec_pdesc ?do_narrowing:_ = make_exec_pdesc ~exec_cfg_internal ~do_narrowing:false

  let compute_post ?do_narrowing:_ = make_compute_post ~exec_cfg_internal ~do_narrowing:false
end

module MakeUsingWTO (TransferFunctions : TransferFunctions.SIL) = struct
  include AbstractInterpreterCommon (TransferFunctions)

  let debug_wto wto node =
    let pp_name fmt =
      TransferFunctions.pp_session_name node fmt ;
      F.pp_print_string fmt " WEAK TOPOLOGICAL ORDER"
    in
    let underlying_node = Node.underlying_node node in
    NodePrinter.start_session ~pp_name underlying_node ;
    let pp_node fmt node = node |> Node.id |> Node.pp_id fmt in
    L.d_printfln "%a" (WeakTopologicalOrder.Partition.pp ~pp_node) wto ;
    let loop_heads =
      wto |> IContainer.to_rev_list ~fold:WeakTopologicalOrder.Partition.fold_heads |> List.rev
    in
    L.d_printfln "Loop heads: %a" (Pp.seq pp_node) loop_heads ;
    NodePrinter.finish_session underlying_node


  let exec_wto_node ~debug cfg proc_data inv_map node ~is_loop_head ~is_narrowing =
    match compute_pre cfg node inv_map with
    | Some astate_pre ->
        exec_node ~debug proc_data node ~is_loop_head ~is_narrowing astate_pre inv_map
    | None ->
        L.(die InternalError) "Could not compute the pre of a node"


  let rec exec_wto_component ~debug cfg proc_data inv_map head ~is_loop_head ~is_narrowing rest =
    match exec_wto_node ~debug cfg proc_data inv_map head ~is_loop_head ~is_narrowing with
    | inv_map, ReachedFixPoint ->
        inv_map
    | inv_map, DidNotReachFixPoint ->
        let inv_map = exec_wto_partition ~debug cfg proc_data ~is_narrowing inv_map rest in
        exec_wto_component ~debug cfg proc_data inv_map head ~is_loop_head:true ~is_narrowing rest


  and exec_wto_partition ~debug cfg proc_data ~is_narrowing inv_map
      (partition : CFG.Node.t WeakTopologicalOrder.Partition.t) =
    match partition with
    | Empty ->
        inv_map
    | Node {node; next} ->
        let inv_map =
          exec_wto_node ~debug cfg proc_data ~is_narrowing inv_map node ~is_loop_head:false |> fst
        in
        exec_wto_partition ~debug cfg proc_data ~is_narrowing inv_map next
    | Component {head; rest; next} ->
        let inv_map =
          exec_wto_component ~debug cfg proc_data inv_map head ~is_loop_head:false ~is_narrowing
            rest
        in
        exec_wto_partition ~debug cfg proc_data ~is_narrowing inv_map next


  let exec_cfg_internal ~debug cfg proc_data ~do_narrowing ~initial =
    let wto = CFG.wto cfg in
    let exec_cfg ~is_narrowing inv_map =
      match wto with
      | Empty ->
          inv_map (* empty cfg *)
      | Node {node= start_node; next} as wto ->
          if Config.write_html then debug_wto wto start_node ;
          let inv_map, _did_not_reach_fix_point =
            exec_node ~debug proc_data start_node ~is_loop_head:false ~is_narrowing initial inv_map
          in
          exec_wto_partition ~debug cfg proc_data ~is_narrowing inv_map next
      | Component _ ->
          L.(die InternalError) "Did not expect the start node to be part of a loop"
    in
    let inv_map = exec_cfg ~is_narrowing:false InvariantMap.empty in
    if do_narrowing then exec_cfg ~is_narrowing:true inv_map else inv_map


  let exec_cfg ?(do_narrowing = false) = exec_cfg_internal ~debug:Default ~do_narrowing

  let exec_pdesc ?(do_narrowing = false) = make_exec_pdesc ~exec_cfg_internal ~do_narrowing

  let compute_post ?(do_narrowing = false) = make_compute_post ~exec_cfg_internal ~do_narrowing
end

module type Make = functor (TransferFunctions : TransferFunctions.SIL) -> S
                                                                          with module TransferFunctions = TransferFunctions

module MakeRPO (T : TransferFunctions.SIL) =
  MakeWithScheduler (Scheduler.ReversePostorder (T.CFG)) (T)
module MakeWTO (T : TransferFunctions.SIL) = MakeUsingWTO (T)
