(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module AnalysisState = State

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

(** use this as [pp_instr] everywhere a SIL CFG is expected *)
let pp_sil_instr _ instr =
  Some (fun f -> F.fprintf f "@[<h>%a;@]@;" (Sil.pp_instr ~print_types:false Pp.text) instr)


module type S = sig
  module TransferFunctions : TransferFunctions.SIL

  module InvariantMap = TransferFunctions.CFG.Node.IdMap

  type invariant_map = TransferFunctions.Domain.t State.t InvariantMap.t

  val compute_post :
       ?do_narrowing:bool
    -> ?pp_instr:(TransferFunctions.Domain.t -> Sil.instr -> (Format.formatter -> unit) option)
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
    -> TransferFunctions.Domain.t option

  val exec_cfg :
       ?do_narrowing:bool
    -> TransferFunctions.CFG.t
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
    -> invariant_map

  val exec_pdesc :
       ?do_narrowing:bool
    -> TransferFunctions.extras ProcData.t
    -> initial:TransferFunctions.Domain.t
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

  type invariant_map = Domain.t State.t InvariantMap.t

  (** extract the state of node [n] from [inv_map] *)
  let extract_state node_id inv_map = InvariantMap.find_opt node_id inv_map

  (** extract the postcondition of node [n] from [inv_map] *)
  let extract_post node_id inv_map = extract_state node_id inv_map |> Option.map ~f:State.post

  (** extract the precondition of node [n] from [inv_map] *)
  let extract_pre node_id inv_map = extract_state node_id inv_map |> Option.map ~f:State.pre

  let debug_absint_operation op =
    let pp_op fmt op =
      match op with
      | `Join _ ->
          F.pp_print_string fmt "JOIN"
      | `Widen (num_iters, _) ->
          F.fprintf fmt "WIDEN(num_iters= %d)" num_iters
    in
    let left, right, result = match op with `Join lrr | `Widen (_, lrr) -> lrr in
    let pp_right f =
      if phys_equal right left then F.pp_print_string f "= LEFT" else Domain.pp f right
    in
    let pp_result f =
      if phys_equal result left then F.pp_print_string f "= LEFT"
      else if phys_equal result right then F.pp_print_string f "= RIGHT"
      else Domain.pp f result
    in
    L.d_printfln_escaped "%a@\n@\nLEFT:   %a@\nRIGHT:  %t@\nRESULT: %t@." pp_op op Domain.pp left
      pp_right pp_result


  (** reference to log errors only at the innermost recursive call *)
  let logged_error = ref false

  let dump_html ~pp_instr pre instr post_result =
    let pp_post_error f (exn, _, instr) =
      F.fprintf f "Analysis stopped in `%a` by error: %a."
        (Sil.pp_instr ~print_types:false Pp.text)
        instr Exn.pp exn
    in
    let pp_post f post =
      match post with
      | Ok astate_post ->
          if phys_equal astate_post pre then F.pp_print_string f "STATE UNCHANGED"
          else F.fprintf f "STATE:@\n@[%a@]" Domain.pp astate_post
      | Error err ->
          pp_post_error f err
    in
    let pp_all f =
      (* we pass [pre] to [pp_instr] because HIL needs it to interpret temporary variables *)
      match (pp_instr pre instr, post_result) with
      | None, Ok _ ->
          ()
      | None, Error err ->
          pp_post_error f err
      | Some pp_instr, _ ->
          Format.fprintf f "@[<h>INSTR=  %t@]@\n@\n%a@\n" pp_instr pp_post post_result
    in
    L.d_printfln_escaped "%t" pp_all


  let exec_instrs ~pp_instr proc_data node node_id ~visit_count pre inv_map =
    let instrs = CFG.instrs node in
    let post =
      if Config.write_html then L.d_printfln_escaped "PRE STATE:@\n@[%a@]@\n" Domain.pp pre ;
      let compute_post pre instr =
        AnalysisState.set_instr instr ;
        let result =
          try
            let post = TransferFunctions.exec_instr pre proc_data node instr in
            (* don't forget to reset this so we output messages for future errors too *)
            logged_error := false ;
            Ok post
          with exn ->
            (* delay reraising to get a chance to write the debug HTML *)
            let backtrace = Caml.Printexc.get_raw_backtrace () in
            Error (exn, backtrace, instr)
        in
        if Config.write_html then dump_html ~pp_instr pre instr result ;
        result
      in
      if Instrs.is_empty instrs then
        (* hack to ensure that we call `exec_instr` on a node even if it has no instructions *)
        compute_post pre Sil.skip_instr
      else Container.fold_result ~fold:Instrs.fold ~init:pre instrs ~f:compute_post
    in
    match post with
    | Ok astate_post ->
        InvariantMap.add node_id {State.pre; post= astate_post; visit_count} inv_map
    | Error (AbstractDomain.Stop_analysis, _, _) ->
        raise_notrace AbstractDomain.Stop_analysis
    | Error (exn, backtrace, instr) ->
        if not !logged_error then (
          L.internal_error "In instruction %a@\n" (Sil.pp_instr ~print_types:true Pp.text) instr ;
          logged_error := true ) ;
        Caml.Printexc.raise_with_backtrace exn backtrace


  (* Note on narrowing operations: we defines the narrowing operations simply to take a smaller one.
     So, as of now, the termination of narrowing is not guaranteed in general. *)
  let exec_node ~pp_instr ({ProcData.summary} as proc_data) node ~is_loop_head ~is_narrowing
      astate_pre inv_map =
    let node_id = Node.id node in
    let update_inv_map pre ~visit_count =
      let inv_map' = exec_instrs ~pp_instr proc_data node node_id ~visit_count pre inv_map in
      (inv_map', DidNotReachFixPoint)
    in
    let inv_map, converged =
      if InvariantMap.mem node_id inv_map then
        let old_state = InvariantMap.find node_id inv_map in
        let new_pre =
          if is_loop_head && not is_narrowing then (
            let num_iters = (old_state.State.visit_count :> int) in
            let prev = old_state.State.pre in
            let next = astate_pre in
            let res = Domain.widen ~prev ~next ~num_iters in
            if Config.write_html then debug_absint_operation (`Widen (num_iters, (prev, next, res))) ;
            res )
          else astate_pre
        in
        if
          if is_narrowing then
            (old_state.State.visit_count :> int) > Config.max_narrows
            || Domain.leq ~lhs:old_state.State.pre ~rhs:new_pre
          else Domain.leq ~lhs:new_pre ~rhs:old_state.State.pre
        then (inv_map, ReachedFixPoint)
        else if is_narrowing && not (Domain.leq ~lhs:new_pre ~rhs:old_state.State.pre) then (
          L.(debug Analysis Verbose)
            "Terminate narrowing because old and new states are not comparable at %a:%a@."
            Typ.Procname.pp (Summary.get_proc_name summary) Node.pp_id node_id ;
          (inv_map, ReachedFixPoint) )
        else
          let visit_count' =
            VisitCount.succ ~pdesc:(Summary.get_proc_desc summary) old_state.State.visit_count
          in
          update_inv_map new_pre ~visit_count:visit_count'
      else
        (* first time visiting this node *)
        update_inv_map astate_pre ~visit_count:VisitCount.first_time
    in
    ( match converged with
    | ReachedFixPoint ->
        L.d_printfln "Fixpoint reached.@."
    | DidNotReachFixPoint ->
        () ) ;
    (inv_map, converged)


  (* shadowed for HTML debug *)
  let exec_node ~pp_instr proc_data node ~is_loop_head ~is_narrowing astate_pre inv_map =
    NodePrinter.with_session (Node.underlying_node node)
      ~kind:(if is_narrowing then `ExecNodeNarrowing else `ExecNode)
      ~pp_name:(TransferFunctions.pp_session_name node)
      ~f:(fun () ->
        exec_node ~pp_instr proc_data node ~is_loop_head ~is_narrowing astate_pre inv_map )


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
              let res = Domain.join post joined_post in
              if Config.write_html then debug_absint_operation (`Join (joined_post, post, res)) ;
              Some res ) )


  (* shadowed for HTML debug *)
  let compute_pre cfg node inv_map =
    NodePrinter.with_session (Node.underlying_node node) ~kind:`ComputePre
      ~pp_name:(TransferFunctions.pp_session_name node) ~f:(fun () -> compute_pre cfg node inv_map
    )


  (** compute and return an invariant map for [pdesc] *)
  let make_exec_pdesc ~exec_cfg_internal ({ProcData.summary} as proc_data) ~do_narrowing ~initial =
    exec_cfg_internal ~pp_instr:pp_sil_instr
      (CFG.from_pdesc (Summary.get_proc_desc summary))
      proc_data ~do_narrowing ~initial


  (** compute and return the postcondition of [pdesc] *)
  let make_compute_post ~exec_cfg_internal ?(pp_instr = pp_sil_instr)
      ({ProcData.summary} as proc_data) ~do_narrowing ~initial =
    let cfg = CFG.from_pdesc (Summary.get_proc_desc summary) in
    let inv_map = exec_cfg_internal ~pp_instr cfg proc_data ~do_narrowing ~initial in
    extract_post (Node.id (CFG.exit_node cfg)) inv_map
end

module MakeWithScheduler
    (Scheduler : Scheduler.S)
    (TransferFunctions : TransferFunctions.SIL with module CFG = Scheduler.CFG) =
struct
  include AbstractInterpreterCommon (TransferFunctions)

  let rec exec_worklist ~pp_instr cfg ({ProcData.summary} as proc_data) work_queue inv_map =
    match Scheduler.pop work_queue with
    | Some (_, [], work_queue') ->
        exec_worklist ~pp_instr cfg proc_data work_queue' inv_map
    | Some (node, _, work_queue') ->
        let inv_map_post, work_queue_post =
          match compute_pre cfg node inv_map with
          | Some astate_pre -> (
              let is_loop_head = CFG.is_loop_head (Summary.get_proc_desc summary) node in
              match
                exec_node ~pp_instr proc_data node ~is_loop_head ~is_narrowing:false astate_pre
                  inv_map
              with
              | inv_map, ReachedFixPoint ->
                  (inv_map, work_queue')
              | inv_map, DidNotReachFixPoint ->
                  (inv_map, Scheduler.schedule_succs work_queue' node) )
          | None ->
              (inv_map, work_queue')
        in
        exec_worklist ~pp_instr cfg proc_data work_queue_post inv_map_post
    | None ->
        inv_map


  (** compute and return an invariant map for [cfg] *)
  let exec_cfg_internal ~pp_instr cfg proc_data ~do_narrowing:_ ~initial =
    let start_node = CFG.start_node cfg in
    let inv_map, _did_not_reach_fix_point =
      exec_node ~pp_instr proc_data start_node ~is_loop_head:false ~is_narrowing:false initial
        InvariantMap.empty
    in
    let work_queue = Scheduler.schedule_succs (Scheduler.empty cfg) start_node in
    exec_worklist ~pp_instr cfg proc_data work_queue inv_map


  let exec_cfg ?do_narrowing:_ = exec_cfg_internal ~pp_instr:pp_sil_instr ~do_narrowing:false

  let exec_pdesc ?do_narrowing:_ = make_exec_pdesc ~exec_cfg_internal ~do_narrowing:false

  let compute_post ?do_narrowing:_ = make_compute_post ~exec_cfg_internal ~do_narrowing:false
end

module MakeUsingWTO (TransferFunctions : TransferFunctions.SIL) = struct
  include AbstractInterpreterCommon (TransferFunctions)

  let debug_wto wto node =
    let underlying_node = Node.underlying_node node in
    NodePrinter.with_session underlying_node ~kind:`WTO
      ~pp_name:(TransferFunctions.pp_session_name node) ~f:(fun () ->
        let pp_node fmt node = node |> Node.id |> Node.pp_id fmt in
        L.d_printfln "%a" (WeakTopologicalOrder.Partition.pp ~pp_node) wto ;
        let loop_heads =
          wto |> IContainer.to_rev_list ~fold:WeakTopologicalOrder.Partition.fold_heads |> List.rev
        in
        L.d_printfln "Loop heads: %a" (Pp.seq pp_node) loop_heads )


  let exec_wto_node ~pp_instr cfg proc_data inv_map node ~is_loop_head ~is_narrowing =
    match compute_pre cfg node inv_map with
    | Some astate_pre ->
        exec_node ~pp_instr proc_data node ~is_loop_head ~is_narrowing astate_pre inv_map
    | None ->
        L.(die InternalError) "Could not compute the pre of a node"


  (* [WidenThenNarrow] mode is to narrow the outermost loops eagerly, so that over-approximated
     widened values do not flow to the following code.

     Problem: There are two phases for finding a fixpoint, widening and narrowing.  First, it finds
     a fixpoint with widening, in function level.  After that, it finds a fixpoint with narrowing.
     A problem is that sometimes an overly-approximated, imprecise, values by widening are flowed to
     the following loops.  They are hard to narrow in the narrowing phase because there is a cycle
     preventing it.

     To mitigate the problem, it tries to do narrowing, in loop level, right after it found a
     fixpoint of a loop.  Thus, it narrows before the widened values are flowed to the following
     loops.  In order to guarantee the termination of the analysis, this eager narrowing is applied
     only to the outermost loops or when the first visits of each loops. *)
  type mode = Widen | WidenThenNarrow | Narrow

  let is_narrowing_of = function Widen | WidenThenNarrow -> false | Narrow -> true

  let rec exec_wto_component ~pp_instr cfg proc_data inv_map head ~is_loop_head ~mode
      ~is_first_visit rest =
    let is_narrowing = is_narrowing_of mode in
    match exec_wto_node ~pp_instr cfg proc_data inv_map head ~is_loop_head ~is_narrowing with
    | inv_map, ReachedFixPoint ->
        if is_narrowing && is_first_visit then
          exec_wto_rest ~pp_instr cfg proc_data inv_map head ~mode ~is_first_visit rest
        else inv_map
    | inv_map, DidNotReachFixPoint ->
        exec_wto_rest ~pp_instr cfg proc_data inv_map head ~mode ~is_first_visit rest


  and exec_wto_rest ~pp_instr cfg proc_data inv_map head ~mode ~is_first_visit rest =
    let inv_map = exec_wto_partition ~pp_instr cfg proc_data ~mode ~is_first_visit inv_map rest in
    exec_wto_component ~pp_instr cfg proc_data inv_map head ~is_loop_head:true ~mode
      ~is_first_visit:false rest


  and exec_wto_partition ~pp_instr cfg proc_data ~mode ~is_first_visit inv_map
      (partition : CFG.Node.t WeakTopologicalOrder.Partition.t) =
    match partition with
    | Empty ->
        inv_map
    | Node {node; next} ->
        let inv_map =
          exec_wto_node ~pp_instr cfg proc_data ~is_narrowing:(is_narrowing_of mode) inv_map node
            ~is_loop_head:false
          |> fst
        in
        exec_wto_partition ~pp_instr cfg proc_data ~mode ~is_first_visit inv_map next
    | Component {head; rest; next} ->
        let inv_map =
          match mode with
          | Widen when is_first_visit ->
              do_widen_then_narrow ~pp_instr cfg proc_data inv_map head ~is_first_visit rest
          | Widen | Narrow ->
              exec_wto_component ~pp_instr cfg proc_data inv_map head ~is_loop_head:false ~mode
                ~is_first_visit rest
          | WidenThenNarrow ->
              do_widen_then_narrow ~pp_instr cfg proc_data inv_map head ~is_first_visit rest
        in
        exec_wto_partition ~pp_instr cfg proc_data ~mode ~is_first_visit inv_map next


  and do_widen_then_narrow ~pp_instr cfg proc_data inv_map head ~is_first_visit rest =
    let inv_map =
      exec_wto_component ~pp_instr cfg proc_data inv_map head ~is_loop_head:false ~mode:Widen
        ~is_first_visit rest
    in
    exec_wto_component ~pp_instr cfg proc_data inv_map head ~is_loop_head:false ~mode:Narrow
      ~is_first_visit rest


  let exec_cfg_internal ~pp_instr cfg proc_data ~do_narrowing ~initial =
    let wto = CFG.wto cfg in
    let exec_cfg ~mode inv_map =
      match wto with
      | Empty ->
          inv_map (* empty cfg *)
      | Node {node= start_node; next} as wto ->
          if Config.write_html then debug_wto wto start_node ;
          let inv_map, _did_not_reach_fix_point =
            exec_node ~pp_instr proc_data start_node ~is_loop_head:false
              ~is_narrowing:(is_narrowing_of mode) initial inv_map
          in
          exec_wto_partition ~pp_instr cfg proc_data ~mode ~is_first_visit:true inv_map next
      | Component _ ->
          L.(die InternalError) "Did not expect the start node to be part of a loop"
    in
    if do_narrowing then exec_cfg ~mode:WidenThenNarrow InvariantMap.empty |> exec_cfg ~mode:Narrow
    else exec_cfg ~mode:Widen InvariantMap.empty


  let exec_cfg ?(do_narrowing = false) = exec_cfg_internal ~pp_instr:pp_sil_instr ~do_narrowing

  let exec_pdesc ?(do_narrowing = false) = make_exec_pdesc ~exec_cfg_internal ~do_narrowing

  let compute_post ?(do_narrowing = false) = make_compute_post ~exec_cfg_internal ~do_narrowing
end

module type Make = functor (TransferFunctions : TransferFunctions.SIL) ->
  S with module TransferFunctions = TransferFunctions

module MakeRPO (T : TransferFunctions.SIL) =
  MakeWithScheduler (Scheduler.ReversePostorder (T.CFG)) (T)
module MakeWTO (T : TransferFunctions.SIL) = MakeUsingWTO (T)
