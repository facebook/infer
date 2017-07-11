(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

module type S = sig
  module CFG : ProcCfg.S

  type t

  (* schedule the successors of [node] *)

  val schedule_succs : t -> CFG.node -> t

  (* remove and return the node with the highest priority, the ids of its visited
     predecessors, and the new schedule *)

  val pop : t -> (CFG.node * CFG.id list * t) option

  val empty : CFG.t -> t
end

module type Make = functor (CFG : ProcCfg.S) -> sig
  include S with module CFG = CFG
end

(* simple scheduler that visits CFG nodes in reverse postorder. fast/precise for straightline code
   and conditionals; not as good for loops (may visit nodes after a loop multiple times). *)
module ReversePostorder (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module M = ProcCfg.NodeIdMap (CFG)

  module WorkUnit = struct
    module IdSet = ProcCfg.NodeIdSet (CFG)

    type t =
      { node: CFG.node
      ; (* node whose instructions will be analyzed *)
      visited_preds: IdSet.t
      ; (* predecessors of [node] we have already visited in current iter *)
      priority: int
      (* |preds| - |visited preds|. *) }

    let node t = t.node

    let visited_preds t = IdSet.elements t.visited_preds

    let priority t = t.priority

    let compute_priority cfg node visited_preds =
      List.length (CFG.preds cfg node) - IdSet.cardinal visited_preds

    let make cfg node =
      let visited_preds = IdSet.empty in
      let priority = compute_priority cfg node visited_preds in
      {node; visited_preds; priority}

    (* add [node_id] to the visited preds for [t] *)
    let add_visited_pred cfg t node_id =
      let visited_preds' = IdSet.add node_id t.visited_preds in
      let priority' = compute_priority cfg t.node visited_preds' in
      {t with visited_preds= visited_preds'; priority= priority'}
  end

  type t = {worklist: WorkUnit.t M.t; cfg: CFG.t}

  (* schedule the succs of [node] for analysis *)
  let schedule_succs t node =
    let node_id = CFG.id node in
    (* mark [node] as a visited pred of [node_to_schedule] and schedule it *)
    let schedule_succ worklist_acc node_to_schedule =
      let id_to_schedule = CFG.id node_to_schedule in
      let old_work =
        try M.find id_to_schedule worklist_acc
        with Not_found -> WorkUnit.make t.cfg node_to_schedule
      in
      let new_work = WorkUnit.add_visited_pred t.cfg old_work node_id in
      M.add id_to_schedule new_work worklist_acc
    in
    let new_worklist = List.fold ~f:schedule_succ ~init:t.worklist (CFG.succs t.cfg node) in
    {t with worklist= new_worklist}

  (* remove and return the node with the highest priority (note that smaller integers have higher
     priority), the ids of its visited predecessors, and new schedule *)
  (* TODO: could do this slightly more efficiently by keeping a list of priority zero nodes for
     quick popping, and do a linear search only when this list is empty *)
  let pop t =
    try
      let init_id, init_work = M.choose t.worklist in
      let init_priority = WorkUnit.priority init_work in
      let max_priority_id, _ =
        M.fold
          (fun id work (lowest_id, lowest_priority) ->
            let priority = WorkUnit.priority work in
            if priority < lowest_priority then (id, priority) else (lowest_id, lowest_priority))
          t.worklist (init_id, init_priority)
      in
      let max_priority_work = M.find max_priority_id t.worklist in
      let node = WorkUnit.node max_priority_work in
      let t' = {t with worklist= M.remove (CFG.id node) t.worklist} in
      Some (node, WorkUnit.visited_preds max_priority_work, t')
    with Not_found -> None

  let empty cfg = {worklist= M.empty; cfg}
end
