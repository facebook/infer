(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type S = sig
  module CFG : ProcCfg.S

  type t

  val schedule_succs : t -> CFG.Node.t -> t
  (** schedule the successors of [node] *)

  val pop : t -> (CFG.Node.t * CFG.Node.id list * t) option
  (** remove and return the node with the highest priority, the ids of its visited
      predecessors, and the new schedule *)

  val empty : CFG.t -> t
end

module type Make = functor (CFG : ProcCfg.S) -> sig
  include S with module CFG = CFG
end

(** simple scheduler that visits CFG nodes in reverse postorder. fast/precise for straightline code
    and conditionals; not as good for loops (may visit nodes after a loop multiple times). *)
module ReversePostorder (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module M = CFG.Node.IdMap

  module WorkUnit = struct
    module IdSet = CFG.Node.IdSet

    type t =
      { node: CFG.Node.t  (** node whose instructions will be analyzed *)
      ; visited_preds: IdSet.t
            (** predecessors of [node] we have already visited in current iter *)
      ; priority: int  (** |preds| - |visited preds|. *) }

    let node t = t.node

    let visited_preds t = IdSet.elements t.visited_preds

    let priority t = t.priority

    let compute_priority cfg node visited_preds =
      Container.length ~fold:(CFG.fold_preds cfg) node - IdSet.cardinal visited_preds


    let make cfg node =
      let visited_preds = IdSet.empty in
      let priority = compute_priority cfg node visited_preds in
      {node; visited_preds; priority}


    (** add [node_id] to the visited preds for [t] *)
    let add_visited_pred cfg t node_id =
      let visited_preds' = IdSet.add node_id t.visited_preds in
      let priority' = compute_priority cfg t.node visited_preds' in
      {t with visited_preds= visited_preds'; priority= priority'}
  end

  type t = {worklist: WorkUnit.t M.t; cfg: CFG.t}

  (** schedule the succs of [node] for analysis *)
  let schedule_succs t node =
    let node_id = CFG.Node.id node in
    (* mark [node] as a visited pred of [node_to_schedule] and schedule it *)
    let schedule_succ worklist_acc node_to_schedule =
      let id_to_schedule = CFG.Node.id node_to_schedule in
      let old_work =
        match M.find_opt id_to_schedule worklist_acc with
        | Some work ->
            work
        | None ->
            WorkUnit.make t.cfg node_to_schedule
      in
      let new_work = WorkUnit.add_visited_pred t.cfg old_work node_id in
      M.add id_to_schedule new_work worklist_acc
    in
    let new_worklist = CFG.fold_succs t.cfg node ~f:schedule_succ ~init:t.worklist in
    {t with worklist= new_worklist}


  (* TODO: could do this slightly more efficiently by keeping a list of priority zero nodes for
     quick popping, and do a linear search only when this list is empty *)

  (** remove and return the node with the highest priority (note that smaller integers have higher
     priority), the ids of its visited predecessors, and new schedule *)
  let pop t =
    try
      let init_id, init_work = M.min_binding t.worklist in
      let init_priority = WorkUnit.priority init_work in
      let max_priority_id, _ =
        M.fold
          (fun id work (lowest_id, lowest_priority) ->
            let priority = WorkUnit.priority work in
            if priority < lowest_priority then (id, priority) else (lowest_id, lowest_priority) )
          t.worklist (init_id, init_priority)
      in
      let max_priority_work = M.find max_priority_id t.worklist in
      let node = WorkUnit.node max_priority_work in
      let t' = {t with worklist= M.remove (CFG.Node.id node) t.worklist} in
      Some (node, WorkUnit.visited_preds max_priority_work, t')
    with Caml.Not_found -> None


  let empty cfg = {worklist= M.empty; cfg}
end
