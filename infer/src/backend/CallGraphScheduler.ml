(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let bottom_up call_graph =
  let open TaskSchedulerTypes in
  let remaining = ref (CallGraph.n_procs call_graph) in
  let remaining_tasks () = !remaining in
  let pending : CallGraph.Node.t Queue.t = Queue.create () in
  let fill_queue () = CallGraph.iter_unflagged_leaves ~f:(Queue.enqueue pending) call_graph in
  (* prime the pending queue so that [empty] doesn't immediately return true *)
  fill_queue () ;
  let scheduled = ref 0 in
  let is_empty () =
    let empty = Int.equal 0 !scheduled && Queue.is_empty pending in
    if empty then (
      remaining := 0 ;
      L.progress "Finished call graph scheduling, %d procs remaining (in, or reaching, cycles).@."
        (CallGraph.n_procs call_graph) ;
      if Config.debug_level_analysis > 0 then CallGraph.to_dotty call_graph "cycles.dot" ;
      (* save some memory *)
      CallGraph.reset call_graph ;
      (* there is no equivalent to [Hashtbl.reset] so set capacity to min, freeing the old array *)
      Queue.set_capacity pending 1 ) ;
    empty
  in
  let rec next () =
    match Queue.dequeue pending with
    | None ->
        fill_queue () ;
        if Queue.is_empty pending then None else next ()
    | Some n when n.flag || not (CallGraph.mem call_graph n.id) ->
        next ()
    | Some n ->
        incr scheduled ;
        CallGraph.Node.set_flag n ;
        Some (Procname n.pname)
  in
  let finished ~result:_ = function
    | Procname pname ->
        decr remaining ;
        decr scheduled ;
        CallGraph.remove call_graph pname
    | File _ | ProcUID _ ->
        L.die InternalError "Only Procnames are scheduled but File/ProcUID target was received"
  in
  {ProcessPool.TaskGenerator.remaining_tasks; is_empty; finished; next}
