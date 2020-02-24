(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module IdMap = Procname.Hash

let build_from_captured_procs g =
  let hashcons_pname =
    let pname_tbl : Procname.t IdMap.t = IdMap.create 1001 in
    fun pname ->
      match IdMap.find_opt pname_tbl pname with
      | Some pname' ->
          pname'
      | None ->
          IdMap.add pname_tbl pname pname ; pname
  in
  let db = ResultsDatabase.get_database () in
  let stmt = Sqlite3.prepare db "SELECT proc_name, callees FROM procedures" in
  SqliteUtils.result_fold_rows db ~log:"creating call graph" stmt ~init:() ~f:(fun () stmt ->
      let proc_name = Sqlite3.column stmt 0 |> Procname.SQLite.deserialize |> hashcons_pname in
      let callees =
        Sqlite3.column stmt 1 |> Procname.SQLiteList.deserialize |> List.map ~f:hashcons_pname
      in
      CallGraph.create_node g proc_name callees )


let build_from_sources sources =
  let g = CallGraph.create CallGraph.default_initial_capacity in
  let time0 = Mtime_clock.counter () in
  L.progress "Building call graph...@\n%!" ;
  build_from_captured_procs g ;
  let n_captured = CallGraph.n_procs g in
  List.iter sources ~f:(fun sf ->
      SourceFiles.proc_names_of_source sf |> List.iter ~f:(CallGraph.flag_reachable g) ) ;
  CallGraph.remove_unflagged_and_unflag_all g ;
  CallGraph.trim_id_map g ;
  if Config.debug_level_analysis > 0 then CallGraph.to_dotty g "syntactic_callgraph.dot" ;
  L.progress
    "Built call graph in %a, from %d total procs, %d reachable defined procs and takes %d bytes@."
    Mtime.Span.pp (Mtime_clock.count time0) n_captured (CallGraph.n_procs g)
    (Obj.(reachable_words (repr g)) * (Sys.word_size / 8)) ;
  g


let bottom_up sources : (SchedulerTypes.target, Procname.t) ProcessPool.TaskGenerator.t =
  let open SchedulerTypes in
  let syntactic_call_graph = build_from_sources sources in
  let remaining = ref (CallGraph.n_procs syntactic_call_graph) in
  let remaining_tasks () = !remaining in
  let pending : CallGraph.Node.t Queue.t = Queue.create () in
  let fill_queue () =
    CallGraph.iter_unflagged_leaves ~f:(Queue.enqueue pending) syntactic_call_graph
  in
  (* prime the pending queue so that [empty] doesn't immediately return true *)
  fill_queue () ;
  let scheduled = ref 0 in
  let is_empty () =
    let empty = Int.equal 0 !scheduled && Queue.is_empty pending in
    if empty then (
      remaining := 0 ;
      L.progress "Finished call graph scheduling, %d procs remaining (in, or reaching, cycles).@."
        (CallGraph.n_procs syntactic_call_graph) ;
      if Config.debug_level_analysis > 0 then CallGraph.to_dotty syntactic_call_graph "cycles.dot" ;
      (* save some memory *)
      CallGraph.reset syntactic_call_graph ;
      (* there is no equivalent to [Hashtbl.reset] so set capacity to min, freeing the old array *)
      Queue.set_capacity pending 1 ) ;
    empty
  in
  let rec next () =
    match Queue.dequeue pending with
    | None ->
        fill_queue () ;
        if Queue.is_empty pending then None else next ()
    | Some n when n.flag || not (CallGraph.mem syntactic_call_graph n.id) ->
        next ()
    | Some n ->
        incr scheduled ;
        CallGraph.flag syntactic_call_graph n.pname ;
        Some (Procname n.pname)
  in
  let finished ~result:_ = function
    | Procname pname ->
        decr remaining ;
        decr scheduled ;
        CallGraph.remove syntactic_call_graph pname
    | File _ ->
        L.die InternalError "Only Procnames are scheduled but File target was received"
  in
  {remaining_tasks; is_empty; finished; next}


let make sources = ProcessPool.TaskGenerator.chain (bottom_up sources) (FileScheduler.make sources)
