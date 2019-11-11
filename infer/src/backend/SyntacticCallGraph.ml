(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module IdMap = Typ.Procname.Hash

let build_from_captured_procs g =
  let hashcons_pname =
    let pname_tbl : Typ.Procname.t IdMap.t = IdMap.create 1001 in
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
      let proc_name = Sqlite3.column stmt 0 |> Typ.Procname.SQLite.deserialize |> hashcons_pname in
      let callees =
        Sqlite3.column stmt 1 |> Typ.Procname.SQLiteList.deserialize |> List.map ~f:hashcons_pname
      in
      CallGraph.create_node g proc_name callees )


let build_from_sources g sources =
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
    (Obj.(reachable_words (repr g)) * (Sys.word_size / 8))


let count_procedures () =
  let db = ResultsDatabase.get_database () in
  let stmt = Sqlite3.prepare db "SELECT COUNT(rowid) FROM procedures" in
  let count =
    match SqliteUtils.result_single_column_option db ~log:"counting procedures" stmt with
    | Some (Sqlite3.Data.INT i64) ->
        Int64.to_int i64 |> Option.value ~default:Int.max_value
    | _ ->
        L.die InternalError "Got no result trying to count procedures"
  in
  L.debug Analysis Quiet "Found %d procedures in procedures table.@." count ;
  count


let bottom_up sources : SchedulerTypes.target ProcessPool.TaskGenerator.t =
  let open SchedulerTypes in
  (* this will potentially grossly overapproximate the tasks *)
  let remaining = ref (count_procedures ()) in
  let remaining_tasks () = !remaining in
  let syntactic_call_graph = CallGraph.create CallGraph.default_initial_capacity in
  let initialized = ref false in
  let pending : CallGraph.Node.t list ref = ref [] in
  let scheduled = ref Typ.Procname.Set.empty in
  let is_empty () =
    let empty = !initialized && List.is_empty !pending && Typ.Procname.Set.is_empty !scheduled in
    if empty then (
      remaining := 0 ;
      L.progress "Finished call graph scheduling, %d procs remaining (in, or reaching, cycles).@."
        (CallGraph.n_procs syntactic_call_graph) ;
      if Config.debug_level_analysis > 0 then CallGraph.to_dotty syntactic_call_graph "cycles.dot" ;
      (* save some memory *)
      CallGraph.reset syntactic_call_graph ) ;
    empty
  in
  let rec next_aux () =
    match !pending with
    | [] ->
        pending := CallGraph.get_unflagged_leaves syntactic_call_graph ;
        if List.is_empty !pending then None else next_aux ()
    | n :: ns when n.flag || not (CallGraph.mem syntactic_call_graph n.id) ->
        pending := ns ;
        next_aux ()
    | n :: ns ->
        pending := ns ;
        scheduled := Typ.Procname.Set.add n.pname !scheduled ;
        CallGraph.flag syntactic_call_graph n.pname ;
        Some (Procname n.pname)
  in
  let finished = function
    | File _ ->
        assert false
    | Procname pname ->
        decr remaining ;
        scheduled := Typ.Procname.Set.remove pname !scheduled ;
        CallGraph.remove syntactic_call_graph pname
  in
  let next () =
    (* do construction here, to avoid having the call graph into forked workers *)
    if not !initialized then (
      build_from_sources syntactic_call_graph sources ;
      initialized := true ) ;
    next_aux ()
  in
  {remaining_tasks; is_empty; finished; next}
