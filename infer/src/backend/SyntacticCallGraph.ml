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
