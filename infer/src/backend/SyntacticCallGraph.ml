(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let iter_captured_procs_and_callees f =
  (* Only query the capturedb (this function is run before the analysis phase) *)
  let db = Database.get_database CaptureDatabase in
  (* only load procedure info for those we have a CFG *)
  let stmt =
    Sqlite3.prepare db "SELECT proc_attributes, callees FROM procedures WHERE cfg IS NOT NULL"
  in
  SqliteUtils.result_fold_rows db ~log:"loading captured procs" stmt ~init:() ~f:(fun () stmt ->
      let proc_name =
        Sqlite3.column stmt 0 |> ProcAttributes.SQLite.deserialize |> ProcAttributes.get_proc_name
      in
      let callees : Procname.t list = Sqlite3.column stmt 1 |> Procname.SQLiteList.deserialize in
      f proc_name callees )


type hashconsed_procname_info =
  {name: Procname.t; mutable defined: bool; mutable callees: Procname.t list}

let hashcons_pname pname_info pname =
  match Procname.Hash.find_opt pname_info pname with
  | Some {name} ->
      name
  | None ->
      Procname.Hash.add pname_info pname {name= pname; defined= false; callees= []} ;
      pname


let hashcons_and_update_pname pname_info pname callees =
  let callees = List.map ~f:(hashcons_pname pname_info) callees in
  match Procname.Hash.find_opt pname_info pname with
  | Some info when info.defined ->
      L.die InternalError "SyntacticCallGraph: Tried to define %a twice.@." Procname.pp pname
  | Some info ->
      info.callees <- callees ;
      info.defined <- true
  | None ->
      Procname.Hash.add pname_info pname {name= pname; defined= true; callees}


(* load and hashcons all captured procs and their callees ; return also number of defined procs *)
let pname_info_from_captured_procs () =
  let pname_info = Procname.Hash.create 1009 in
  let n_captured = ref 0 in
  iter_captured_procs_and_callees (fun pname callees ->
      incr n_captured ;
      hashcons_and_update_pname pname_info pname callees ) ;
  (pname_info, !n_captured)


let enqueue q pname = Procname.HashQueue.enqueue_back q pname pname |> ignore

let dequeue q = Procname.HashQueue.dequeue_front q

let queue_from_sources pname_info sources =
  let q = Procname.HashQueue.create () in
  List.iter sources ~f:(fun sf ->
      SourceFiles.proc_names_of_source sf
      |> List.iter ~f:(fun pname -> hashcons_pname pname_info pname |> enqueue q) ) ;
  q


let rec bfs pname_info g q =
  match dequeue q with
  | Some pname ->
      ( match Procname.Hash.find_opt pname_info pname with
      | Some {defined= true; callees} ->
          CallGraph.create_node g pname callees ;
          List.iter callees ~f:(fun pname ->
              if not (CallGraph.mem_procname g pname) then enqueue q pname )
      | _ ->
          () ) ;
      bfs pname_info g q
  | _ ->
      ()


let build_from_sources sources =
  let g = CallGraph.create CallGraph.default_initial_capacity in
  let time0 = Mtime_clock.counter () in
  L.progress "Building call graph...@\n%!" ;
  let pname_info, n_captured = pname_info_from_captured_procs () in
  let q = queue_from_sources pname_info sources in
  bfs pname_info g q ;
  L.progress
    "Built call graph in %a, from %d total procs, %d reachable defined procs and takes %d bytes@."
    Mtime.Span.pp (Mtime_clock.count time0) n_captured (CallGraph.n_procs g)
    (Obj.(reachable_words (repr g)) * (Sys.word_size_in_bits / 8)) ;
  g


let to_dotty g = CallGraph.to_dotty g SyntacticDependencyGraphDot

let bottom_up sources : (TaskSchedulerTypes.target, 'a) ProcessPool.TaskGenerator.t =
  let syntactic_call_graph = build_from_sources sources in
  if Config.debug_level_analysis > 0 then to_dotty syntactic_call_graph ;
  CallGraphScheduler.bottom_up syntactic_call_graph


let make sources = ProcessPool.TaskGenerator.chain (bottom_up sources) (FileScheduler.make sources)
