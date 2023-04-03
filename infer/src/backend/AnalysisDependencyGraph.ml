(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

let build ~changed_files =
  let graph = CallGraph.(create default_initial_capacity) in
  let tenv_deps = SourceFile.Hash.create 0 in
  (* set up a hashtable of procedures in changed files at the current version upfront, to avoid
     querying SQLite repeatedly for [SourceFiles.proc_names_of_source] *)
  let procs_in_changed_files : Procname.Set.t SourceFile.Hash.t =
    let hashtbl = SourceFile.Hash.create (SourceFile.Set.cardinal changed_files) in
    SourceFile.Set.iter
      (fun changed_file ->
        let procs = SourceFiles.proc_names_of_source changed_file |> Procname.Set.of_list in
        SourceFile.Hash.add hashtbl changed_file procs )
      changed_files ;
    hashtbl
  in
  let deleted_procs = ref [] in
  (* First, build a reverse analysis callgraph [graph] and tenv dependency map [tenv_deps]. *)
  Summary.OnDisk.iter_specs ~f:(fun {Summary.proc_name; dependencies} ->
      let Dependencies.{callees; used_tenv_sources} =
        match dependencies with
        | Complete c ->
            c
        | Partial _ ->
            L.die InternalError "deserialized summary with incomplete dependencies"
      in
      let is_deleted_proc =
        match Attributes.load proc_name with
        | None ->
            true
        | Some {ProcAttributes.translation_unit} ->
            SourceFile.Set.mem translation_unit changed_files
            && not
                 (Procname.Set.mem proc_name
                    (SourceFile.Hash.find procs_in_changed_files translation_unit) )
      in
      if is_deleted_proc then deleted_procs := proc_name :: !deleted_procs ;
      List.iter callees ~f:(fun callee ->
          CallGraph.add_edge graph ~pname:callee ~successor_pname:proc_name ) ;
      List.iter used_tenv_sources ~f:(fun src_file ->
          match SourceFile.Hash.find_opt tenv_deps src_file with
          | Some deps ->
              Procname.HashSet.add proc_name deps
          | None ->
              Procname.HashSet.singleton proc_name |> SourceFile.Hash.add tenv_deps src_file ) ) ;
  (* Then, flag in [graph] any procedure with a summary depending (transitively) on either (1) a
     deleted procedure, (2) the tenv of a changed file or (3) the summary of a changed procedure. *)
  List.iter !deleted_procs ~f:(CallGraph.flag_reachable graph) ;
  SourceFile.Set.iter
    (fun sf ->
      SourceFile.Hash.find_opt tenv_deps sf
      |> Option.iter ~f:(fun sf_tenv_deps ->
             Procname.HashSet.iter sf_tenv_deps (CallGraph.flag_reachable graph) ) ;
      SourceFile.Hash.find procs_in_changed_files sf
      |> Procname.Set.iter (fun pname ->
             match Attributes.load pname with
             | None ->
                 CallGraph.flag_reachable graph pname
             | Some attrs ->
                 if attrs.changed then CallGraph.flag_reachable graph pname ) )
    changed_files ;
  graph
