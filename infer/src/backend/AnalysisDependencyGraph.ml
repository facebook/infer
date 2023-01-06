(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let build ~changed_files =
  let graph = CallGraph.(create default_initial_capacity) in
  let tenv_deps = SourceFile.Hash.create 0 in
  (* First, build a reverse analysis callgraph [graph] and tenv dependency map [tenv_deps]. *)
  Summary.OnDisk.iter_specs ~f:(fun summary ->
      let summary_pname = Summary.get_proc_name summary in
      Procname.Set.iter
        (fun callee -> CallGraph.add_edge graph ~pname:callee ~successor_pname:summary_pname)
        summary.callee_pnames ;
      SourceFile.Set.iter
        (fun src_file ->
          match SourceFile.Hash.find_opt tenv_deps src_file with
          | Some pnames ->
              SourceFile.Hash.replace tenv_deps src_file (Procname.Set.add summary_pname pnames)
          | None ->
              SourceFile.Hash.add tenv_deps src_file (Procname.Set.singleton summary_pname) )
        summary.used_tenv_sources ) ;
  (* Then, flag in [graph] any procedure with a summary depending (transitively) on either
     (1) the tenv of a changed file or (2) the summary of a changed procedure. *)
  SourceFile.Set.iter
    (fun sf ->
      SourceFile.Hash.find_opt tenv_deps sf
      |> Option.iter ~f:(Procname.Set.iter (CallGraph.flag_reachable graph)) ;
      SourceFiles.proc_names_of_source sf
      |> List.iter ~f:(fun pname ->
             match Attributes.load pname with
             | None ->
                 CallGraph.flag_reachable graph pname
             | Some attrs ->
                 if attrs.changed then CallGraph.flag_reachable graph pname ) )
    changed_files ;
  graph
