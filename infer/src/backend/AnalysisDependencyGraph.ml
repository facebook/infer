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
      let {Dependencies.summary_loads; other_proc_names; used_tenv_sources} =
        match dependencies with
        | Complete c ->
            c
        | Partial ->
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
      List.iter summary_loads ~f:(fun callee ->
          CallGraph.add_edge graph callee ~successor:proc_name ) ;
      List.iter other_proc_names ~f:(fun callee ->
          CallGraph.add_edge graph callee ~successor:proc_name ) ;
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


let build_for_analysis_replay () =
  let graph = CallGraph.(create default_initial_capacity) in
  let edges_to_ignore = ref Procname.Map.empty in
  Summary.OnDisk.iter_specs ~f:(fun {Summary.proc_name; dependencies} ->
      let {Dependencies.summary_loads; recursion_edges} =
        match dependencies with
        | Complete c ->
            c
        | Partial ->
            L.die InternalError "deserialized summary with incomplete dependencies"
      in
      edges_to_ignore := Procname.Map.add proc_name recursion_edges !edges_to_ignore ;
      CallGraph.create_node graph proc_name summary_loads ) ;
  if Config.debug_level_analysis > 0 then CallGraph.to_dotty graph AnalysisDependencyGraphDot ;
  Ondemand.edges_to_ignore := Some !edges_to_ignore ;
  graph


let invalidate ~changed_files =
  let changed_files =
    match changed_files with
    | Some files ->
        files
    | None ->
        L.die ExternalError
          "incremental invalidation requires specifying changed files via --changed-files-index"
  in
  L.progress "Incremental analysis: invalidating potentially-affected analysis results.@." ;
  let dependency_graph = build ~changed_files in
  let total_nodes = CallGraph.n_procs dependency_graph in
  (* Only bother with incremental invalidation and logging if there are already some analysis
     results stored in the db. *)
  if total_nodes > 0 then (
    if Config.debug_level_analysis > 0 then
      CallGraph.to_dotty dependency_graph AnalysisDependencyInvalidationGraphDot ;
    let invalidated_nodes, invalidated_files =
      CallGraph.fold_flagged dependency_graph
        ~f:(fun node (acc_nodes, acc_files) ->
          let files =
            match Attributes.load node.pname with
            | Some {translation_unit} ->
                SourceFile.Set.add translation_unit acc_files
            | None ->
                acc_files
          in
          Summary.OnDisk.delete node.pname ;
          (acc_nodes + 1, files) )
        (0, SourceFile.Set.empty)
    in
    SourceFile.Set.iter IssueLog.invalidate invalidated_files ;
    let invalidated_files = SourceFile.Set.cardinal invalidated_files in
    L.progress
      "Incremental analysis: Invalidated %d of %d procedure summaries, and file-level analyses for \
       %d distinct file%s.@."
      invalidated_nodes total_nodes invalidated_files
      (if Int.equal invalidated_files 1 then "" else "s") ;
    ScubaLogging.log_count ~label:"incremental_analysis.total_nodes" ~value:total_nodes ;
    ScubaLogging.log_count ~label:"incremental_analysis.invalidated_nodes" ~value:invalidated_nodes ;
    ScubaLogging.log_count ~label:"incremental_analysis.invalidated_files" ~value:invalidated_files
    ) ;
  (* save some memory *)
  ResultsDir.scrub_for_incremental ()
