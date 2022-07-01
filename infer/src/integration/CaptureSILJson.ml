(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging


let invalidate_and_return_changed_procedures changed_files =
  let procname_list = ref [] in
  let changed_files =
    match changed_files with
    | Some cf ->
        cf
    | None ->
        L.die InternalError "Incremental analysis enabled without specifying changed files"
  in
  L.progress "Incremental analysis: invalidating procedures that have been changed@." ;
  let reverse_callgraph = CallGraph.create CallGraph.default_initial_capacity in
  ReverseAnalysisCallGraph.build reverse_callgraph ;
  let total_nodes = CallGraph.n_procs reverse_callgraph in
  SourceFile.Set.iter
    (fun sf ->
      SourceFiles.proc_names_of_source sf
      |> List.iter ~f:(CallGraph.flag_reachable reverse_callgraph) )
    changed_files ;
  if Config.debug_level_analysis > 0 then
    CallGraph.to_dotty reverse_callgraph "reverse_analysis_callgraph.dot" ;
  let invalidated_nodes =
    CallGraph.fold_flagged reverse_callgraph
      ~f:(fun node acc ->
        procname_list := !procname_list @ [node.pname] ;
        Ondemand.LocalCache.remove node.pname ;
        Summary.OnDisk.delete node.pname ;
        acc + 1 )
      0
  in
  L.progress
    "Incremental analysis: %d nodes in reverse analysis call graph, %d of which were invalidated \
      @."
    total_nodes invalidated_nodes ;
  ScubaLogging.log_count ~label:"incremental_analysis.total_nodes" ~value:total_nodes ;
  ScubaLogging.log_count ~label:"incremental_analysis.invalidated_nodes" ~value:invalidated_nodes ;
  (* save some memory *)
  CallGraph.reset reverse_callgraph ;
  ResultsDir.scrub_for_incremental () ;
  !procname_list

 
let capture ~changed_files ~json_cfg ~json_tenv =
  InferAnalyze.register_active_checkers () ;
  let reanalyzed_procname_list = ref [] in
  if not Config.continue_analysis then
    if Config.reanalyze then (
      L.progress "Invalidating procedures to be reanalyzed@." ;
      Summary.OnDisk.reset_all ~filter:(Lazy.force Filtering.procedures_filter) () ;
      L.progress "Done@." )
    else if Option.is_some changed_files then (
      reanalyzed_procname_list := invalidate_and_return_changed_procedures changed_files )
    else if not Config.incremental_analysis then 
      DBWriter.delete_all_specs () ; 
  Printexc.record_backtrace true ;
  let tenv = InferAnalyzeJson.parse_tenv (Yojson.Safe.from_file json_tenv) in
  let cfg = InferAnalyzeJson.parse_cfg (Yojson.Safe.from_file json_cfg) in
  let source_file = SourceFile.create ~warn_on_error:false "./Program.cs" in
  Tenv.store_global tenv ;
  Language.curr_language := Language.CIL ;
  SourceFiles.add source_file cfg Tenv.Global None ;
  (* L.progress "%a@." Cfg.pp_proc_signatures cfg ; *)
  let exe_env = Exe_env.mk () in
  Ondemand.analyze_file exe_env source_file (Option.some !reanalyzed_procname_list) ;
  if Config.write_html then Printer.write_all_html_files source_file ;
  ()