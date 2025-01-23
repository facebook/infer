(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module Node = SpecializedProcname
module NodeSet = SpecializedProcname.Set

let read_procs_to_analyze () =
  Option.value_map ~default:NodeSet.empty Config.procs_to_analyze_index ~f:(fun index ->
      In_channel.read_all index |> Parsexp.Single.parse_string_exn |> NodeSet.t_of_sexp )


module TargetStack = struct
  let mutex = Error_checking_mutex.create ()

  let stack : TaskSchedulerTypes.target Stack.t = Stack.create ()

  let push x = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.push stack x)

  let pop () = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.pop stack)

  let length () = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.length stack)
end

let analyze_source_file exe_env source_file =
  DB.Results_dir.init source_file ;
  Ondemand.analyze_file exe_env AnalysisRequest.all source_file ;
  if Config.write_html then Printer.write_all_html_files source_file


let analyze_proc_name exe_env ~specialization proc_name =
  Ondemand.analyze_proc_name_toplevel exe_env AnalysisRequest.all ~specialization proc_name


let analyze_target exe_env target =
  match (target : TaskSchedulerTypes.target) with
  | File source_file ->
      analyze_source_file exe_env source_file
  | Procname {proc_name; specialization} ->
      analyze_proc_name exe_env ~specialization proc_name


let crash_flag = Atomic.make false

let rec analysis_loop exe_env =
  if not @@ Atomic.get crash_flag then
    match TargetStack.pop () with
    | None ->
        ()
    | Some target ->
        analyze_target exe_env target ;
        analysis_loop exe_env


let worker num =
  try
    Printexc.record_backtrace true ;
    Database.new_database_connections Primary ;
    L.debug Analysis Quiet "Multicore: worker %d opened db@." num ;
    let exe_env = Exe_env.mk () in
    analysis_loop exe_env ;
    L.debug Analysis Quiet "Multicore: worker %d stopping@." num
  with exn ->
    Atomic.set crash_flag true ;
    L.die InternalError "Multicore: worker %d crashed@\n%a@\n%s@." num Exn.pp exn
      (Printexc.get_backtrace ())


let run_analysis replay_call_graph source_files_to_analyze_lazy =
  L.debug Analysis Quiet "Multicore analysis starting...@." ;
  if Option.is_some replay_call_graph then
    L.die UserError "Multicore analysis does not support the replay scheduler.@\n" ;
  let pre_analysis_gc_stats = GCStats.get ~since:ProgramStart in
  let source_files = Lazy.force source_files_to_analyze_lazy in
  List.iter source_files ~f:(fun f -> TargetStack.push (File f)) ;
  (* leave procedures on top of files to increase concurrency *)
  List.iter source_files ~f:(fun source ->
      SourceFiles.proc_names_of_source source
      |> List.iter ~f:(fun proc_name ->
             TargetStack.push (Procname {proc_name; specialization= None}) ) ) ;
  read_procs_to_analyze ()
  |> NodeSet.iter (fun {Node.proc_name; specialization} ->
         TargetStack.push (Procname {specialization; proc_name}) ) ;
  DBWriter.terminate () ;
  DBWriter.use_multicore := true ;
  DBWriter.start () ;
  let n_workers = min (Domain.recommended_domain_count () - 2) (TargetStack.length ()) in
  let workers = List.init n_workers ~f:(fun num -> Domain.spawn (fun () -> worker num)) in
  L.debug Analysis Quiet "Multicore analysis spawned %d domains.@." n_workers ;
  List.iter workers ~f:Domain.join ;
  L.debug Analysis Quiet "Multicore analysis finished.@." ;
  ( [Stats.get ()]
  , [GCStats.get ~since:(PreviousStats pre_analysis_gc_stats)]
  , [MissingDependencies.get ()] )
