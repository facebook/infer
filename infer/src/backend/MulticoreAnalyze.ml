(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module TargetStack = struct
  let mutex = Error_checking_mutex.create ()

  let stack : SourceFile.t Stack.t = Stack.create ()

  let push x = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.push stack x)

  let pop () = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.pop stack)

  let length () = Error_checking_mutex.critical_section mutex ~f:(fun () -> Stack.length stack)
end

let analyze_source_file exe_env source_file =
  DB.Results_dir.init source_file ;
  Ondemand.analyze_file exe_env AnalysisRequest.all source_file ;
  if Config.write_html then Printer.write_all_html_files source_file


let crash_flag = Atomic.make false

let rec analysis_loop exe_env =
  match TargetStack.pop () with
  | Some source_file when not (Atomic.get crash_flag) ->
      analyze_source_file exe_env source_file ;
      analysis_loop exe_env
  | _ ->
      ()


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
  Lazy.force source_files_to_analyze_lazy |> List.iter ~f:TargetStack.push ;
  Database.db_close () ;
  let n_workers = min (Domain.recommended_domain_count () - 1) (TargetStack.length ()) in
  let workers = List.init n_workers ~f:(fun num -> Domain.spawn (fun () -> worker num)) in
  Database.new_database_connections Primary ;
  L.debug Analysis Quiet "Multicore analysis spawned %d domains.@." n_workers ;
  List.iter workers ~f:Domain.join ;
  L.debug Analysis Quiet "Multicore analysis finished.@." ;
  ( [Stats.get ()]
  , [GCStats.get ~since:(PreviousStats pre_analysis_gc_stats)]
  , [MissingDependencies.get ()] )
