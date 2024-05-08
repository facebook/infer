(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let report_proc_json {Summary.payloads= {lineage}; proc_name} =
  match Lazy.force lineage with
  | None ->
      L.debug Report Verbose "No summary for %a@\n" Procname.pp proc_name
  | Some lineage_summary ->
      Lineage.Out.report_summary (Procdesc.load_exn proc_name) lineage_summary


let worker source_file =
  let t0 = Mtime_clock.now () in
  let status = Format.asprintf "%a" SourceFile.pp source_file in
  !ProcessPoolState.update_status t0 status ;
  let proc_names = SourceFiles.proc_names_of_source source_file in
  List.iter
    ~f:(fun proc_name ->
      let summary = Summary.OnDisk.get ~lazy_payloads:true proc_name in
      Option.iter summary ~f:report_proc_json )
    proc_names ;
  None


let report_json () =
  let tasks () =
    ProcessPool.TaskGenerator.of_list (SourceFiles.get_all ~filter:(fun _ -> true) ())
  in
  Tasks.Runner.create ~jobs:Config.jobs ~child_prologue:ignore ~f:worker ~child_epilogue:ignore
    tasks
  |> Tasks.Runner.run |> ignore


let report_taint ~lineage_source ~lineage_sink = LineageTaint.report ~lineage_source ~lineage_sink
