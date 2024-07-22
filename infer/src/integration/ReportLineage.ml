(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let analysis_req = AnalysisRequest.one Lineage

let report_proc_json {Summary.payloads= {lineage}; proc_name} =
  match ILazy.force_option lineage with
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
      let summary = Summary.OnDisk.get ~lazy_payloads:true analysis_req proc_name in
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


let pp_issue_log fmt issue_log =
  JsonReports.JsonIssuePrinter.pp_open fmt () ;
  IssueLog.iter
    ~f:(fun proc_name err_log ->
      Errlog.iter
        (fun err_key err_data ->
          JsonReports.JsonIssuePrinter.pp fmt
            { JsonReports.error_filter= (fun _ _ -> true)
            ; proc_name
            ; proc_location_opt= None
            ; err_key
            ; err_data } )
        err_log )
    issue_log ;
  JsonReports.JsonIssuePrinter.pp_close fmt ()


let report_taint taint_config =
  let issue_log = LineageTaint.report taint_config in
  LineageTaint.export_result ~name:"Traces sample" ~fileparts:["lineage-taint"; "traces.json"]
    pp_issue_log issue_log ;
  ()
