(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let use_multicore = ref false

let perform cmd =
  if !use_multicore then DBWriterDomain.perform cmd
  else if DBWriterProcess.use_daemon () then DBWriterProcess.perform cmd
  else DBWriterCommand.perform cmd


let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  perform (AddSourceFile {source_file; tenv; integer_type_widths; proc_names})


let canonicalize () = perform Checkpoint

let delete_all_specs () = perform DeleteAllSpecs

let delete_attributes ~proc_uid = perform (DeleteAttributes {proc_uid})

let delete_issue_logs ~source_file = perform (DeleteIssueLogs {source_file})

let delete_specs ~proc_uids = perform (DeleteSpecs {proc_uids})

let mark_all_source_files_stale () = perform MarkAllSourceFilesStale

let merge_captures ~root ~infer_deps_file = perform (MergeCaptures {root; infer_deps_file})

let merge_summaries ~infer_outs = perform (MergeSummaries {infer_outs})

let replace_attributes ~proc_uid ~proc_attributes ~cfg ~callees ~analysis =
  perform (ReplaceAttributes {proc_uid; proc_attributes; cfg; callees; analysis})


let shrink_analysis_db () = perform ShrinkAnalysisDB

let start () = perform Start

let store_issue_log ~checker ~source_file ~issue_log =
  perform (StoreIssueLog {checker; source_file; issue_log})


let terminate () = perform Terminate

let store_spec ?(transaction = not @@ DBWriterProcess.use_daemon ()) analysis_req ~proc_uid
    ~proc_name ~merge_pulse_payload ~merge_report_summary ~merge_summary_metadata =
  let counter = ExecutionDuration.counter () in
  let result =
    perform
      (StoreSpec
         { transaction
         ; analysis_req
         ; proc_uid
         ; proc_name
         ; merge_pulse_payload
         ; merge_report_summary
         ; merge_summary_metadata } )
  in
  Stats.incr_spec_store_times counter ;
  result


let update_report_summary ?(transaction = not @@ DBWriterProcess.use_daemon ()) ~proc_uid
    ~merge_report_summary () =
  perform (UpdateReportSummary {transaction; proc_uid; merge_report_summary})
