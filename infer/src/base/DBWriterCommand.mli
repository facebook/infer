(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val store_sql_time : ExecutionDuration.t ref

type t =
  | AddSourceFile of
      { source_file: Sqlite3.Data.t
      ; tenv: Sqlite3.Data.t
      ; integer_type_widths: Sqlite3.Data.t
      ; proc_names: Sqlite3.Data.t }
  | Checkpoint
  | DeleteAllSpecs
  | DeleteAttributes of {proc_uid: string}
  | DeleteIssueLogs of {source_file: Sqlite3.Data.t}
  | DeleteSpecs of {proc_uids: string list}
  | MarkAllSourceFilesStale
  | MergeCaptures of {root: string; infer_deps_file: string}
  | MergeSummaries of {infer_outs: string list}
  | ReplaceAttributes of
      { proc_uid: string
      ; proc_attributes: Sqlite3.Data.t
      ; cfg: Sqlite3.Data.t
      ; callees: Sqlite3.Data.t
      ; analysis: bool }
  | ShrinkAnalysisDB
  | Start
  | StoreIssueLog of {checker: string; source_file: Sqlite3.Data.t; issue_log: Sqlite3.Data.t}
  | StoreSpec of
      { transaction: bool
      ; analysis_req: AnalysisRequest.t
      ; proc_uid: string
      ; proc_name: Sqlite3.Data.t
      ; merge_pulse_payload: old_pulse_payload:Sqlite3.Data.t option -> Sqlite3.Data.t list
      ; merge_report_summary: old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t
      ; merge_summary_metadata: old_summary_metadata:Sqlite3.Data.t option -> Sqlite3.Data.t }
  | Terminate
  | UpdateReportSummary of
      { transaction: bool
      ; proc_uid: string
      ; merge_report_summary: old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t }

val to_string : t -> string [@@warning "-unused-value-declaration"]

val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]

val perform : t -> unit
