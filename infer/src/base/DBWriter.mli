(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val override_use_daemon : bool -> unit
(** override the default of whether the process should use DB daemon [true] or not [false] *)

val add_source_file :
     source_file:Sqlite3.Data.t
  -> tenv:Sqlite3.Data.t
  -> integer_type_widths:Sqlite3.Data.t
  -> proc_names:Sqlite3.Data.t
  -> unit

val canonicalize : unit -> unit
(** put the **capture** database on disk in deterministic form *)

val delete_all_specs : unit -> unit

val delete_attributes : proc_uid:string -> unit

val delete_issue_logs : source_file:Sqlite3.Data.t -> unit

val delete_specs : proc_uids:string list -> unit

val mark_all_source_files_stale : unit -> unit

val merge_captures : root:string -> infer_deps_file:string -> unit

val merge_summaries : infer_outs:string list -> unit

val replace_attributes :
     proc_uid:string
  -> proc_attributes:Sqlite3.Data.t
  -> cfg:Sqlite3.Data.t
  -> callees:Sqlite3.Data.t
  -> analysis:bool
  -> unit

val shrink_analysis_db : unit -> unit
(** Delete all analysis summaries (by overwriting with [NULL]) and [VACUUM]ing. *)

val start : unit -> unit

val store_issue_log :
  checker:string -> source_file:Sqlite3.Data.t -> issue_log:Sqlite3.Data.t -> unit

val store_spec :
     AnalysisRequest.t
  -> proc_uid:string
  -> proc_name:Sqlite3.Data.t
  -> merge_pulse_payload:(old_pulse_payload:Sqlite3.Data.t option -> Sqlite3.Data.t list)
  -> merge_report_summary:(old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t)
  -> merge_summary_metadata:(old_summary_metadata:Sqlite3.Data.t option -> Sqlite3.Data.t)
  -> unit

val update_report_summary :
     proc_uid:string
  -> merge_report_summary:(old_report_summary:Sqlite3.Data.t option -> Sqlite3.Data.t)
  -> unit
