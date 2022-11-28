(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val remove_socket_file : unit -> unit
(** deletes (unlinks) the file corresponding to the socket in the results dir if it exists; useful
    to clean up stale state at the start of an infer execution *)

val use_daemon : bool Lazy.t
(** indicates that there should be a daemon running *)

val add_source_file :
     source_file:Sqlite3.Data.t
  -> tenv:Sqlite3.Data.t
  -> integer_type_widths:Sqlite3.Data.t
  -> proc_names:Sqlite3.Data.t
  -> unit

val canonicalize : unit -> unit
(** put the database on disk in deterministic form *)

val delete_all_specs : unit -> unit

val delete_issue_logs : source_file:Sqlite3.Data.t -> unit

val delete_spec : proc_uid:string -> unit

val mark_all_source_files_stale : unit -> unit

val merge_captures : root:string -> infer_deps_file:string -> unit

val merge_report_summaries : infer_outs:string list -> unit

val replace_attributes :
     proc_uid:string
  -> proc_attributes:Sqlite3.Data.t
  -> cfg:Sqlite3.Data.t
  -> callees:Sqlite3.Data.t
  -> analysis:bool
  -> unit

val reset_capture_tables : unit -> unit

val shrink_analysis_db : unit -> unit
(** Delete all analysis summaries (by overwriting with [NULL]) and [VACUUM]ing. *)

val start : unit -> unit

val stop : unit -> unit

val store_issue_log :
  checker:string -> source_file:Sqlite3.Data.t -> issue_log:Sqlite3.Data.t -> unit

val store_spec :
     proc_uid:string
  -> proc_name:Sqlite3.Data.t
  -> payloads:Sqlite3.Data.t list
  -> report_summary:Sqlite3.Data.t
  -> summary_metadata:Sqlite3.Data.t
  -> unit
