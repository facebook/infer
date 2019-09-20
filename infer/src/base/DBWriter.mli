(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val use_daemon : bool
(** indicates that there should be a daemon running *)

val replace_attributes :
     pname_str:string
  -> pname:Sqlite3.Data.t
  -> akind:int64
  -> source_file:Sqlite3.Data.t
  -> attributes:Sqlite3.Data.t
  -> proc_desc:Sqlite3.Data.t
  -> callees:Sqlite3.Data.t
  -> unit

val add_source_file :
     source_file:Sqlite3.Data.t
  -> tenv:Sqlite3.Data.t
  -> integer_type_widths:Sqlite3.Data.t
  -> proc_names:Sqlite3.Data.t
  -> unit

val mark_all_source_files_stale : unit -> unit

val merge : infer_deps_file:string -> unit

val canonicalize : unit -> unit
(** put the database on disk in deterministic form *)

val reset_capture_tables : unit -> unit

val start : unit -> unit

val stop : unit -> unit
