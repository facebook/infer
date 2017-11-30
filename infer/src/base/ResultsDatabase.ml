(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant
module L = Logging

let database : Sqlite3.db option ref = ref None

let database_filename = "results.db"

let database_fullpath = Config.results_dir ^/ database_filename

let create_attributes_table db =
  (* it would be nice to use "WITHOUT ROWID" here but ancient versions of sqlite do not support
     it *)
  SqliteUtils.exec db ~log:"initializing results DB"
    ~stmt:
      {|
CREATE TABLE IF NOT EXISTS attributes
  ( proc_name TEXT PRIMARY KEY
  , attr_kind INTEGER NOT NULL
  , source_file TEXT NOT NULL
  , proc_attributes BLOB NOT NULL )|}


let create_db () =
  let temp_db = Filename.temp_file ~in_dir:Config.results_dir database_filename ".tmp" in
  let db = Sqlite3.db_open ~mutex:`FULL temp_db in
  create_attributes_table db ;
  (* This should be the default but better be sure, otherwise we cannot access the database concurrently. This has to happen before setting WAL mode. *)
  SqliteUtils.exec db ~log:"locking mode=NORMAL" ~stmt:"PRAGMA locking_mode=NORMAL" ;
  ( match Config.sqlite_vfs with
  | None ->
      (* Write-ahead log is much faster than other journalling modes. *)
      SqliteUtils.exec db ~log:"journal_mode=WAL" ~stmt:"PRAGMA journal_mode=WAL"
  | Some _ ->
      (* Can't use WAL with custom VFS *)
      () ) ;
  SqliteUtils.db_close db ;
  try Sys.rename temp_db database_fullpath with Sys_error _ ->
    (* lost the race, doesn't matter *) ()


let new_db_callbacks = ref []

let on_new_database_connection ~f = new_db_callbacks := f :: !new_db_callbacks

let close_db_callbacks = ref []

let on_close_database ~f = close_db_callbacks := f :: !close_db_callbacks

let get_database () = Option.value_exn !database

let reset_attributes_table () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"drop attributes table" ~stmt:"DROP TABLE attributes" ;
  create_attributes_table db


let db_canonicalize () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"running VACUUM" ~stmt:"VACUUM"


let register_statement stmt_fmt =
  let k stmt0 =
    let stmt_ref = ref None in
    let new_statement db =
      let stmt =
        try Sqlite3.prepare db stmt0 with Sqlite3.Error error ->
          L.die InternalError "Could not prepare the following statement:@\n%s@\nReason: %s" stmt0
            error
      in
      on_close_database ~f:(fun _ -> SqliteUtils.finalize ~log:"db close callback" stmt) ;
      stmt_ref := Some stmt
    in
    on_new_database_connection ~f:new_statement ;
    fun () ->
      match !stmt_ref with
      | None ->
          L.(die InternalError) "database not initialized"
      | Some stmt ->
          Sqlite3.reset stmt |> SqliteUtils.check_sqlite_error ~log:"reset prepared statement" ;
          Sqlite3.clear_bindings stmt
          |> SqliteUtils.check_sqlite_error ~log:"clear bindings of prepared statement" ;
          stmt
  in
  Printf.ksprintf k stmt_fmt


let do_db_close db =
  List.iter ~f:(fun callback -> callback db) !close_db_callbacks ;
  close_db_callbacks := [] ;
  SqliteUtils.db_close db


let db_close () =
  Option.iter !database ~f:do_db_close ;
  database := None


let new_database_connection () =
  (* we always want at most one connection alive throughout the lifetime of the module *)
  db_close () ;
  let db =
    Sqlite3.db_open ~mode:`NO_CREATE ~cache:`PRIVATE ~mutex:`FULL ?vfs:Config.sqlite_vfs
      database_fullpath
  in
  Sqlite3.busy_timeout db 10_000 ;
  SqliteUtils.exec db ~log:"synchronous=OFF" ~stmt:"PRAGMA synchronous=OFF" ;
  database := Some db ;
  List.iter ~f:(fun callback -> callback db) !new_db_callbacks


let () = Config.register_late_epilogue db_close
