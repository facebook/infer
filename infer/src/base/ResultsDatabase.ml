(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let database_filename = "results.db"

let database_fullpath = Config.results_dir ^/ database_filename

let procedures_schema =
  {|CREATE TABLE IF NOT EXISTS procedures
  ( proc_name TEXT PRIMARY KEY
  , proc_name_hum TEXT
  , attr_kind INTEGER NOT NULL
  , source_file TEXT NOT NULL
  , proc_attributes BLOB NOT NULL
  , cfg BLOB
  )|}


let source_files_schema =
  {|CREATE TABLE IF NOT EXISTS source_files
  ( source_file TEXT PRIMARY KEY
  , type_environment BLOB NOT NULL
  , integer_type_widths BLOB
  , procedure_names BLOB NOT NULL
  , freshly_captured INT NOT NULL )|}


let schema_hum = Printf.sprintf "%s;\n%s" procedures_schema source_files_schema

let create_procedures_table db =
  (* it would be nice to use "WITHOUT ROWID" here but ancient versions of sqlite do not support
     it *)
  SqliteUtils.exec db ~log:"creating procedures table" ~stmt:procedures_schema


let create_source_files_table db =
  SqliteUtils.exec db ~log:"creating source_files table" ~stmt:source_files_schema


let create_db () =
  let temp_db = Filename.temp_file ~in_dir:Config.results_dir database_filename ".tmp" in
  let db = Sqlite3.db_open ~mutex:`FULL temp_db in
  create_procedures_table db ;
  create_source_files_table db ;
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

type registered_stmt = unit -> Sqlite3.stmt * Sqlite3.db

let register_statement =
  let k stmt0 =
    let stmt_ref = ref None in
    let new_statement db =
      let stmt =
        try Sqlite3.prepare db stmt0 with Sqlite3.Error error ->
          L.die InternalError "Could not prepare the following statement:@\n%s@\nReason: %s" stmt0
            error
      in
      on_close_database ~f:(fun _ -> SqliteUtils.finalize db ~log:"db close callback" stmt) ;
      stmt_ref := Some (stmt, db)
    in
    on_new_database_connection ~f:new_statement ;
    fun () ->
      match !stmt_ref with
      | None ->
          L.(die InternalError) "database not initialized"
      | Some (stmt, db) ->
          Sqlite3.clear_bindings stmt
          |> SqliteUtils.check_result_code db ~log:"clear bindings of prepared statement" ;
          (stmt, db)
  in
  fun stmt_fmt -> Printf.ksprintf k stmt_fmt


let with_registered_statement get_stmt ~f =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"sql op" ())) ;
  let stmt, db = get_stmt () in
  let result = f db stmt in
  Sqlite3.reset stmt |> SqliteUtils.check_result_code db ~log:"reset prepared statement" ;
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  result


let do_db_close db =
  List.iter ~f:(fun callback -> callback db) !close_db_callbacks ;
  close_db_callbacks := [] ;
  SqliteUtils.db_close db


module UnsafeDatabaseRef : sig
  val get_database : unit -> Sqlite3.db

  val db_close : unit -> unit

  val new_database_connection : unit -> unit
end = struct
  let database : Sqlite3.db option ref = ref None

  let get_database () =
    match !database with
    | Some db ->
        db
    | None ->
        L.die InternalError
          "Could not open the database. Did you forget to call `ResultsDir.assert_results_dir \
           \"\"` or `ResultsDir.create_results_dir ()`?"


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
    Sqlite3.busy_timeout db Config.sqlite_lock_timeout ;
    SqliteUtils.exec db ~log:"synchronous=OFF" ~stmt:"PRAGMA synchronous=OFF" ;
    database := Some db ;
    List.iter ~f:(fun callback -> callback db) !new_db_callbacks
end

include UnsafeDatabaseRef

let reset_capture_tables () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"drop procedures table" ~stmt:"DROP TABLE procedures" ;
  create_procedures_table db ;
  SqliteUtils.exec db ~log:"drop source_files table" ~stmt:"DROP TABLE source_files" ;
  create_source_files_table db


let db_canonicalize () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"running VACUUM" ~stmt:"VACUUM"


let () = Epilogues.register_late ~f:db_close ~description:"closing database connection"
