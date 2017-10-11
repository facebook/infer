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

let results_dir_dir_markers =
  List.map ~f:(Filename.concat Config.results_dir) [Config.captured_dir_name; Config.specs_dir_name]

let is_results_dir ~check_correct_version () =
  let not_found = ref "" in
  let has_all_markers =
    List.for_all results_dir_dir_markers ~f:(fun d ->
        Sys.is_directory d = `Yes
        ||
        (not_found := d ^ "/" ;
        false) )
    && ( not check_correct_version || Sys.is_file database_fullpath = `Yes
       ||
       (not_found := database_fullpath ;
       false) )
  in
  Result.ok_if_true has_all_markers ~error:(Printf.sprintf "'%s' not found" !not_found)

let remove_results_dir () =
  (* Look if file exists, it may not be a directory but that will be caught by the call to [is_results_dir]. If it's an empty directory, leave it alone. This allows users to create a temporary directory for the infer results without infer removing it to recreate it, which could be racy. *)
  if Sys.file_exists Config.results_dir = `Yes && not (Utils.directory_is_empty Config.results_dir)
  then (
    if not Config.force_delete_results_dir then
      Result.iter_error (is_results_dir ~check_correct_version:false ()) ~f:(fun err ->
          L.(die UserError)
            "ERROR: '%s' exists but does not seem to be an infer results directory: %s@\nERROR: Please delete '%s' and try again@."
            Config.results_dir err Config.results_dir ) ;
    Utils.rmtree Config.results_dir )

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
  (* Write-ahead log is much faster than other journalling modes. *)
  SqliteUtils.exec db ~log:"journal_mode=WAL" ~stmt:"PRAGMA journal_mode=WAL" ;
  SqliteUtils.db_close db ;
  try Sys.rename temp_db database_fullpath
  with Sys_error _ -> (* lost the race, doesn't matter *) ()

let new_db_callbacks = ref []

let on_new_database_connection ~f = new_db_callbacks := f :: !new_db_callbacks

let close_db_callbacks = ref []

let on_close_database ~f = close_db_callbacks := f :: !close_db_callbacks

let do_db_close db =
  List.iter ~f:(fun callback -> callback db) !close_db_callbacks ;
  close_db_callbacks := [] ;
  SqliteUtils.db_close db

let db_close () =
  Option.iter !database ~f:do_db_close ;
  database := None

let new_database_connection () =
  db_close () ;
  let db = Sqlite3.db_open ~mode:`NO_CREATE ~cache:`PRIVATE ~mutex:`FULL database_fullpath in
  Sqlite3.busy_timeout db 10_000 ;
  (* Higher level of "synchronous" are only useful to guarantee that the db will not be corrupted if the machine crashes for some reason before the data has been actually written to disk. We do not need this kind of guarantee for infer results as one can always rerun infer if interrupted. *)
  SqliteUtils.exec db ~log:"synchronous=OFF" ~stmt:"PRAGMA synchronous=OFF" ;
  database := Some db ;
  List.iter ~f:(fun callback -> callback db) !new_db_callbacks

let () = Epilogues.register "closing results database" ~f:db_close

let create_results_dir () =
  Unix.mkdir_p Config.results_dir ;
  L.setup_log_file () ;
  if Sys.file_exists database_fullpath <> `Yes then create_db () ;
  new_database_connection () ;
  List.iter ~f:Unix.mkdir_p results_dir_dir_markers

let assert_results_dir advice =
  Result.iter_error (is_results_dir ~check_correct_version:true ()) ~f:(fun err ->
      L.(die UserError)
        "ERROR: No results directory at '%s': %s@\nERROR: %s@." Config.results_dir err advice ) ;
  L.setup_log_file () ;
  new_database_connection ()

let get_database () = Option.value_exn !database

let reset_attributes_table () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"drop attributes table" ~stmt:"DROP TABLE attributes" ;
  create_attributes_table db

let delete_capture_and_analysis_data () =
  reset_attributes_table () ;
  let dirs_to_delete =
    List.map ~f:(Filename.concat Config.results_dir) Config.([captured_dir_name; specs_dir_name])
  in
  List.iter ~f:Utils.rmtree dirs_to_delete ; List.iter ~f:Unix.mkdir_p dirs_to_delete ; ()

let db_canonicalize () =
  let db = get_database () in
  SqliteUtils.exec db ~log:"running VACUUM" ~stmt:"VACUUM"

let register_statement stmt_fmt =
  let k stmt0 =
    let stmt_ref = ref None in
    let new_statement db =
      let stmt =
        try Sqlite3.prepare db stmt0
        with Sqlite3.Error error ->
          L.die InternalError "Could not prepare the following statement:@\n%s@\nReason: %s" stmt0
            error
      in
      on_close_database ~f:(fun _ -> SqliteUtils.finalize ~log:"db close callback" stmt) ;
      stmt_ref := Some stmt
    in
    on_new_database_connection ~f:new_statement ;
    fun () ->
      match !stmt_ref with
      | None
       -> L.(die InternalError) "database not initialized"
      | Some stmt
       -> Sqlite3.reset stmt |> SqliteUtils.check_sqlite_error ~log:"reset prepared statement" ;
          Sqlite3.clear_bindings stmt
          |> SqliteUtils.check_sqlite_error ~log:"clear bindings of prepared statement" ;
          stmt
  in
  Printf.ksprintf k stmt_fmt
