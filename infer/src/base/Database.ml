(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type id = CaptureDatabase | AnalysisDatabase

(** cannot use {!ResultsDir.get_path} due to circular dependency so re-implement it *)
let results_dir_get_path entry = ResultsDirEntryName.get_path ~results_dir:Config.results_dir entry

let procedures_schema prefix =
  (* [proc_uid] is meant to only be used with [Procname.to_unique_id]
     [Marshal]ed values must never be used as keys. *)
  Printf.sprintf
    {|
      CREATE TABLE IF NOT EXISTS %sprocedures
        ( proc_uid TEXT PRIMARY KEY NOT NULL
        , proc_attributes BLOB NOT NULL
        , cfg BLOB
        , callees BLOB NOT NULL
        )
    |}
    prefix


let source_files_schema prefix =
  (* [Marshal]ed values must never be used as keys. [source_file] has a custom serialiser *)
  Printf.sprintf
    {|
      CREATE TABLE IF NOT EXISTS %ssource_files
        ( source_file TEXT PRIMARY KEY
        , type_environment BLOB NOT NULL
        , integer_type_widths BLOB
        , procedure_names BLOB NOT NULL
        , freshly_captured INT NOT NULL )
    |}
    prefix


type analysis_table = Specs | BiabductionModelsSpecs

let string_of_analysis_table = function Specs -> "specs" | BiabductionModelsSpecs -> "model_specs"

let specs_schema table prefix =
  (* [proc_uid] is meant to only be used with [Procname.to_unique_id]
     [Marshal]ed values must never be used as keys. *)
  Printf.sprintf
    {|
      CREATE TABLE IF NOT EXISTS %s%s
        ( proc_uid TEXT PRIMARY KEY NOT NULL
        , proc_name BLOB NOT NULL
        , report_summary BLOB NOT NULL
        , summary_metadata BLOB
        , payloads BLOB
        )
    |}
    prefix (string_of_analysis_table table)


let issues_schema prefix =
  Printf.sprintf
    {|
      CREATE TABLE IF NOT EXISTS %sissue_logs
        ( checker STRING NOT NULL
        , source_file TEXT NOT NULL
        , issue_log BLOB NOT NULL
        , PRIMARY KEY (checker, source_file)
        )
    |}
    prefix


let schema_hum =
  String.concat ~sep:";\n"
    [ procedures_schema ""
    ; source_files_schema ""
    ; specs_schema Specs ""
    ; specs_schema BiabductionModelsSpecs ""
    ; issues_schema "" ]


let create_procedures_table ~prefix db =
  SqliteUtils.exec db ~log:"creating procedures table" ~stmt:(procedures_schema prefix)


let create_source_files_table ~prefix db =
  SqliteUtils.exec db ~log:"creating source_files table" ~stmt:(source_files_schema prefix)


let create_specs_tables ~prefix db =
  SqliteUtils.exec db ~log:"creating specs table" ~stmt:(specs_schema Specs prefix) ;
  SqliteUtils.exec db ~log:"creating model specs table"
    ~stmt:(specs_schema BiabductionModelsSpecs prefix) ;
  SqliteUtils.exec db ~log:"creating issue logs table" ~stmt:(issues_schema prefix)


let create_tables ?(prefix = "") db = function
  | CaptureDatabase ->
      create_procedures_table ~prefix db ;
      create_source_files_table ~prefix db
  | AnalysisDatabase ->
      create_procedures_table ~prefix db ;
      create_specs_tables ~prefix db


let load_model_specs db = function
  | AnalysisDatabase
    when Config.is_checker_enabled Biabduction && not Config.biabduction_models_mode -> (
    try
      let time0 = Mtime_clock.counter () in
      SqliteUtils.exec db ~log:"begin transaction" ~stmt:"BEGIN IMMEDIATE TRANSACTION" ;
      Utils.with_file_in Config.biabduction_models_sql
        ~f:(In_channel.iter_lines ~f:(fun stmt -> SqliteUtils.exec db ~log:"load models" ~stmt)) ;
      SqliteUtils.exec db ~log:"commit transaction" ~stmt:"COMMIT" ;
      L.debug Capture Quiet "Loading models took %a@." Mtime.Span.pp (Mtime_clock.count time0)
    with Sys_error _ ->
      L.die ExternalError "Could not load model file %s@." Config.biabduction_models_sql )
  | _ ->
      ()


let get_dbpath = function
  | AnalysisDatabase ->
      ResultsDirEntryName.AnalysisDB
  | CaptureDatabase ->
      ResultsDirEntryName.CaptureDB


let create_db id =
  let temp_db =
    Filename.temp_file ~in_dir:(results_dir_get_path Temporary)
      (match id with CaptureDatabase -> "capture.db" | AnalysisDatabase -> "results.db")
      ".tmp"
  in
  let db = Sqlite3.db_open ~mutex:`FULL temp_db in
  SqliteUtils.exec db ~log:"sqlite page size"
    ~stmt:(Printf.sprintf "PRAGMA page_size=%d" Config.sqlite_page_size) ;
  create_tables db id ;
  (* This should be the default but better be sure, otherwise we cannot access the database concurrently. This has to happen before setting WAL mode. *)
  SqliteUtils.exec db ~log:"locking mode=NORMAL" ~stmt:"PRAGMA locking_mode=NORMAL" ;
  ( match Config.sqlite_vfs with
  | None ->
      (* Write-ahead log is much faster than other journalling modes. *)
      SqliteUtils.exec db ~log:"journal_mode=WAL" ~stmt:"PRAGMA journal_mode=WAL"
  | Some _ ->
      (* Can't use WAL with custom VFS *)
      () ) ;
  (* load biabduction models *)
  load_model_specs db id ;
  SqliteUtils.db_close db ;
  try Sys.rename temp_db (results_dir_get_path (get_dbpath id))
  with Sys_error _ -> (* lost the race, doesn't matter *) ()


let new_analysis_db_callbacks = ref []

let new_capture_db_callbacks = ref []

let get_new_db_callbacks = function
  | AnalysisDatabase ->
      new_analysis_db_callbacks
  | CaptureDatabase ->
      new_capture_db_callbacks


let on_new_database_connection id ~f =
  let callbacks = get_new_db_callbacks id in
  callbacks := f :: !callbacks


let close_analysis_db_callbacks = ref []

let close_capture_db_callbacks = ref []

let get_close_db_callbacks = function
  | AnalysisDatabase ->
      close_analysis_db_callbacks
  | CaptureDatabase ->
      close_capture_db_callbacks


let on_close_database id ~f =
  let callbacks = get_close_db_callbacks id in
  callbacks := f :: !callbacks


type registered_stmt = unit -> Sqlite3.stmt * Sqlite3.db

let register_statement id =
  let k stmt0 =
    let stmt_ref = ref None in
    let new_statement db =
      let stmt =
        try Sqlite3.prepare db stmt0
        with Sqlite3.Error error ->
          L.die InternalError "Could not prepare the following statement:@\n%s@\nReason: %s" stmt0
            error
      in
      on_close_database id ~f:(fun _ -> SqliteUtils.finalize db ~log:"db close callback" stmt) ;
      stmt_ref := Some (stmt, db)
    in
    on_new_database_connection id ~f:new_statement ;
    fun () ->
      match !stmt_ref with
      | None ->
          L.(die InternalError) "database not initialized"
      | Some (stmt, db) ->
          Sqlite3.reset stmt |> SqliteUtils.check_result_code db ~log:"reset prepared statement" ;
          Sqlite3.clear_bindings stmt
          |> SqliteUtils.check_result_code db ~log:"clear bindings of prepared statement" ;
          (stmt, db)
  in
  fun stmt_fmt -> Printf.ksprintf k stmt_fmt


let with_registered_statement get_stmt ~f =
  PerfEvent.(log (fun logger -> log_begin_event logger ~name:"sql op" ())) ;
  let stmt, db = get_stmt () in
  let result = f db stmt in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  result


let do_db_close db close_callbacks =
  List.iter ~f:(fun callback -> callback db) !close_callbacks ;
  close_callbacks := [] ;
  SqliteUtils.db_close db


module UnsafeDatabaseRef : sig
  val get_database : id -> Sqlite3.db

  val db_close : unit -> unit

  val new_database_connection : unit -> unit
end = struct
  let capture_database : Sqlite3.db option ref = ref None

  let results_database : Sqlite3.db option ref = ref None

  let get_db = function CaptureDatabase -> capture_database | AnalysisDatabase -> results_database

  let get_database id =
    match !(get_db id) with
    | Some db ->
        db
    | None ->
        L.die InternalError
          "Could not open the database. Did you forget to call `ResultsDir.assert_results_dir \
           \"\"` or `ResultsDir.create_results_dir ()`?"


  let db_close () =
    List.iter
      ~f:(fun id ->
        let db = get_db id in
        let callbacks = get_close_db_callbacks id in
        Option.iter !db ~f:(fun db -> do_db_close db callbacks) ;
        db := None )
      [CaptureDatabase; AnalysisDatabase]


  let do_new_database id =
    let db =
      Sqlite3.db_open ~mode:`NO_CREATE ~cache:`PRIVATE ~mutex:`FULL ?vfs:Config.sqlite_vfs
        (results_dir_get_path (get_dbpath id))
    in
    Sqlite3.busy_timeout db Config.sqlite_lock_timeout ;
    SqliteUtils.exec db ~log:"synchronous=OFF" ~stmt:"PRAGMA synchronous=OFF" ;
    SqliteUtils.exec db ~log:"sqlite cache size"
      ~stmt:(Printf.sprintf "PRAGMA cache_size=%i" Config.sqlite_cache_size) ;
    let db_ref = get_db id in
    db_ref := Some db ;
    List.iter ~f:(fun callback -> callback db) !(get_new_db_callbacks id)


  let new_database_connection () =
    (* we always want at most one connection alive throughout the lifetime of the module *)
    db_close () ;
    List.iter ~f:do_new_database [CaptureDatabase; AnalysisDatabase]
end

include UnsafeDatabaseRef

let () = Epilogues.register_late ~f:db_close ~description:"closing database connection"
