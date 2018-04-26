(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

let store_statement =
  ResultsDatabase.register_statement
    {|
  INSERT OR REPLACE INTO source_files
  VALUES (:source, :cfgs, :tenv, :proc_names, :freshly_captured) |}


let select_existing_statement =
  ResultsDatabase.register_statement
    "SELECT cfgs, type_environment FROM source_files WHERE source_file = :source AND \
     freshly_captured = 1"


let get_existing_data source_file =
  ResultsDatabase.with_registered_statement select_existing_statement ~f:(fun db stmt ->
      SourceFile.SQLite.serialize source_file |> Sqlite3.bind stmt 1
      (* :source *)
      |> SqliteUtils.check_sqlite_error db ~log:"get_existing_data bind source file" ;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          (* the operation returned a result, get it *)
          let cfgs = Sqlite3.column stmt 0 |> Cfg.SQLite.deserialize
          and tenv = Sqlite3.column stmt 1 |> Tenv.SQLite.deserialize in
          (match Sqlite3.step stmt with DONE -> () | _ -> assert false) ;
          Some (cfgs, tenv)
      | DONE ->
          None
      | err ->
          L.die InternalError "Looking for pre-existing cfgs: %s (%s)" (Sqlite3.Rc.to_string err)
            (Sqlite3.errmsg db) )


let add source_file cfg tenv =
  Cfg.inline_java_synthetic_methods cfg ;
  let cfg, tenv =
    (* The same source file may get captured several times in a single capture event, for instance
       because it is compiled with different options, or from different symbolic links. This may
       generate different procedures in each phase, so make an attempt to merge them into the same
       CFG. *)
    match get_existing_data source_file with
    | Some (old_cfg, old_tenv) ->
        L.debug Capture Quiet "Merging capture data for already-captured '%a'@\n" SourceFile.pp
          source_file ;
        (Cfg.merge ~dst:old_cfg ~src:cfg, Tenv.merge ~dst:old_tenv ~src:tenv)
    | None ->
        (cfg, tenv)
  in
  (* NOTE: it's important to write attribute files to disk before writing cfgs to disk.
     OndemandCapture module relies on it - it uses existance of the cfg as a barrier to make
     sure that all attributes were written to disk (but not necessarily flushed) *)
  Cfg.save_attributes source_file cfg ;
  ResultsDatabase.with_registered_statement store_statement ~f:(fun db store_stmt ->
      SourceFile.SQLite.serialize source_file |> Sqlite3.bind store_stmt 1
      (* :source *)
      |> SqliteUtils.check_sqlite_error db ~log:"store bind source file" ;
      Cfg.SQLite.serialize cfg |> Sqlite3.bind store_stmt 2
      (* :cfg *)
      |> SqliteUtils.check_sqlite_error db ~log:"store bind cfg" ;
      Tenv.SQLite.serialize tenv |> Sqlite3.bind store_stmt 3
      (* :tenv *)
      |> SqliteUtils.check_sqlite_error db ~log:"store bind type environment" ;
      Cfg.get_all_proc_names cfg |> Typ.Procname.SQLiteList.serialize |> Sqlite3.bind store_stmt 4
      (* :proc_names *)
      |> SqliteUtils.check_sqlite_error db ~log:"store bind proc names" ;
      Sqlite3.bind store_stmt 5 (Sqlite3.Data.INT Int64.one)
      (* :freshly_captured *)
      |> SqliteUtils.check_sqlite_error db ~log:"store freshness" ;
      SqliteUtils.sqlite_unit_step ~finalize:false ~log:"Cfg.store" db store_stmt )


let get_all () =
  let db = ResultsDatabase.get_database () in
  Sqlite3.prepare db "SELECT source_file FROM source_files"
  |> SqliteUtils.sqlite_result_rev_list_step db ~log:"getting all source files"
  |> List.filter_map ~f:(Option.map ~f:SourceFile.SQLite.deserialize)


let load_proc_names_statement =
  ResultsDatabase.register_statement
    "SELECT procedure_names FROM source_files WHERE source_file = :k"


let proc_names_of_source source =
  ResultsDatabase.with_registered_statement load_proc_names_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_sqlite_error db ~log:"load bind source file" ;
      SqliteUtils.sqlite_result_step ~finalize:false db ~log:"SourceFiles.proc_names_of_source"
        load_stmt
      |> Option.value_map ~default:[] ~f:Typ.Procname.SQLiteList.deserialize )


let exists_source_statement =
  ResultsDatabase.register_statement "SELECT 1 FROM source_files WHERE source_file = :k"


let is_captured source =
  ResultsDatabase.with_registered_statement exists_source_statement ~f:(fun db exists_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind exists_stmt 1
      (* :k *)
      |> SqliteUtils.check_sqlite_error db ~log:"load captured source file" ;
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"SourceFiles.is_captured" db exists_stmt
      |> Option.is_some )


let is_non_empty_statement =
  ResultsDatabase.register_statement "SELECT 1 FROM source_files LIMIT 1"


let is_empty () =
  ResultsDatabase.with_registered_statement is_non_empty_statement ~f:(fun db stmt ->
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"SourceFiles.is_empty" db stmt
      |> Option.is_none )


let is_freshly_captured_statement =
  ResultsDatabase.register_statement
    "SELECT freshly_captured FROM source_files WHERE source_file = :k"


let is_freshly_captured source =
  ResultsDatabase.with_registered_statement is_freshly_captured_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_sqlite_error db ~log:"load bind source file" ;
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"SourceFiles.is_freshly_captured" db
        load_stmt
      |> Option.value_map ~default:false ~f:(function [@warning "-8"] Sqlite3.Data.INT p ->
             Int64.equal p Int64.one ) )


let mark_all_stale_statement =
  ResultsDatabase.register_statement "UPDATE source_files SET freshly_captured = 0"


let mark_all_stale () =
  ResultsDatabase.with_registered_statement mark_all_stale_statement ~f:(fun db stmt ->
      SqliteUtils.sqlite_unit_step db ~finalize:false ~log:"mark_all_stale" stmt )
