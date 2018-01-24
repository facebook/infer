(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

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


let exists_statement =
  ResultsDatabase.register_statement "SELECT 1 FROM source_files WHERE source_file = :k"


let is_captured source =
  ResultsDatabase.with_registered_statement exists_statement ~f:(fun db exists_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind exists_stmt 1
      (* :k *)
      |> SqliteUtils.check_sqlite_error db ~log:"load captured source file" ;
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"SourceFiles.is_captured" db exists_stmt
      |> Option.is_some )
