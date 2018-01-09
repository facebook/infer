(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

let merge_attributes_table ~db_file =
  let db = ResultsDatabase.get_database () in
  (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
     sub-table and the main one, and applying the same "more defined" logic as in Attributes in the
     cases where a proc_name is present in both the sub-table and the main one (main.attr_kind !=
     NULL). All the rows that pass this filter are inserted/updated into the main table. *)
  Sqlite3.exec db
    {|
INSERT OR REPLACE INTO attributes
SELECT sub.proc_name, sub.attr_kind, sub.source_file, sub.proc_attributes
FROM (
  attached.attributes AS sub
  LEFT OUTER JOIN attributes AS main
  ON sub.proc_name = main.proc_name )
WHERE
  main.attr_kind IS NULL
  OR main.attr_kind < sub.attr_kind
  OR (main.attr_kind = sub.attr_kind AND main.source_file < sub.source_file)
|}
  |> SqliteUtils.check_sqlite_error db
       ~log:(Printf.sprintf "copying attributes of database '%s'" db_file)


let merge_cfg_table ~db_file =
  let db = ResultsDatabase.get_database () in
  Sqlite3.exec db "INSERT OR REPLACE INTO cfg SELECT * FROM attached.cfg"
  |> SqliteUtils.check_sqlite_error db ~log:(Printf.sprintf "copying cfg of database '%s'" db_file)


let merge ~db_file =
  let main_db = ResultsDatabase.get_database () in
  Sqlite3.exec main_db (Printf.sprintf "ATTACH '%s' AS attached" db_file)
  |> SqliteUtils.check_sqlite_error ~fatal:true main_db
       ~log:(Printf.sprintf "attaching database '%s'" db_file) ;
  merge_attributes_table ~db_file ;
  merge_cfg_table ~db_file ;
  Sqlite3.exec main_db "DETACH attached"
  |> SqliteUtils.check_sqlite_error ~fatal:true main_db
       ~log:(Printf.sprintf "detaching database '%s'" db_file) ;
  ()


let merge_buck_flavors_results infer_deps_file =
  let one_line line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir] ->
        let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (Config.project_root ^/ "buck-out") ^/ target_results_dir
          else target_results_dir
        in
        merge ~db_file:(infer_out_src ^/ ResultsDatabase.database_filename)
    | _ ->
        assert false
  in
  match Utils.read_file infer_deps_file with
  | Ok lines ->
      List.iter ~f:one_line lines
  | Error error ->
      L.internal_error "Couldn't read deps file '%s': %s" infer_deps_file error
