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
  (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
     sub-table and the main one, and applying the same "more defined" logic as in Attributes in the
     cases where a proc_name is present in both the sub-table and the main one (main_attr_kind !=
     NULL). All the rows that pass this filter are inserted/updated into the main table. *)
  let copy_stmt =
    Sqlite3.prepare (ResultsDir.get_database ())
      {|
INSERT OR REPLACE INTO attributes
SELECT proc_name, attr_kind, source_file, proc_attributes
FROM (
  SELECT sub.proc_name, sub.attr_kind, sub.source_file, sub.proc_attributes,
         main.attr_kind AS main_attr_kind, main.source_file AS main_source_file
  FROM (
    attached.attributes AS sub
    LEFT JOIN attributes AS main
    ON sub.proc_name = main.proc_name ) )
WHERE
  main_attr_kind is NULL
  OR main_attr_kind < attr_kind
  OR (main_attr_kind = attr_kind AND main_source_file < source_file)
|}
  in
  SqliteUtils.sqlite_unit_step ~log:(Printf.sprintf "copying contents of database '%s'" db_file)
    copy_stmt

let merge ~db_file =
  (* no need to wrap all the individual table merges in a single transaction (to batch writes)
     because we open the table with synchronous=OFF *)
  let main_db = ResultsDir.get_database () in
  SqliteUtils.check_sqlite_error ~fatal:true
    ~log:(Printf.sprintf "attaching database '%s'" db_file)
    (Sqlite3.exec main_db (Printf.sprintf "ATTACH '%s' AS attached" db_file)) ;
  merge_attributes_table ~db_file ;
  SqliteUtils.check_sqlite_error ~fatal:true
    ~log:(Printf.sprintf "detaching database '%s'" db_file)
    (Sqlite3.exec main_db "DETACH attached") ;
  ()

let merge_buck_flavors_results infer_deps_file =
  let one_line line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir]
     -> let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (Config.project_root ^/ "buck-out") ^/ target_results_dir
          else target_results_dir
        in
        merge ~db_file:(infer_out_src ^/ ResultsDir.database_filename)
    | _
     -> assert false
  in
  match Utils.read_file infer_deps_file with
  | Ok lines
   -> List.iter ~f:one_line lines
  | Error error
   -> L.internal_error "Couldn't read deps file '%s': %s" infer_deps_file error
