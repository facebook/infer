(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module YB = Yojson.Basic
module YBU = Yojson.Basic.Util

let merge_procedures_table ~db_file =
  let db = ResultsDatabase.get_database () in
  (* Do the merge purely in SQL for great speed. The query works by doing a left join between the
     sub-table and the main one, and applying the same "more defined" logic as in Attributes in the
     cases where a proc_name is present in both the sub-table and the main one (main.attr_kind !=
     NULL). All the rows that pass this filter are inserted/updated into the main table. *)
  Sqlite3.exec db
    {|
INSERT OR REPLACE INTO procedures
SELECT sub.proc_name, sub.proc_name_hum, sub.attr_kind, sub.source_file, sub.proc_attributes, sub.cfg, sub.callees
FROM (
  attached.procedures AS sub
  LEFT OUTER JOIN procedures AS main
  ON sub.proc_name = main.proc_name )
WHERE
  main.attr_kind IS NULL
  OR main.attr_kind < sub.attr_kind
  OR (main.attr_kind = sub.attr_kind AND main.source_file < sub.source_file)
|}
  |> SqliteUtils.check_result_code db
       ~log:(Printf.sprintf "copying procedures of database '%s'" db_file)


let merge_source_files_table ~db_file =
  let db = ResultsDatabase.get_database () in
  Sqlite3.exec db
    {|
    INSERT OR REPLACE INTO source_files
    SELECT source_file, type_environment, integer_type_widths, procedure_names, 1
    FROM attached.source_files
|}
  |> SqliteUtils.check_result_code db
       ~log:(Printf.sprintf "copying source_files of database '%s'" db_file)


let merge_dbs infer_out_src =
  let db_file = infer_out_src ^/ ResultsDatabase.database_filename in
  let main_db = ResultsDatabase.get_database () in
  Sqlite3.exec main_db (Printf.sprintf "ATTACH '%s' AS attached" db_file)
  |> SqliteUtils.check_result_code main_db ~log:(Printf.sprintf "attaching database '%s'" db_file) ;
  merge_procedures_table ~db_file ;
  merge_source_files_table ~db_file ;
  Sqlite3.exec main_db "DETACH attached"
  |> SqliteUtils.check_result_code main_db ~log:(Printf.sprintf "detaching database '%s'" db_file) ;
  ()


let merge_changed_functions_json infer_out_src =
  let main_changed_fs_file = Config.results_dir ^/ Config.export_changed_functions_output in
  let changed_fs_file = infer_out_src ^/ Config.export_changed_functions_output in
  let main_json = try YB.from_file main_changed_fs_file |> YBU.to_list with Sys_error _ -> [] in
  let changed_json = try YB.from_file changed_fs_file |> YBU.to_list with Sys_error _ -> [] in
  let all_fs =
    `List
      (List.dedup_and_sort
         ~compare:(fun s1 s2 ->
           match (s1, s2) with `String s1, `String s2 -> String.compare s1 s2 | _ -> 0 )
         (List.append main_json changed_json))
  in
  YB.to_file main_changed_fs_file all_fs


let iter_infer_deps infer_deps_file ~f =
  let one_line line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir] ->
        let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (Config.project_root ^/ "buck-out") ^/ target_results_dir
          else target_results_dir
        in
        f infer_out_src
    | _ ->
        assert false
  in
  match Utils.read_file infer_deps_file with
  | Ok lines ->
      List.iter ~f:one_line lines
  | Error error ->
      L.internal_error "Couldn't read deps file '%s': %s" infer_deps_file error


let merge_buck_flavors_results infer_deps_file = iter_infer_deps infer_deps_file ~f:merge_dbs

let merge_buck_changed_functions infer_deps_file =
  iter_infer_deps infer_deps_file ~f:merge_changed_functions_json
