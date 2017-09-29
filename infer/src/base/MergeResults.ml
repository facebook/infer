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

let all_attributes ~into ~db_name =
  let select_stmt =
    Sqlite3.prepare into
      (Printf.sprintf "SELECT value FROM %s.%s" db_name ResultsDir.attributes_table)
  in
  List.filter_map ~f:(Option.bind ~f:Attributes.Store.value_of_blob)
    (SqliteUtils.sqlite_result_rev_list_step ~log:"select" select_stmt)

let merge_attributes_table ~into ~db_name =
  let rows = all_attributes ~into ~db_name in
  (* no need to wrap this in a single transaction because we open the table with synchronous=OFF *)
  List.iter rows ~f:Attributes.store

let merge ~into db =
  let db_name = "db" in
  SqliteUtils.check_sqlite_error ~fatal:true ~log:"attaching db"
    (Sqlite3.exec into (Printf.sprintf "ATTACH '%s' AS %s" db db_name)) ;
  let do_merge () = merge_attributes_table ~into ~db_name in
  Utils.without_gc ~f:do_merge ;
  SqliteUtils.check_sqlite_error ~fatal:true ~log:"detaching db"
    (Sqlite3.exec into (Printf.sprintf "DETACH %s" db_name)) ;
  ()

let merge_buck_flavors_results infer_deps_file =
  let into = ResultsDir.get_database () in
  let one_line line =
    match String.split ~on:'\t' line with
    | [_; _; target_results_dir]
     -> let infer_out_src =
          if Filename.is_relative target_results_dir then
            Filename.dirname (Config.project_root ^/ "buck-out") ^/ target_results_dir
          else target_results_dir
        in
        merge ~into (infer_out_src ^/ ResultsDir.database_filename)
    | _
     -> assert false
  in
  match Utils.read_file infer_deps_file with
  | Ok lines
   -> List.iter ~f:one_line lines
  | Error error
   -> L.internal_error "Couldn't read deps file '%s': %s" infer_deps_file error
