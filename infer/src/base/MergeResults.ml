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

let merge_attributes_table ~into ~db_name ~db_file =
  (* no need to wrap this in a single transaction (to batch writes) because we open the table with
     synchronous=OFF *)
  (* do not go through Attributes so as not to deserialize and reserialize objects pointlessly, and
     so as not to fill the cache with all the attributes (especially since this function will be
     called before forking all the analysis processes. *)
  let copy_stmt =
    Sqlite3.prepare into
      (Printf.sprintf "REPLACE INTO %s SELECT * FROM %s.%s" ResultsDir.attributes_table db_name
         ResultsDir.attributes_table)
  in
  SqliteUtils.sqlite_unit_step ~log:(Printf.sprintf "copying contents of database '%s'" db_file)
    copy_stmt

let merge ~into db_file =
  let db_name = "db" in
  SqliteUtils.check_sqlite_error ~fatal:true
    ~log:(Printf.sprintf "attaching database '%s'" db_file)
    (Sqlite3.exec into (Printf.sprintf "ATTACH '%s' AS %s" db_file db_name)) ;
  let do_merge () = merge_attributes_table ~into ~db_name ~db_file in
  Utils.without_gc ~f:do_merge ;
  SqliteUtils.check_sqlite_error ~fatal:true
    ~log:(Printf.sprintf "detaching database '%s'" db_file)
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
