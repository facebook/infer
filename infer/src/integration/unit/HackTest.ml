(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let%test_module "naming_table" =
  ( module struct
    (* Scaled down version of Hack's naming table.
       The full version is
       CREATE TABLE NAMING_FILE_INFO(
          FILE_INFO_ID INTEGER PRIMARY KEY AUTOINCREMENT,
          PATH_PREFIX_TYPE INTEGER NOT NULL,
          PATH_SUFFIX TEXT NOT NULL,
          TYPE_CHECKER_MODE INTEGER,
          DECL_HASH INTEGER,
          CLASSES TEXT,
          CONSTS TEXT,
          FUNS TEXT,
          TYPEDEFS TEXT,
          MODULES TEXT
        );
    *)
    (* TODO(vsiles): speed things up by computing the hash
     * and querying FILE_INFO_ID instead of CLASSES
     *)
    let create_table =
      {| CREATE TABLE NAMING_FILE_INFO(
          FILE_INFO_ID INTEGER PRIMARY KEY AUTOINCREMENT,
          PATH_SUFFIX TEXT NOT NULL,
          CLASSES TEXT
        );
        |}


    let%expect_test _ =
      let show (_, l) = List.to_string ~f:(fun s -> s) l in
      let open Sqlite3 in
      let& db = Sqlite3.db_open "in-memory.sql" ~memory:true in
      let _ = Sqlite3.exec db create_table in
      let _ =
        Sqlite3.exec db
          "INSERT INTO NAMING_FILE_INFO (FILE_INFO_ID, PATH_SUFFIX, CLASSES) VALUES (0, \"world\", \
           \"hello\");"
      in
      let good = Integration.Hack.location_of_class_db db ~class_name:"hello" in
      let bad = Integration.Hack.location_of_class_db db ~class_name:"bad" in
      F.printf "%s" (show good) ;
      [%expect {| (world) |}] ;
      F.printf "%s" (show bad) ;
      [%expect {| () |}]
  end )
