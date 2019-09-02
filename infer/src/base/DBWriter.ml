(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Implementation = struct
  let attribute_replace_statement =
    (* The innermost SELECT returns either the current attributes_kind and source_file associated with
     the given proc name, or default values of (-1,""). These default values have the property that
     they are always "less than" any legit value. More precisely, MAX ensures that some value is
     returned even if there is no row satisfying WHERE (we'll get NULL in that case, the value in
     the row otherwise). COALESCE then returns the first non-NULL value, which will be either the
     value of the row corresponding to that pname in the DB, or the default if no such row exists.

     The next (second-outermost) SELECT filters out that value if it is "more defined" than the ones
     we would like to insert (which will never be the case if the default values are returned). If
     not, it returns a trivial row (consisting solely of NULL since we don't use its values) and the
     INSERT OR REPLACE will proceed and insert or update the values stored into the DB for that
     pname.  *)
    (* TRICK: use the source file to be more deterministic in case the same procedure name is defined
     in several files *)
    (* TRICK: older versions of sqlite (prior to version 3.15.0 (2016-10-14)) do not support row
     values so the lexicographic ordering for (:akind, :sfile) is done by hand *)
    ResultsDatabase.register_statement
      {|
        INSERT OR REPLACE INTO procedures
        SELECT :pname, :proc_name_hum, :akind, :sfile, :pattr, :cfg, :callees
        FROM (
          SELECT NULL
          FROM (
            SELECT COALESCE(MAX(attr_kind),-1) AS attr_kind,
                  COALESCE(MAX(source_file),"") AS source_file
            FROM procedures
            WHERE proc_name = :pname )
          WHERE attr_kind < :akind
                OR (attr_kind = :akind AND source_file <= :sfile) )
      |}


  let replace_attributes ~pname_str ~pname ~akind ~source_file ~attributes ~proc_desc ~callees =
    ResultsDatabase.with_registered_statement attribute_replace_statement
      ~f:(fun db replace_stmt ->
        Sqlite3.bind replace_stmt 1 (* :pname *) pname
        |> SqliteUtils.check_result_code db ~log:"replace bind pname" ;
        Sqlite3.bind replace_stmt 2 (* :proc_name_hum *) (Sqlite3.Data.TEXT pname_str)
        |> SqliteUtils.check_result_code db ~log:"replace bind proc_name_hum" ;
        Sqlite3.bind replace_stmt 3 (* :akind *) (Sqlite3.Data.INT akind)
        |> SqliteUtils.check_result_code db ~log:"replace bind attribute kind" ;
        Sqlite3.bind replace_stmt 4 (* :sfile *) source_file
        |> SqliteUtils.check_result_code db ~log:"replace bind source file" ;
        Sqlite3.bind replace_stmt 5 (* :pattr *) attributes
        |> SqliteUtils.check_result_code db ~log:"replace bind proc attributes" ;
        Sqlite3.bind replace_stmt 6 (* :cfg *) proc_desc
        |> SqliteUtils.check_result_code db ~log:"replace bind cfg" ;
        Sqlite3.bind replace_stmt 7 (* :callees *) callees
        |> SqliteUtils.check_result_code db ~log:"replace bind callees" ;
        SqliteUtils.result_unit db ~finalize:false ~log:"Attributes.replace" replace_stmt )


  let source_file_store_statement =
    ResultsDatabase.register_statement
      {|
        INSERT OR REPLACE INTO source_files
        VALUES (:source, :tenv, :integer_type_widths, :proc_names, :freshly_captured) 
      |}


  let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
    ResultsDatabase.with_registered_statement source_file_store_statement ~f:(fun db store_stmt ->
        Sqlite3.bind store_stmt 1 source_file
        (* :source *)
        |> SqliteUtils.check_result_code db ~log:"store bind source file" ;
        Sqlite3.bind store_stmt 2 tenv
        (* :tenv *)
        |> SqliteUtils.check_result_code db ~log:"store bind type environment" ;
        Sqlite3.bind store_stmt 3 integer_type_widths
        (* :integer_type_widths *)
        |> SqliteUtils.check_result_code db ~log:"store bind integer type widths" ;
        Sqlite3.bind store_stmt 4 proc_names
        (* :proc_names *)
        |> SqliteUtils.check_result_code db ~log:"store bind proc names" ;
        Sqlite3.bind store_stmt 5 (Sqlite3.Data.INT Int64.one)
        (* :freshly_captured *)
        |> SqliteUtils.check_result_code db ~log:"store freshness" ;
        SqliteUtils.result_unit ~finalize:false ~log:"Cfg.store" db store_stmt )


  let mark_all_source_files_stale_statement =
    ResultsDatabase.register_statement "UPDATE source_files SET freshly_captured = 0"


  let mark_all_source_files_stale () =
    ResultsDatabase.with_registered_statement mark_all_source_files_stale_statement
      ~f:(fun db stmt -> SqliteUtils.result_unit db ~finalize:false ~log:"mark_all_stale" stmt)


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


  let merge_dbs ~infer_out_src =
    let db_file = infer_out_src ^/ ResultsDatabase.database_filename in
    let main_db = ResultsDatabase.get_database () in
    Sqlite3.exec main_db (Printf.sprintf "ATTACH '%s' AS attached" db_file)
    |> SqliteUtils.check_result_code main_db
         ~log:(Printf.sprintf "attaching database '%s'" db_file) ;
    merge_procedures_table ~db_file ;
    merge_source_files_table ~db_file ;
    Sqlite3.exec main_db "DETACH attached"
    |> SqliteUtils.check_result_code main_db
         ~log:(Printf.sprintf "detaching database '%s'" db_file) ;
    ()


  let canonicalize () =
    let db = ResultsDatabase.get_database () in
    SqliteUtils.exec db ~log:"running VACUUM" ~stmt:"VACUUM"
end

module Command = struct
  type t =
    | ReplaceAttributes of
        { pname_str: string
        ; pname: Sqlite3.Data.t
        ; akind: int64
        ; source_file: Sqlite3.Data.t
        ; attributes: Sqlite3.Data.t
        ; proc_desc: Sqlite3.Data.t
        ; callees: Sqlite3.Data.t }
    | AddSourceFile of
        { source_file: Sqlite3.Data.t
        ; tenv: Sqlite3.Data.t
        ; integer_type_widths: Sqlite3.Data.t
        ; proc_names: Sqlite3.Data.t }
    | MarkAllSourceFilesStale
    | MergeDBs of {infer_out_src: string}
    | Vacuum

  let execute = function
    | ReplaceAttributes {pname_str; pname; akind; source_file; attributes; proc_desc; callees} ->
        Implementation.replace_attributes ~pname_str ~pname ~akind ~source_file ~attributes
          ~proc_desc ~callees
    | AddSourceFile {source_file; tenv; integer_type_widths; proc_names} ->
        Implementation.add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names
    | MarkAllSourceFilesStale ->
        Implementation.mark_all_source_files_stale ()
    | MergeDBs {infer_out_src} ->
        Implementation.merge_dbs ~infer_out_src
    | Vacuum ->
        Implementation.canonicalize ()
end

let perform cmd = Command.execute cmd

let replace_attributes ~pname_str ~pname ~akind ~source_file ~attributes ~proc_desc ~callees =
  Command.ReplaceAttributes {pname_str; pname; akind; source_file; attributes; proc_desc; callees}
  |> perform


let add_source_file ~source_file ~tenv ~integer_type_widths ~proc_names =
  Command.AddSourceFile {source_file; tenv; integer_type_widths; proc_names} |> perform


let mark_all_source_files_stale () = perform Command.MarkAllSourceFilesStale

let merge_dbs ~infer_out_src = Command.MergeDBs {infer_out_src} |> perform

let canonicalize () = perform Command.Vacuum
