(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(* HACK: we want to prepare some Sqlite statements as they take arguments and they may be used
   many times.  We cannot do this using [Database.register_statement] because the queries use named
   attached databases and the names will only be attached in the code path leading to this module.
   We hide the `prepare` in lazy values and we do not finalize the statements. *)

(* Copy all procedures without CFGs into main capture database, which is expected to be empty. *)
let copy_undefined_procedures () =
  let stmt =
    {|
        INSERT INTO procedures
        SELECT proc_uid, proc_attributes, cfg, callees
        FROM attached.procedures
        WHERE cfg is NULL
      |}
  in
  Database.get_database CaptureDatabase
  |> SqliteUtils.exec ~log:"copying undefined procedures" ~stmt


(* Copy procedure row if not already present in the [procedures] table. Returns if any rows updated. *)
let copy_procedure_with_uid =
  let stmt =
    lazy
      (Sqlite3.prepare
         (Database.get_database CaptureDatabase)
         {|
            INSERT INTO procedures
            SELECT proc_uid, proc_attributes, cfg, callees
            FROM attached.procedures
            WHERE proc_uid = :k
              AND NOT EXISTS (SELECT 1 FROM procedures WHERE proc_uid = :k)
          |} )
  in
  fun ~proc_uid ->
    let db = Database.get_database CaptureDatabase in
    let stmt = Lazy.force stmt in
    Sqlite3.reset stmt |> SqliteUtils.check_result_code db ~log:"reset" ;
    Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT proc_uid)
    |> SqliteUtils.check_result_code db ~log:"bind proc_uid" ;
    SqliteUtils.result_unit ~finalize:false ~log:"copy proc_uid" db stmt ;
    let result = Sqlite3.changes db > 0 in
    if result then L.debug Capture Verbose "Added procedure: %s@\n" proc_uid ;
    result


(* Copy source file if not already present in the [source_files] table. Returns if any rows updated. *)
let copy_sourcefile =
  let stmt =
    lazy
      (Sqlite3.prepare
         (Database.get_database CaptureDatabase)
         {|
            INSERT INTO source_files
            SELECT source_file, type_environment, integer_type_widths, procedure_names, 1
            FROM attached.source_files
            WHERE source_file = :k
              AND NOT EXISTS (SELECT 1 FROM source_files WHERE source_file = :k)
          |} )
  in
  fun file ->
    let db = Database.get_database CaptureDatabase in
    let stmt = Lazy.force stmt in
    Sqlite3.reset stmt |> SqliteUtils.check_result_code db ~log:"reset" ;
    Sqlite3.bind stmt 1 (SourceFile.SQLite.serialize file)
    |> SqliteUtils.check_result_code db ~log:"bind sourcefile" ;
    SqliteUtils.result_unit ~finalize:false ~log:"copy sourcefile" db stmt ;
    let result = Sqlite3.changes db > 0 in
    if result then L.debug Capture Verbose "Added sourcefile: %a@\n" SourceFile.pp file ;
    result


(* Copy source file if not already present in the [source_files] table. Returns if any rows updated. *)
let copy_sourcefile_with_procedures file =
  let file_updated = copy_sourcefile file in
  SourceFiles.proc_names_of_source file
  |> List.fold ~init:file_updated ~f:(fun acc procname ->
         copy_procedure_with_uid ~proc_uid:(Procname.to_unique_id procname) || acc )


let get_sourcefile_and_callees =
  let statement =
    Database.register_statement CaptureDatabase
      "SELECT proc_attributes, callees FROM procedures WHERE proc_uid = :k"
  in
  fun ~proc_uid ->
    Database.with_registered_statement statement ~f:(fun db select_stmt ->
        Sqlite3.bind select_stmt 1 (Sqlite3.Data.TEXT proc_uid)
        |> SqliteUtils.check_result_code db ~log:"bind proc_uid" ;
        SqliteUtils.result_option db ~finalize:false ~log:"get_sourcefile_and_callees" select_stmt
          ~read_row:(fun stmt ->
            let attributes = Sqlite3.column stmt 0 |> ProcAttributes.SQLite.deserialize in
            let callees = Sqlite3.column stmt 1 |> Procname.SQLiteList.deserialize in
            (attributes.translation_unit, callees) ) )


(* recursively copy a procedure row and its static callees, while recording its source file
   in the given [HashSet] *)
let rec copy_procedure_and_callees acc ~proc_uid =
  if copy_procedure_with_uid ~proc_uid then (
    let sourcefile, callees = get_sourcefile_and_callees ~proc_uid |> Option.value_exn in
    SourceFile.HashSet.add sourcefile acc ;
    List.iter callees ~f:(fun callee ->
        copy_procedure_and_callees acc ~proc_uid:(Procname.to_unique_id callee) |> ignore ) ;
    true )
  else false


(* copy sourcefile, then recursively copy its procedures while marking their sourcefiles
   in the accumulator, but without copying those *)
let copy_sourcefile_with_procedures_recursively acc file =
  copy_sourcefile file |> ignore ;
  SourceFiles.proc_names_of_source file
  |> List.iter ~f:(fun procname ->
         copy_procedure_and_callees acc ~proc_uid:(Procname.to_unique_id procname) |> ignore )


let extract ~files ~input_capture_path =
  let db_file = ResultsDirEntryName.get_path ~results_dir:input_capture_path CaptureDB in
  let db = Database.get_database CaptureDatabase in
  SqliteUtils.with_attached_db db ~db_file ~db_name:"attached" ~immutable:true ~f:(fun () ->
      if SourceFile.Set.is_empty files then L.die UserError "No files where found in the index." ;
      copy_undefined_procedures () ;
      let sources_to_copy = SourceFile.HashSet.create 1 in
      SourceFile.Set.iter
        (fun s -> copy_sourcefile_with_procedures_recursively sources_to_copy s)
        files ;
      SourceFile.HashSet.iter sources_to_copy
      |> Iter.iter (fun s -> copy_sourcefile_with_procedures s |> ignore) ) ;
  DBWriter.canonicalize ()


let complete ~input_capture_path =
  let missing_deps = MissingDependencies.load () in
  let db_file = ResultsDirEntryName.get_path ~results_dir:input_capture_path CaptureDB in
  let db = Database.get_database CaptureDatabase in
  let rows_changed =
    SqliteUtils.with_attached_db db ~db_file ~db_name:"attached" ~immutable:true ~f:(fun () ->
        MissingDependencies.ProcUidSet.fold
          (fun proc_uid acc -> copy_procedure_and_callees missing_deps.sources ~proc_uid || acc)
          missing_deps.procedures false
        |> SourceFile.HashSet.fold
             (fun s acc -> copy_sourcefile_with_procedures s || acc)
             missing_deps.sources )
  in
  Unix.unlink (ResultsDirEntryName.get_path ~results_dir:Config.results_dir MissingProcedures) ;
  Unix.unlink (ResultsDirEntryName.get_path ~results_dir:Config.results_dir MissingSourceFiles) ;
  DBWriter.canonicalize () ;
  rows_changed
