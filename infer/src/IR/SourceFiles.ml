(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
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
      |> SqliteUtils.check_result_code db ~log:"get_existing_data bind source file" ;
      SqliteUtils.result_option ~finalize:false db ~log:"looking for pre-existing cfgs" stmt
        ~read_row:(fun stmt ->
          let cfgs = Sqlite3.column stmt 0 |> Cfg.SQLite.deserialize
          and tenv = Sqlite3.column stmt 1 |> Tenv.SQLite.deserialize in
          (cfgs, tenv) ) )


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
      |> SqliteUtils.check_result_code db ~log:"store bind source file" ;
      Cfg.SQLite.serialize cfg |> Sqlite3.bind store_stmt 2
      (* :cfg *)
      |> SqliteUtils.check_result_code db ~log:"store bind cfg" ;
      Tenv.SQLite.serialize tenv |> Sqlite3.bind store_stmt 3
      (* :tenv *)
      |> SqliteUtils.check_result_code db ~log:"store bind type environment" ;
      Cfg.get_all_proc_names cfg |> Typ.Procname.SQLiteList.serialize |> Sqlite3.bind store_stmt 4
      (* :proc_names *)
      |> SqliteUtils.check_result_code db ~log:"store bind proc names" ;
      Sqlite3.bind store_stmt 5 (Sqlite3.Data.INT Int64.one)
      (* :freshly_captured *)
      |> SqliteUtils.check_result_code db ~log:"store freshness" ;
      SqliteUtils.result_unit ~finalize:false ~log:"Cfg.store" db store_stmt )


let get_all () =
  let db = ResultsDatabase.get_database () in
  (* we could also register this statement but it's typically used only once per run so just prepare
     it inside the function *)
  Sqlite3.prepare db "SELECT source_file FROM source_files"
  |> IContainer.rev_map_to_list
       ~fold:(SqliteUtils.result_fold_single_column_rows db ~log:"getting all source files")
       ~f:SourceFile.SQLite.deserialize


let load_proc_names_statement =
  ResultsDatabase.register_statement
    "SELECT procedure_names FROM source_files WHERE source_file = :k"


let proc_names_of_source source =
  ResultsDatabase.with_registered_statement load_proc_names_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
      SqliteUtils.result_single_column_option ~finalize:false db
        ~log:"SourceFiles.proc_names_of_source" load_stmt
      |> Option.value_map ~default:[] ~f:Typ.Procname.SQLiteList.deserialize )


let exists_source_statement =
  ResultsDatabase.register_statement "SELECT 1 FROM source_files WHERE source_file = :k"


let is_captured source =
  ResultsDatabase.with_registered_statement exists_source_statement ~f:(fun db exists_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind exists_stmt 1
      (* :k *)
      |> SqliteUtils.check_result_code db ~log:"load captured source file" ;
      SqliteUtils.result_single_column_option ~finalize:false ~log:"SourceFiles.is_captured" db
        exists_stmt
      |> Option.is_some )


let is_non_empty_statement =
  ResultsDatabase.register_statement "SELECT 1 FROM source_files LIMIT 1"


let is_empty () =
  ResultsDatabase.with_registered_statement is_non_empty_statement ~f:(fun db stmt ->
      SqliteUtils.result_single_column_option ~finalize:false ~log:"SourceFiles.is_empty" db stmt
      |> Option.is_none )


let is_freshly_captured_statement =
  ResultsDatabase.register_statement
    "SELECT freshly_captured FROM source_files WHERE source_file = :k"


let deserialize_freshly_captured = function[@warning "-8"]
  | Sqlite3.Data.INT p ->
      Int64.equal p Int64.one


let is_freshly_captured source =
  ResultsDatabase.with_registered_statement is_freshly_captured_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
      SqliteUtils.result_single_column_option ~finalize:false
        ~log:"SourceFiles.is_freshly_captured" db load_stmt
      |> Option.value_map ~default:false ~f:deserialize_freshly_captured )


let mark_all_stale_statement =
  ResultsDatabase.register_statement "UPDATE source_files SET freshly_captured = 0"


let mark_all_stale () =
  ResultsDatabase.with_registered_statement mark_all_stale_statement ~f:(fun db stmt ->
      SqliteUtils.result_unit db ~finalize:false ~log:"mark_all_stale" stmt )


let select_all_source_files_statement =
  ResultsDatabase.register_statement "SELECT * FROM source_files"


let pp_all ?filter ~cfgs ~type_environment ~procedure_names ~freshly_captured fmt () =
  let filter = Staged.unstage (Filtering.mk_source_file_filter ~filter) in
  let pp_procnames fmt procs =
    F.fprintf fmt "@[<v>" ;
    List.iter ~f:(F.fprintf fmt "%a@," Typ.Procname.pp) procs ;
    F.fprintf fmt "@]"
  in
  let pp_if stmt title condition deserialize pp fmt column =
    if condition then
      F.fprintf fmt "  @[<v2>%s@,%a@]@;" title pp (Sqlite3.column stmt column |> deserialize)
  in
  let pp_row stmt fmt source_file =
    F.fprintf fmt "%a@,%a%a%a%a" SourceFile.pp source_file
      (pp_if stmt "cfgs" cfgs Cfg.SQLite.deserialize Cfg.pp_proc_signatures)
      1
      (pp_if stmt "type_environment" type_environment Tenv.SQLite.deserialize Tenv.pp_per_file)
      2
      (pp_if stmt "procedure_names" procedure_names Typ.Procname.SQLiteList.deserialize
         pp_procnames)
      3
      (pp_if stmt "freshly_captured" freshly_captured deserialize_freshly_captured
         Format.pp_print_bool)
      4
  in
  ResultsDatabase.with_registered_statement select_all_source_files_statement ~f:(fun db stmt ->
      let pp fmt column =
        let source_file = SourceFile.SQLite.deserialize column in
        if filter source_file then pp_row stmt fmt source_file
      in
      let pp_result fmt stmt =
        Container.iter stmt ~f:(pp fmt)
          ~fold:(SqliteUtils.result_fold_single_column_rows db ~log:"printing all source files")
      in
      F.fprintf fmt "@[<v>%a@]" pp_result stmt )
