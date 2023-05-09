(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let select_existing_statement =
  Database.register_statement CaptureDatabase
    "SELECT type_environment, procedure_names FROM source_files WHERE source_file = :source AND \
     freshly_captured = 1"


let get_existing_data source_file =
  Database.with_registered_statement select_existing_statement ~f:(fun db stmt ->
      SourceFile.SQLite.serialize source_file
      |> Sqlite3.bind stmt 1
      (* :source *)
      |> SqliteUtils.check_result_code db ~log:"get_existing_data bind source file" ;
      SqliteUtils.result_option ~finalize:false db ~log:"looking for pre-existing source file data"
        stmt ~read_row:(fun stmt ->
          let tenv = Sqlite3.column stmt 0 |> Tenv.SQLite.deserialize
          and proc_names = Sqlite3.column stmt 1 |> Procname.SQLiteList.deserialize in
          (tenv, proc_names) ) )


let proc_names_of_source =
  let stmt =
    Database.register_statement CaptureDatabase
      "SELECT procedure_names FROM source_files WHERE source_file = :k"
  in
  fun source ->
    let res_opt =
      Database.with_registered_statement stmt ~f:(fun db stmt ->
          SourceFile.SQLite.serialize source
          |> Sqlite3.bind stmt 1
          |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
          SqliteUtils.result_single_column_option ~finalize:false db
            ~log:"SourceFiles.proc_names_of_source" stmt
          |> Option.map ~f:Procname.SQLiteList.deserialize )
    in
    match res_opt with
    | None ->
        MissingDependencies.record_sourcefile source ;
        []
    | Some procs ->
        procs


let add source_file cfg tenv integer_type_widths =
  let tenv, proc_names =
    (* The same source file may get captured several times in a single capture event, for instance
       because it is compiled with different options, or from different symbolic links. This may
       generate different procedures in each phase, so make an attempt to merge them into the same
       tenv. *)
    let new_proc_names = Cfg.get_all_defined_proc_names cfg in
    match get_existing_data source_file with
    | Some (old_tenv, old_proc_names) ->
        L.debug Capture Quiet "Merging capture data for already-captured '%a'@\n" SourceFile.pp
          source_file ;
        (* merge the proc names defined in the source file using a hashtbl so that order is
           preserved but merging is still linear time *)
        let existing_proc_names = Caml.Hashtbl.create (List.length old_proc_names) in
        List.iter old_proc_names ~f:(fun proc_name ->
            Caml.Hashtbl.add existing_proc_names proc_name () ) ;
        let proc_names =
          List.fold new_proc_names ~init:old_proc_names ~f:(fun proc_names proc_name ->
              if not (Caml.Hashtbl.mem existing_proc_names proc_name) then proc_name :: proc_names
              else proc_names )
        in
        (Tenv.merge_per_file ~dst:old_tenv ~src:tenv, proc_names)
    | None ->
        (tenv, new_proc_names)
  in
  if Config.mark_unchanged_procs then
    (* Mark any newly-captured procedure as unchanged if it is structurally identical to the
       previously-captured version, in order to avoid invalidating summaries of unchanged
       procedures in changed files.

       NB that this only works if the capture DB has some previous version of the procedure
       and thus won't work with integrations that require merging (e.g. buck).
    *)
    List.iter (proc_names_of_source source_file) ~f:(fun pname ->
        IOption.iter2 (Procdesc.load pname) (Procname.Hash.find_opt cfg pname)
          ~f:(fun old_pdesc new_pdesc ->
            Procdesc.mark_if_unchanged ~old_pdesc ~new_pdesc ;
            (* If there are already stored attributes for this procedure, delete them from the DB
               to ensure that the one we have in [cfg] is persisted deterministically. *)
            if (Procdesc.get_attributes new_pdesc).changed then
              DBWriter.delete_attributes ~proc_uid:(Procname.to_unique_id pname) ) ) ;
  let tenv = Tenv.normalize tenv in
  Cfg.store source_file cfg ;
  DBWriter.add_source_file
    ~source_file:(SourceFile.SQLite.serialize source_file)
    ~tenv:(Tenv.SQLite.serialize tenv)
    ~integer_type_widths:(IntegerWidths.SQLite.serialize integer_type_widths)
    ~proc_names:(Procname.SQLiteList.serialize proc_names)


let get_all ~filter () =
  let db = Database.get_database CaptureDatabase in
  (* we could also register this statement but it's typically used only once per run so just prepare
     it inside the function *)
  Sqlite3.prepare db "SELECT source_file FROM source_files"
  |> IContainer.rev_filter_map_to_list
       ~fold:(SqliteUtils.result_fold_single_column_rows db ~log:"getting all source files")
       ~f:(fun column ->
         let source_file = SourceFile.SQLite.deserialize column in
         Option.some_if (filter source_file) source_file )


let is_non_empty_statement =
  Database.register_statement CaptureDatabase "SELECT 1 FROM source_files LIMIT 1"


let is_empty () =
  Database.with_registered_statement is_non_empty_statement ~f:(fun db stmt ->
      SqliteUtils.result_single_column_option ~finalize:false ~log:"SourceFiles.is_empty" db stmt
      |> Option.is_none )


let is_freshly_captured_statement =
  Database.register_statement CaptureDatabase
    "SELECT freshly_captured FROM source_files WHERE source_file = :k"


let deserialize_freshly_captured = function[@warning "-partial-match"]
  | Sqlite3.Data.INT p ->
      Int64.equal p Int64.one


let is_freshly_captured source =
  Database.with_registered_statement is_freshly_captured_statement ~f:(fun db load_stmt ->
      SourceFile.SQLite.serialize source
      |> Sqlite3.bind load_stmt 1
      |> SqliteUtils.check_result_code db ~log:"load bind source file" ;
      SqliteUtils.result_single_column_option ~finalize:false ~log:"SourceFiles.is_freshly_captured"
        db load_stmt
      |> Option.exists ~f:deserialize_freshly_captured )


let mark_all_stale () = DBWriter.mark_all_source_files_stale ()

let select_all_source_files_statement =
  Database.register_statement CaptureDatabase
    {|
  SELECT source_file
  , type_environment
  , procedure_names
  , freshly_captured
  FROM source_files |}


let pp_all ~filter ~type_environment ~procedure_names ~freshly_captured fmt () =
  let pp_procnames fmt procs =
    F.fprintf fmt "@[<v>" ;
    List.iter ~f:(F.fprintf fmt "%a@," Procname.pp) procs ;
    F.fprintf fmt "@]"
  in
  let pp_if stmt title condition deserialize pp fmt column =
    if condition then
      F.fprintf fmt "  @[<v2>%s@,%a@]@;" title pp (Sqlite3.column stmt column |> deserialize)
  in
  let pp_row stmt fmt source_file =
    F.fprintf fmt "%a@,%a%a%a" SourceFile.pp source_file
      (pp_if stmt "type_environment" type_environment Tenv.SQLite.deserialize Tenv.pp_per_file)
      1
      (pp_if stmt "procedure_names" procedure_names Procname.SQLiteList.deserialize pp_procnames)
      2
      (pp_if stmt "freshly_captured" freshly_captured deserialize_freshly_captured
         Format.pp_print_bool )
      3
  in
  Database.with_registered_statement select_all_source_files_statement ~f:(fun db stmt ->
      let pp fmt column =
        let source_file = SourceFile.SQLite.deserialize column in
        if filter source_file then pp_row stmt fmt source_file
      in
      let pp_result fmt stmt =
        Container.iter stmt ~f:(pp fmt)
          ~fold:
            (SqliteUtils.result_fold_single_column_rows ~finalize:false db
               ~log:"printing all source files" )
      in
      F.fprintf fmt "@[<v>%a@]" pp_result stmt )


let get_procs_in_file proc_name =
  let source_file =
    match Attributes.load proc_name with
    | Some {ProcAttributes.translation_unit} ->
        Some translation_unit
    | None ->
        None
  in
  Option.value_map source_file ~default:[] ~f:proc_names_of_source
