(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type attributes_kind = ProcUndefined | ProcDefined [@@deriving compare, equal]

let attributes_kind_to_int64 = [(ProcUndefined, Int64.zero); (ProcDefined, Int64.of_int 2)]

let int64_of_attributes_kind a =
  List.Assoc.find_exn ~equal:equal_attributes_kind attributes_kind_to_int64 a


let deserialize_attributes_kind =
  let int64_to_attributes_kind = List.Assoc.inverse attributes_kind_to_int64 in
  function[@warning "-8"]
  | Sqlite3.Data.INT n -> List.Assoc.find_exn ~equal:Int64.equal int64_to_attributes_kind n


let proc_kind_of_attr (proc_attributes : ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined else ProcUndefined


let should_try_to_update =
  let find_more_defined_statement =
    ResultsDatabase.register_statement
      {|
        SELECT attr_kind
        FROM procedures
        WHERE proc_uid = :uid
              AND attr_kind > :akind
      |}
  in
  fun proc_uid attr_kind ->
    ResultsDatabase.with_registered_statement find_more_defined_statement ~f:(fun db find_stmt ->
        Sqlite3.bind find_stmt 1 (* uid *) (Sqlite3.Data.TEXT proc_uid)
        |> SqliteUtils.check_result_code db ~log:"should update bind pname_uid" ;
        Sqlite3.bind find_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind attr_kind))
        |> SqliteUtils.check_result_code db ~log:"should update bind attribute kind" ;
        SqliteUtils.result_single_column_option ~finalize:false ~log:"Attributes.replace" db
          find_stmt
        |> (* there is no entry with a strictly larger "definedness" for that proc name *)
        Option.is_none )


let find =
  let select_statement =
    ResultsDatabase.register_statement "SELECT proc_attributes FROM procedures WHERE proc_uid = :k"
  in
  fun proc_uid ->
    ResultsDatabase.with_registered_statement select_statement ~f:(fun db select_stmt ->
        Sqlite3.bind select_stmt 1 (Sqlite3.Data.TEXT proc_uid)
        |> SqliteUtils.check_result_code db ~log:"find bind proc name" ;
        SqliteUtils.result_single_column_option ~finalize:false ~log:"Attributes.find" db
          select_stmt
        |> Option.map ~f:ProcAttributes.SQLite.deserialize )


let update_source_file =
let select_existing_statement =
  ResultsDatabase.register_statement
    "SELECT procedure_names FROM source_files WHERE source_file = :source"
in
let load_proc_names source_file =
  ResultsDatabase.with_registered_statement select_existing_statement ~f:(fun db stmt ->
    SourceFile.SQLite.serialize source_file
    |> Sqlite3.bind stmt 1
    (* :source *)
    |> SqliteUtils.check_result_code db ~log:"get_existing_data bind source file" ;
    SqliteUtils.result_option ~finalize:false db ~log:"looking for pre-existing source file data"
      stmt ~read_row:(fun stmt ->
        Sqlite3.column stmt 0 |> Procname.SQLiteList.deserialize ) )
in
fun ~source_file ~tenv ~integer_type_widths ~procname ->
  let new_proc_names = 
    match load_proc_names source_file with
    | Some old_proc_names ->
      if not (List.mem old_proc_names procname ~equal:Procname.equal) then 
        List.append old_proc_names [procname]
      else
        old_proc_names
    | None ->
      [procname]
  in
  DBWriter.add_source_file
    ~source_file:(SourceFile.SQLite.serialize source_file) 
    ~tenv:(Tenv.SQLite.serialize tenv) 
    ~integer_type_widths:(Typ.IntegerWidths.SQLite.serialize integer_type_widths)
    ~proc_names:(Procname.SQLiteList.serialize new_proc_names)


let load, clear_cache, store =
  (* capture DB attribute cache: only keeps positive entries as analysis may add entries *)
  let cache : ProcAttributes.t Procname.Hash.t = Procname.Hash.create 1 in
  let load pname =
    match Procname.Hash.find_opt cache pname with
    | Some _ as result ->
        result
    | None ->
        let result = find (Procname.to_unique_id pname) in
        Option.iter result ~f:(Procname.Hash.add cache pname) ;
        result
  in
  let clear_cache () = Procname.Hash.clear cache in
  let store ~proc_desc (attr : ProcAttributes.t) =
    if attr.is_defined && Option.is_none proc_desc then
      Logging.die InternalError "Was given DEFINED procedure without procdesc: %a@."
        ProcAttributes.pp attr ;
    if (not attr.is_defined) && Option.is_some proc_desc then
      Logging.die InternalError
        "Was given UNDEFINED procedure WITH procdesc:@\nAttributes:%a@\nProcdesc:%a@."
        ProcAttributes.pp attr (Pp.option Procdesc.pp_signature) proc_desc ;
    let pname = attr.proc_name in
    let akind = proc_kind_of_attr attr in
    let proc_uid = Procname.to_unique_id pname in
    if should_try_to_update proc_uid akind then (
      let proc_name = Procname.SQLite.serialize pname in
      let attr_kind = int64_of_attributes_kind akind in
      let cfg = Procdesc.SQLite.serialize proc_desc in
      let proc_attributes = ProcAttributes.SQLite.serialize attr in
      let source_file = SourceFile.SQLite.serialize attr.loc.Location.file in
      let callees =
        Option.value_map proc_desc ~f:Procdesc.get_static_callees ~default:[]
        |> Procname.SQLiteList.serialize
      in
      DBWriter.replace_attributes ~proc_uid ~proc_name ~attr_kind ~source_file ~proc_attributes ~cfg
        ~callees ;
      if Language.curr_language_is Language.CIL then (
        update_source_file ~source_file:attr.loc.Location.file ~tenv:Tenv.Global ~integer_type_widths:None ~procname:pname ) ;
      Procname.Hash.remove cache pname )
  in
  (load, clear_cache, store)


let is_no_return pname = match load pname with Some {is_no_return} -> is_no_return | _ -> false

let pp_attributes_kind f = function
  | ProcUndefined ->
      F.pp_print_string f "<undefined>"
  | ProcDefined ->
      F.pp_print_string f "<defined>"