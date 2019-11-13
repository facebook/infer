(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type attributes_kind = ProcUndefined | ProcObjCAccessor | ProcDefined [@@deriving compare]

let equal_attributes_kind = [%compare.equal: attributes_kind]

let attributes_kind_to_int64 =
  [(ProcUndefined, Int64.zero); (ProcObjCAccessor, Int64.one); (ProcDefined, Int64.of_int 2)]


let int64_of_attributes_kind a =
  List.Assoc.find_exn ~equal:equal_attributes_kind attributes_kind_to_int64 a


let deserialize_attributes_kind =
  let int64_to_attributes_kind = List.Assoc.inverse attributes_kind_to_int64 in
  function[@warning "-8"]
  | Sqlite3.Data.INT n -> List.Assoc.find_exn ~equal:Int64.equal int64_to_attributes_kind n


let proc_kind_of_attr (proc_attributes : ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined
  else if Option.is_some proc_attributes.objc_accessor then ProcObjCAccessor
  else ProcUndefined


let replace pname pname_blob akind source_file attributes proc_desc callees =
  let pname_str = Typ.Procname.to_string pname in
  let akind_int64 = int64_of_attributes_kind akind in
  let proc_desc_blob = Procdesc.SQLite.serialize proc_desc in
  let callees_blob = Typ.Procname.SQLiteList.serialize callees in
  DBWriter.replace_attributes ~pname_str ~pname:pname_blob ~akind:akind_int64 ~source_file
    ~attributes ~proc_desc:proc_desc_blob ~callees:callees_blob


let find_more_defined_statement =
  ResultsDatabase.register_statement
    {|
SELECT attr_kind
FROM procedures
WHERE proc_name = :pname
      AND attr_kind > :akind
|}


let should_try_to_update pname_blob akind =
  ResultsDatabase.with_registered_statement find_more_defined_statement ~f:(fun db find_stmt ->
      Sqlite3.bind find_stmt 1 (* :pname *) pname_blob
      |> SqliteUtils.check_result_code db ~log:"replace bind pname" ;
      Sqlite3.bind find_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind akind))
      |> SqliteUtils.check_result_code db ~log:"replace bind attribute kind" ;
      SqliteUtils.result_single_column_option ~finalize:false ~log:"Attributes.replace" db find_stmt
      |> (* there is no entry with a strictly larger "definedness" for that proc name *)
      Option.is_none )


let select_statement =
  ResultsDatabase.register_statement "SELECT proc_attributes FROM procedures WHERE proc_name = :k"


let select_defined_statement =
  ResultsDatabase.register_statement
    "SELECT proc_attributes FROM procedures WHERE proc_name = :k AND attr_kind = %Ld"
    (int64_of_attributes_kind ProcDefined)


let find ~defined pname_blob =
  (if defined then select_defined_statement else select_statement)
  |> ResultsDatabase.with_registered_statement ~f:(fun db select_stmt ->
         Sqlite3.bind select_stmt 1 pname_blob
         |> SqliteUtils.check_result_code db ~log:"find bind proc name" ;
         SqliteUtils.result_single_column_option ~finalize:false ~log:"Attributes.find" db
           select_stmt
         |> Option.map ~f:ProcAttributes.SQLite.deserialize )


let load pname = Typ.Procname.SQLite.serialize pname |> find ~defined:false

let store ~proc_desc (attr : ProcAttributes.t) =
  let pkind = proc_kind_of_attr attr in
  let key = Typ.Procname.SQLite.serialize attr.proc_name in
  if should_try_to_update key pkind then
    replace attr.proc_name key pkind
      (SourceFile.SQLite.serialize attr.loc.Location.file)
      (ProcAttributes.SQLite.serialize attr)
      proc_desc
      (Option.map proc_desc ~f:Procdesc.get_static_callees |> Option.value ~default:[])


let load_defined pname = Typ.Procname.SQLite.serialize pname |> find ~defined:true

let find_file_capturing_procedure pname =
  Option.map (load pname) ~f:(fun proc_attributes ->
      let source_file = proc_attributes.ProcAttributes.translation_unit in
      let origin =
        (* Procedure coming from include files if it has different location than the file where it
           was captured. *)
        match SourceFile.compare source_file proc_attributes.ProcAttributes.loc.file <> 0 with
        | true ->
            `Include
        | false ->
            `Source
      in
      (source_file, origin) )


let pp_attributes_kind f = function
  | ProcUndefined ->
      F.pp_print_string f "<undefined>"
  | ProcObjCAccessor ->
      F.pp_print_string f "<ObjC accessor>"
  | ProcDefined ->
      F.pp_print_string f "<defined>"
