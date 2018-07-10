(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

let select_all_procedures_statement = ResultsDatabase.register_statement "SELECT * FROM procedures"

let pp_all ?filter ~proc_name:proc_name_cond ~attr_kind ~source_file:source_file_cond
    ~proc_attributes fmt () =
  let filter = Filtering.mk_procedure_name_filter ~filter |> Staged.unstage in
  ResultsDatabase.with_registered_statement select_all_procedures_statement ~f:(fun db stmt ->
      let pp_if ?(new_line= false) condition title pp fmt x =
        if condition then (
          if new_line then F.fprintf fmt "@[<v2>" else F.fprintf fmt "@[<h>" ;
          F.fprintf fmt "%s:@ %a@]@;" title pp x )
      in
      let pp_column_if ?new_line condition title deserialize pp fmt column =
        if condition then
          (* repeat the [condition] check so that we do not deserialize if there's nothing to do *)
          pp_if ?new_line condition title pp fmt (Sqlite3.column stmt column |> deserialize)
      in
      let pp_row fmt source_file proc_name =
        let[@warning "-8"] Sqlite3.Data.TEXT proc_name_hum = Sqlite3.column stmt 1 in
        Format.fprintf fmt "@[<v2>%s@,%a%a%a%a@]@\n" proc_name_hum
          (pp_if source_file_cond "source_file" SourceFile.pp)
          source_file
          (pp_if proc_name_cond "proc_name" Typ.Procname.pp)
          proc_name
          (pp_column_if attr_kind "attribute_kind" Attributes.deserialize_attributes_kind
             Attributes.pp_attributes_kind)
          2
          (pp_column_if ~new_line:true proc_attributes "attributes"
             ProcAttributes.SQLite.deserialize ProcAttributes.pp)
          4
      in
      let rec aux () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let proc_name = Sqlite3.column stmt 0 |> Typ.Procname.SQLite.deserialize in
            let source_file = Sqlite3.column stmt 3 |> SourceFile.SQLite.deserialize in
            if filter source_file proc_name then pp_row fmt source_file proc_name ;
            aux ()
        | DONE ->
            ()
        | err ->
            L.die InternalError "procedures_iter: %s (%s)" (Sqlite3.Rc.to_string err)
              (Sqlite3.errmsg db)
      in
      aux () )
