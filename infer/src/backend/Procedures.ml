(*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module F = Format
module L = Logging

let select_all_procedures_like_statement =
  ResultsDatabase.register_statement
    "SELECT * FROM procedures WHERE proc_name_hum LIKE :proc_name_like AND source_file LIKE \
     :source_file_like"


let pp_all ?filter ~proc_name ~attr_kind ~source_file ~proc_attributes fmt () =
  let source_file_like, proc_name_like =
    match filter with
    | None ->
        let wildcard = Sqlite3.Data.TEXT "%" in
        (wildcard, wildcard)
    | Some filter_string ->
      match String.lsplit2 ~on:':' filter_string with
      | Some (source_file_like, proc_name_like) ->
          (Sqlite3.Data.TEXT source_file_like, Sqlite3.Data.TEXT proc_name_like)
      | None ->
          L.die UserError
            "Invalid filter for procedures. Please see the documentation for --procedures-filter \
             in `infer explore --help`."
  in
  ResultsDatabase.with_registered_statement select_all_procedures_like_statement ~f:(fun db stmt ->
      Sqlite3.bind stmt 1 (* :proc_name_like *) proc_name_like
      |> SqliteUtils.check_sqlite_error db ~log:"procedures filter pname bind" ;
      Sqlite3.bind stmt 2 (* :source_file_like *) source_file_like
      |> SqliteUtils.check_sqlite_error db ~log:"procedures filter source file bind" ;
      let pp_if ?(newline= false) condition deserialize pp fmt column =
        if condition then (
          if newline then F.fprintf fmt "@\n  " ;
          F.fprintf fmt "%a@ " pp (Sqlite3.column stmt column |> deserialize) )
      in
      let rec aux () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let proc_name_hum =
              match[@warning "-8"] Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s
            in
            Format.fprintf fmt "@[<h2>%s:@ %a%a%a%a@]@\n" proc_name_hum
              (pp_if source_file SourceFile.SQLite.deserialize SourceFile.pp)
              3
              (pp_if proc_name Typ.Procname.SQLite.deserialize Typ.Procname.pp)
              0
              (pp_if attr_kind Attributes.deserialize_attributes_kind Attributes.pp_attributes_kind)
              2
              (pp_if ~newline:true proc_attributes ProcAttributes.SQLite.deserialize
                 ProcAttributes.pp)
              4 ;
            aux ()
        | DONE ->
            ()
        | err ->
            L.die InternalError "procedures_iter: %s (%s)" (Sqlite3.Rc.to_string err)
              (Sqlite3.errmsg db)
      in
      aux () )
