(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

type attributes_kind = ProcUndefined | ProcObjCAccessor | ProcDefined [@@deriving compare]

let equal_attributes_kind = [%compare.equal : attributes_kind]

let attributes_kind_to_int64 =
  [(ProcUndefined, Int64.zero); (ProcObjCAccessor, Int64.one); (ProcDefined, Int64.of_int 2)]


let int64_of_attributes_kind a =
  List.Assoc.find_exn ~equal:equal_attributes_kind attributes_kind_to_int64 a


let deserialize_attributes_kind =
  let int64_to_attributes_kind = List.Assoc.inverse attributes_kind_to_int64 in
  function[@warning "-8"]
    | Sqlite3.Data.INT n -> List.Assoc.find_exn ~equal:Int64.equal int64_to_attributes_kind n


let proc_kind_of_attr (proc_attributes: ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined
  else if Option.is_some proc_attributes.objc_accessor then ProcObjCAccessor
  else ProcUndefined


let replace_statement =
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
SELECT :pname, :akind, :sfile, :pattr
FROM (
  SELECT NULL
  FROM (
    SELECT COALESCE(MAX(attr_kind),-1) AS attr_kind,
           COALESCE(MAX(source_file),"") AS source_file
    FROM procedures
    WHERE proc_name = :pname )
  WHERE attr_kind < :akind
        OR (attr_kind = :akind AND source_file < :sfile) )|}


let replace pname_blob akind loc_file attr_blob =
  ResultsDatabase.with_registered_statement replace_statement ~f:(fun db replace_stmt ->
      Sqlite3.bind replace_stmt 1 (* :pname *) pname_blob
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind pname" ;
      Sqlite3.bind replace_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind akind))
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind attribute kind" ;
      Sqlite3.bind replace_stmt 3 (* :sfile *) loc_file
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind source file" ;
      Sqlite3.bind replace_stmt 4 (* :pattr *) attr_blob
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind proc attributes" ;
      SqliteUtils.sqlite_unit_step db ~finalize:false ~log:"Attributes.replace" replace_stmt )


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
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind pname" ;
      Sqlite3.bind find_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind akind))
      |> SqliteUtils.check_sqlite_error db ~log:"replace bind attribute kind" ;
      SqliteUtils.sqlite_result_step ~finalize:false ~log:"Attributes.replace" db find_stmt
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
         |> SqliteUtils.check_sqlite_error db ~log:"find bind proc name" ;
         SqliteUtils.sqlite_result_step ~finalize:false ~log:"Attributes.find" db select_stmt
         |> Option.map ~f:ProcAttributes.SQLite.deserialize )


let load pname = Typ.Procname.SQLite.serialize pname |> find ~defined:false

let store (attr: ProcAttributes.t) =
  let pkind = proc_kind_of_attr attr in
  let key = Typ.Procname.SQLite.serialize attr.proc_name in
  if should_try_to_update key pkind then
    replace key pkind
      (SourceFile.SQLite.serialize attr.loc.Location.file)
      (ProcAttributes.SQLite.serialize attr)


let load_defined pname = Typ.Procname.SQLite.serialize pname |> find ~defined:true

let find_file_capturing_procedure pname =
  Option.map (load pname) ~f:(fun proc_attributes ->
      let source_file = proc_attributes.ProcAttributes.source_file_captured in
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
