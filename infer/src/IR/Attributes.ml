(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd
module L = Logging

type attributes_kind = ProcUndefined | ProcObjCAccessor | ProcDefined [@@deriving compare]

let int64_of_attributes_kind =
  (* only allocate this once *)
  let int64_two = Int64.of_int 2 in
  function ProcUndefined -> Int64.zero | ProcObjCAccessor -> Int64.one | ProcDefined -> int64_two

let proc_kind_of_attr (proc_attributes: ProcAttributes.t) =
  if proc_attributes.is_defined then ProcDefined
  else if Option.is_some proc_attributes.objc_accessor then ProcObjCAccessor
  else ProcUndefined

module type Data = sig
  val of_pname : Typ.Procname.t -> Sqlite3.Data.t

  val of_source_file : SourceFile.t -> Sqlite3.Data.t

  val of_proc_attr : ProcAttributes.t -> Sqlite3.Data.t

  val to_proc_attr : Sqlite3.Data.t -> ProcAttributes.t
end

module Data : Data = struct
  let pname_to_key = Base.Hashtbl.create (module Typ.Procname) ()

  let of_pname pname =
    let default () = Sqlite3.Data.TEXT (Typ.Procname.to_filename pname) in
    Base.Hashtbl.find_or_add pname_to_key pname ~default

  let of_source_file file = Sqlite3.Data.TEXT (SourceFile.to_string file)

  let to_proc_attr = function[@warning "-8"] Sqlite3.Data.BLOB b -> Marshal.from_string b 0

  let of_proc_attr x = Sqlite3.Data.BLOB (Marshal.to_string x [])
end

let get_replace_statement =
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
  ResultsDir.register_statement
    {|
INSERT OR REPLACE INTO attributes
SELECT :pname, :akind, :sfile, :pattr
FROM (
  SELECT NULL
  FROM (
    SELECT COALESCE(MAX(attr_kind),-1) AS attr_kind,
           COALESCE(MAX(source_file),"") AS source_file
    FROM attributes
    WHERE proc_name = :pname )
  WHERE attr_kind < :akind
        OR (attr_kind = :akind AND source_file < :sfile) )|}

let replace pname_blob akind loc_file attr_blob =
  let replace_stmt = get_replace_statement () in
  Sqlite3.bind replace_stmt 1 (* :pname *) pname_blob
  |> SqliteUtils.check_sqlite_error ~log:"replace bind pname" ;
  Sqlite3.bind replace_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind akind))
  |> SqliteUtils.check_sqlite_error ~log:"replace bind attribute kind" ;
  Sqlite3.bind replace_stmt 3 (* :sfile *) loc_file
  |> SqliteUtils.check_sqlite_error ~log:"replace bind source file" ;
  Sqlite3.bind replace_stmt 4 (* :pattr *) attr_blob
  |> SqliteUtils.check_sqlite_error ~log:"replace bind proc attributes" ;
  SqliteUtils.sqlite_unit_step ~finalize:false ~log:"Attributes.replace" replace_stmt

let get_find_more_defined_statement =
  ResultsDir.register_statement
    {|
SELECT attr_kind
FROM attributes
WHERE proc_name = :pname
      AND attr_kind > :akind
|}

let should_try_to_update pname_blob akind =
  let find_stmt = get_find_more_defined_statement () in
  Sqlite3.bind find_stmt 1 (* :pname *) pname_blob
  |> SqliteUtils.check_sqlite_error ~log:"replace bind pname" ;
  Sqlite3.bind find_stmt 2 (* :akind *) (Sqlite3.Data.INT (int64_of_attributes_kind akind))
  |> SqliteUtils.check_sqlite_error ~log:"replace bind attribute kind" ;
  SqliteUtils.sqlite_result_step ~finalize:false ~log:"Attributes.replace" find_stmt
  |> (* there is no entry with a strictly larger "definedness" for that proc name *) Option.is_none

let get_select_statement =
  ResultsDir.register_statement "SELECT proc_attributes FROM attributes WHERE proc_name = :k"

let get_select_defined_statement =
  ResultsDir.register_statement
    "SELECT proc_attributes FROM attributes WHERE proc_name = :k AND attr_kind = %Ld"
    (int64_of_attributes_kind ProcDefined)

let find ~defined pname_blob =
  let select_stmt = if defined then get_select_defined_statement () else get_select_statement () in
  Sqlite3.bind select_stmt 1 pname_blob
  |> SqliteUtils.check_sqlite_error ~log:"find bind proc name" ;
  SqliteUtils.sqlite_result_step ~finalize:false ~log:"Attributes.find" select_stmt
  |> Option.map ~f:Data.to_proc_attr

let load pname = Data.of_pname pname |> find ~defined:false

let store (attr: ProcAttributes.t) =
  let pkind = proc_kind_of_attr attr in
  let key = Data.of_pname attr.proc_name in
  if should_try_to_update key pkind then
    replace key pkind (Data.of_source_file attr.loc.Location.file) (Data.of_proc_attr attr)

let load_defined pname = Data.of_pname pname |> find ~defined:true

let get_correct_type_from_objc_class_name type_name =
  (* ToDo: this function should return a type that includes a reference to the tenv computed by:
     let class_method = Typ.Procname.get_default_objc_class_method (Typ.Name.name type_name);
     switch (find_tenv_from_class_of_proc class_method) {
     | Some tenv =>
    *)
  Some (Typ.mk (Tstruct type_name))

let find_file_capturing_procedure pname =
  match load pname with
  | None
   -> None
  | Some proc_attributes
   -> let source_file = proc_attributes.ProcAttributes.source_file_captured in
      let source_dir = DB.source_dir_from_source_file source_file in
      let origin =
        (* Procedure coming from include files if it has different location
         than the file where it was captured. *)
        match SourceFile.compare source_file proc_attributes.ProcAttributes.loc.file <> 0 with
        | true
         -> `Include
        | false
         -> `Source
      in
      let cfg_fname = DB.source_dir_get_internal_file source_dir ".cfg" in
      let cfg_fname_exists =
        PVariant.( = ) `Yes (Sys.file_exists (DB.filename_to_string cfg_fname))
      in
      if cfg_fname_exists then Some (source_file, origin) else None
