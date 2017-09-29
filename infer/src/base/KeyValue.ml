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

module type Table = sig
  type key

  type value

  val table : string
end

module type Blob = sig
  module Table : Table

  val blob_of_key : Table.key -> Sqlite3.Data.t

  val blob_of_value : Table.value -> Sqlite3.Data.t

  val key_of_blob : Sqlite3.Data.t -> Table.key option

  val value_of_blob : Sqlite3.Data.t -> Table.value option
end

module type S = sig
  include Blob

  val replace : Table.key -> Table.value -> unit

  val find : Table.key -> Table.value option

  val delete : Table.key -> unit
end

(* The functor is mostly here to provide a modicum of type safety around blobing/unblobing *)
module Make (Table : Table) : S with module Table = Table = struct
  module Unsafe : Blob with module Table = Table = struct
    module Table = Table

    let blob x = Sqlite3.Data.BLOB (Marshal.to_string x [])

    let unblob = function
      | Sqlite3.Data.BLOB b
       -> Some (Marshal.from_string b 0)
      | Sqlite3.Data.NULL
       -> None
      | _
       -> assert false

    let blob_of_key = blob

    let blob_of_value = blob

    let key_of_blob = unblob

    let value_of_blob = unblob
  end

  (* cannot mix, e.g., blob_key and blob_value now *)
  include Unsafe

  let register_statement stmt_fmt =
    let k stmt0 =
      let stmt_ref = ref None in
      let new_statement db =
        let stmt = Sqlite3.prepare db stmt0 in
        ResultsDir.on_close_database ~f:(fun _ ->
            Option.iter !stmt_ref ~f:(SqliteUtils.finalize ~log:"db close callback") ) ;
        stmt_ref := Some stmt
      in
      ResultsDir.on_new_database_connection ~f:new_statement ;
      fun () ->
        match !stmt_ref with
        | None
         -> L.(die InternalError) "database not initialized"
        | Some stmt
         -> Sqlite3.reset stmt |> SqliteUtils.check_sqlite_error ~log:"reset prepared statement" ;
            Sqlite3.clear_bindings stmt
            |> SqliteUtils.check_sqlite_error ~log:"clear bindings of prepared statement" ;
            stmt
    in
    Printf.ksprintf k stmt_fmt

  let get_replace_statement =
    register_statement "REPLACE INTO %s(key, value) VALUES(:k, :v)" Table.table

  let replace key value =
    let replace_stmt = get_replace_statement () in
    Sqlite3.bind replace_stmt 1 (blob_of_key key)
    |> SqliteUtils.check_sqlite_error ~log:"replace bind key" ;
    Sqlite3.bind replace_stmt 2 (blob_of_value value)
    |> SqliteUtils.check_sqlite_error ~log:"replace bind value" ;
    SqliteUtils.sqlite_unit_step ~finalize:false ~log:"KeyValue.replace" replace_stmt

  let get_select_statement = register_statement "SELECT value FROM %s WHERE key = :k" Table.table

  let find key =
    let select_stmt = get_select_statement () in
    Sqlite3.bind select_stmt 1 (blob_of_key key)
    |> SqliteUtils.check_sqlite_error ~log:"insert bind key" ;
    SqliteUtils.sqlite_result_step ~finalize:false ~log:"KeyValue.find" select_stmt
    |> Option.bind ~f:value_of_blob

  let get_delete_statement = register_statement "DELETE FROM %s WHERE key = :k" Table.table

  let delete key =
    let delete_stmt = get_delete_statement () in
    Sqlite3.bind delete_stmt 1 (blob_of_key key)
    |> SqliteUtils.check_sqlite_error ~log:"delete bind key" ;
    SqliteUtils.sqlite_unit_step ~finalize:false ~log:"KeyValue.delete" delete_stmt
end
