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

module type BlobInternal = sig
  module Table : Table

  type key_blob = Sqlite3.Data.t

  type value_blob = Sqlite3.Data.t

  val blob_of_key : Table.key -> key_blob

  val blob_of_value : Table.value -> value_blob

  val value_of_blob : value_blob -> Table.value option
end

module type S = sig
  module Table : Table

  type key_blob

  type value_blob

  val blob_of_key : Table.key -> key_blob

  val blob_of_value : Table.value -> value_blob

  external key_blob_of_data : Sqlite3.Data.t -> key_blob = "%identity"

  external value_blob_of_data : Sqlite3.Data.t -> value_blob = "%identity"

  val value_of_blob : value_blob -> Table.value option

  val replace : key_blob -> value_blob -> unit

  val find : key_blob -> Table.value option

  val delete : key_blob -> unit
end

(* The functor is mostly here to provide a modicum of type safety around blobing/unblobing *)
module Make (Table : Table) : S with module Table = Table = struct
  module Unsafe : BlobInternal with module Table = Table = struct
    module Table = Table

    type key_blob = Sqlite3.Data.t

    type value_blob = Sqlite3.Data.t

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

    let value_of_blob = unblob
  end

  (* cannot mix, e.g., blob_key and blob_value now *)
  include Unsafe

  external key_blob_of_data : Sqlite3.Data.t -> key_blob = "%identity"

  external value_blob_of_data : Sqlite3.Data.t -> value_blob = "%identity"

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

  let replace key_blob value_blob =
    let replace_stmt = get_replace_statement () in
    Sqlite3.bind replace_stmt 1 key_blob |> SqliteUtils.check_sqlite_error ~log:"replace bind key" ;
    Sqlite3.bind replace_stmt 2 value_blob
    |> SqliteUtils.check_sqlite_error ~log:"replace bind value" ;
    SqliteUtils.sqlite_unit_step ~finalize:false ~log:"KeyValue.replace" replace_stmt

  let get_select_statement = register_statement "SELECT value FROM %s WHERE key = :k" Table.table

  let find key_blob =
    let select_stmt = get_select_statement () in
    Sqlite3.bind select_stmt 1 key_blob |> SqliteUtils.check_sqlite_error ~log:"insert bind key" ;
    SqliteUtils.sqlite_result_step ~finalize:false ~log:"KeyValue.find" select_stmt
    |> Option.bind ~f:value_of_blob

  let get_delete_statement = register_statement "DELETE FROM %s WHERE key = :k" Table.table

  let delete key_blob =
    let delete_stmt = get_delete_statement () in
    Sqlite3.bind delete_stmt 1 key_blob |> SqliteUtils.check_sqlite_error ~log:"delete bind key" ;
    SqliteUtils.sqlite_unit_step ~finalize:false ~log:"KeyValue.delete" delete_stmt
end
