(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

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

  let replace =
    let replace_statement =
      Printf.sprintf "REPLACE INTO %s(key, value) VALUES(:k, :v)" Table.table
    in
    fun key value ->
      let db = ResultsDir.get_database () in
      let replace_stmt = Sqlite3.prepare db replace_statement in
      SqliteUtils.check_sqlite_error ~log:"replace bind key"
        (Sqlite3.bind replace_stmt 1 (blob_of_key key)) ;
      SqliteUtils.check_sqlite_error ~log:"replace bind value"
        (Sqlite3.bind replace_stmt 2 (blob_of_value value)) ;
      SqliteUtils.sqlite_unit_step ~log:"KeyValue.replace" replace_stmt

  let find =
    let select_statement = Printf.sprintf "SELECT value FROM %s WHERE key = :k" Table.table in
    fun key ->
      let db = ResultsDir.get_database () in
      let select_stmt = Sqlite3.prepare db select_statement in
      SqliteUtils.check_sqlite_error ~log:"insert bind key"
        (Sqlite3.bind select_stmt 1 (blob_of_key key)) ;
      Option.bind ~f:value_of_blob
        (SqliteUtils.sqlite_result_step ~log:"KeyValue.find" select_stmt)

  let delete =
    let delete_statement = Printf.sprintf "DELETE FROM %s WHERE key = :k" Table.table in
    fun key ->
      let db = ResultsDir.get_database () in
      let delete_stmt = Sqlite3.prepare db delete_statement in
      SqliteUtils.check_sqlite_error ~log:"delete bind key"
        (Sqlite3.bind delete_stmt 1 (blob_of_key key)) ;
      SqliteUtils.sqlite_unit_step ~log:"KeyValue.delete" delete_stmt
end
