(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** key/value database information *)
module type Table = sig
  type key

  type value

  val table : string
end

(** Key/value store backed by the ResultsDir database *)
module type S = sig
  module Table : Table

  type key_blob

  type value_blob

  val blob_of_key : Table.key -> key_blob
  (** turn a key into a [Sqlite3.Data.BLOB] *)

  val blob_of_value : Table.value -> value_blob
  (** turn a value into a [Sqlite3.Data.BLOB] *)

  external key_blob_of_data : Sqlite3.Data.t -> key_blob = "%identity"

  external value_blob_of_data : Sqlite3.Data.t -> value_blob = "%identity"

  val value_of_blob : value_blob -> Table.value option

  val replace : key_blob -> value_blob -> unit

  val find : key_blob -> Table.value option

  val delete : key_blob -> unit
end

module Make (Table : Table) : S with module Table = Table
