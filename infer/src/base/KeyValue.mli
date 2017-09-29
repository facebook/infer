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

  val blob_of_key : Table.key -> Sqlite3.Data.t
  (** turn a key into a [Sqlite3.Data.BLOB] *)

  val blob_of_value : Table.value -> Sqlite3.Data.t
  (** turn a value into a [Sqlite3.Data.BLOB] *)

  val key_of_blob : Sqlite3.Data.t -> Table.key option
  (** turn a [Sqlite3.Data.BLOB] (or [Sqlite3.Data.NULL]) back into a key *)

  val value_of_blob : Sqlite3.Data.t -> Table.value option
  (** turn a [Sqlite3.Data.BLOB] (or [Sqlite3.Data.NULL]) back into a value *)

  val replace : Table.key -> Table.value -> unit

  val find : Table.key -> Table.value option

  val delete : Table.key -> unit
end

module Make (Table : Table) : S with module Table = Table
