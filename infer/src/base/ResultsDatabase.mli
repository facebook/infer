(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val database_filename : string
(** the relative path to the database from the results directory *)

val database_fullpath : string
(** the absolute path to the database file *)

val schema_hum : string
(**  some human-readable string describing the tables *)

val get_database : unit -> Sqlite3.db
(** The results database. You should always use this function to access the database, as the connection to it may change during the execution (see [new_database_connection]). *)

val reset_capture_tables : unit -> unit
(** zero out the tables associated with capture data *)

val new_database_connection : unit -> unit
(** Closes the previous connection to the database (if any), and opens a new one. Needed after calls to fork(2). *)

val db_canonicalize : unit -> unit
(** put the database on disk in deterministic form *)

val db_close : unit -> unit
(** close the current connection to the database *)

val create_db : unit -> unit
(** create the database file and initialize all the necessary tables *)

type registered_stmt

val register_statement : ('a, unit, string, registered_stmt) Base.format4 -> 'a
(** Return a function unit -> Sqlite3.stmt that can be called (once the DB has been initialized) to
    get the prepared statement corresponding to the current DB connection. Use this to prepare
    statements only once per DB connection.

    In particular, clients of this need not worry about calling [Sqlite3.finalize] on the returned
    statement, or about generating new statements when the connection to the DB changes: this is all
    handled internally. *)

val with_registered_statement : registered_stmt -> f:(Sqlite3.db -> Sqlite3.stmt -> 'a) -> 'a
