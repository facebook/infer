(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type id =
  | CaptureDatabase  (** the capture databse **)
  | AnalysisDatabase  (** the analysis/summaries database **)

val schema_hum : string
(** some human-readable string describing the tables *)

val create_tables : ?prefix:string -> Sqlite3.db -> id -> unit

val get_database : id -> Sqlite3.db
(** You should always use this function to access the database, as the connection to it may change
    during the execution (see [new_database_connection]). *)

val new_database_connection : unit -> unit
(** Closes the previous connection to the database (if any), and opens a new one. Needed after calls
    to fork(2). *)

val db_close : unit -> unit
(** close the current connection to the database *)

val create_db : id -> unit
(** create the database file and initialize all the necessary tables *)

type registered_stmt

val register_statement : id -> ('a, unit, string, registered_stmt) Base.format4 -> 'a
(** Return a function unit -> Sqlite3.stmt that can be called (once the DB has been initialized) to
    get the prepared statement corresponding to the current DB connection. Use this to prepare
    statements only once per DB connection.

    In particular, clients of this need not worry about calling [Sqlite3.finalize] on the returned
    statement, or about generating new statements when the connection to the DB changes: this is all
    handled internally. *)

val with_registered_statement : registered_stmt -> f:(Sqlite3.db -> Sqlite3.stmt -> 'a) -> 'a
