(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

val database_filename : string
(** the relative path to the database from the results directory *)

val get_database : unit -> Sqlite3.db
(** The results database. You should always use this function to access the database, as the connection to it may change during the execution (see [new_database_connection]). *)

val reset_attributes_table : unit -> unit
(** zero out the attributes table *)

val assert_results_dir : string -> unit
(** Check that the results dir exists and sets up logging, the database, etc. *)

val remove_results_dir : unit -> unit
(** Recursively delete the results directory. *)

val create_results_dir : unit -> unit
(** Create the results dir and sets up logging, the database, etc. *)

val new_database_connection : unit -> unit
(** Closes the previous connection to the database (if any), and opens a new one. Needed after calls to fork(2). *)

val delete_capture_and_analysis_data : unit -> unit
(** delete all results from the capture and the analysis *)

val db_canonicalize : unit -> unit
(** put the database on disk in deterministic form *)

val db_close : unit -> unit
(** close the current connection to the database *)

val register_statement : ('a, unit, string, (unit -> Sqlite3.stmt)) Base.format4 -> 'a
(** Return a function unit -> Sqlite3.stmt that can be called (once the DB has been initialized) to
    get the prepared statement corresponding to the current DB connection. Use this to prepare
    statements only once per DB connection.

    In particular, clients of this need not worry about calling [Sqlite3.finalize] on the returned
    statement, or about generating new statements when the connection to the DB changes: this is all
    handled internally. *)
