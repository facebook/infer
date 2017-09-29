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

val attributes_table : string
(** the name of the table of proc names to their proc attributes  *)

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

val canonicalize_db : unit -> unit
(** put the database on disk in deterministic form *)

val on_new_database_connection : f:(Sqlite3.db -> unit) -> unit

val on_close_database : f:(Sqlite3.db -> unit) -> unit
