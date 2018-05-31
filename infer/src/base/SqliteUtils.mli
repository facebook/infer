(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The functions in this module tend to raise more often than their counterparts in [Sqlite3]. In particular, they may raise if the [Sqlite3.Rc.t] result of certain operations is unexpected. *)
exception Error of string

val check_sqlite_error : ?fatal:bool -> Sqlite3.db -> log:string -> Sqlite3.Rc.t -> unit
(** Assert that the result is either [Sqlite3.Rc.OK]. If [row_is_ok] then [Sqlite3.Rc.ROW] is also accepted. If the result is not valid, then if [fatal] is set raise [Error], otherwise log the error and proceed. *)

val exec : Sqlite3.db -> log:string -> stmt:string -> unit
(** Execute the given Sqlite [stmt] and asserts that it resulted in [Sqlite3.Rc.OK]. Otherwise, fail similarly to [check_sqlite_error ~fatal:true]. *)

val finalize : Sqlite3.db -> log:string -> Sqlite3.stmt -> unit
(** Finalize the given [stmt]. Raises [Error] on failure. *)

val sqlite_result_rev_list_step :
  ?finalize:bool -> Sqlite3.db -> log:string -> Sqlite3.stmt -> Sqlite3.Data.t option list
(** Return a reversed list of results obtained by repeatedly stepping through [stmt] and saving only column 0 of each returned row (all that's been needed so far). *)

val sqlite_result_step :
  ?finalize:bool -> Sqlite3.db -> log:string -> Sqlite3.stmt -> Sqlite3.Data.t option
(** Same as [sqlite_result_rev_list_step] but asserts that at most one result is returned. *)

val sqlite_unit_step : ?finalize:bool -> Sqlite3.db -> log:string -> Sqlite3.stmt -> unit
(** Same as [sqlite_result_rev_list_step] but asserts that no result is returned. *)

val db_close : Sqlite3.db -> unit
(** Close the given database and asserts that it was effective. Raises [Error] if not. *)

(** An API commonly needed to store and retrieve objects from the database *)
module type Data = sig
  type t

  val serialize : t -> Sqlite3.Data.t

  val deserialize : Sqlite3.Data.t -> t
end

(** A default implementation of the Data API that encodes every objects as marshalled blobs *)
module MarshalledData (D : sig
  type t
end) :
  Data with type t = D.t
