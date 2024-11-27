(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** The functions in this module tend to raise more often than their counterparts in [Sqlite3]. In
    particular, they may raise if the [Sqlite3.Rc.t] result of certain operations is unexpected. *)
exception Error of string

(** Raised from serializers when final size exceeds Sqlite3 limits (normally 1000_000_000 bytes). *)
exception DataTooBig

val check_result_code : Sqlite3.db -> log:string -> Sqlite3.Rc.t -> unit
(** Assert that the result is either [Sqlite3.Rc.OK] or [Sqlite3.Rc.ROW]. If the result is not
    valid, raise {!Error}. *)

val exec : Sqlite3.db -> log:string -> stmt:string -> unit
(** Execute the given Sqlite [stmt] and check the result with {!check_result_code}. *)

val finalize : Sqlite3.db -> log:string -> Sqlite3.stmt -> unit
(** Finalize the given [stmt]. Raises {!Error} on failure. *)

val result_fold_rows :
     ?finalize:bool
  -> Sqlite3.db
  -> log:string
  -> Sqlite3.stmt
  -> init:'a
  -> f:('a -> Sqlite3.stmt -> 'a)
  -> 'a
(** Fold [f] over each row of the result. [f] must not access the database. *)

val result_fold_single_column_rows :
     ?finalize:bool
  -> Sqlite3.db
  -> log:string
  -> Sqlite3.stmt
  -> init:'b
  -> f:('b -> Sqlite3.Data.t -> 'b)
  -> 'b
(** Like {!result_fold_rows} but pass column 0 of each row in the results to [f]. *)

val result_option :
     ?finalize:bool
  -> Sqlite3.db
  -> log:string
  -> read_row:(Sqlite3.stmt -> 'a)
  -> Sqlite3.stmt
  -> 'a option
(** Same as {!result_fold_rows} but asserts that at most one row is returned. *)

val result_single_column_option :
  ?finalize:bool -> Sqlite3.db -> log:string -> Sqlite3.stmt -> Sqlite3.Data.t option
(** Same as {!result_fold_single_column_rows} but asserts that at most one row is returned. *)

val result_unit : ?finalize:bool -> Sqlite3.db -> log:string -> Sqlite3.stmt -> unit
(** Same as {!result_fold_rows} but asserts that no row is returned. *)

val db_close : Sqlite3.db -> unit
(** Close the given database and asserts that it was effective. Raises {!Error} if not. *)

val with_attached_db :
  db_file:string -> db_name:string -> ?immutable:bool -> f:(unit -> 'a) -> Sqlite3.db -> 'a
(** Attach the given [db_file] as [db_name], execute [f], then detach. If [immutable=true], attach
    the database with the [immutable] Sqlite flag. See [https://www.sqlite.org/uri.html] for more
    info. *)

val transaction : ?immediate:bool -> Sqlite3.db -> f:(unit -> 'a) -> 'a
(** Run [f] as a transaction. *)

(** An API commonly needed to store and retrieve objects from the database *)
module type Data = sig
  type t

  val serialize : t -> Sqlite3.Data.t

  val deserialize : Sqlite3.Data.t -> t
end

(** A default implementation of the Data API that encodes every objects as marshalled blobs *)
module MarshalledDataNOTForComparison (D : sig
  type t
end) : Data with type t = D.t

(** A default implementation of the Data API that encodes None as a NULL SQLite value *)
module MarshalledNullableDataNOTForComparison (D : sig
  type t
end) : Data with type t = D.t option
