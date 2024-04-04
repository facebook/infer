(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : prog:string -> args:string list -> unit

val location_of_class_db : Sqlite3.db -> class_name:string -> Sqlite3.Rc.t * string list
[@@warning "-unused-value-declaration"]

val location_of_class : naming_table:string -> class_name:string -> Sqlite3.Rc.t * string list
[@@warning "-unused-value-declaration"]
