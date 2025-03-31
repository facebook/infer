(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_compilation_database_files_xcodebuild :
  prog:string -> args:string list -> [> `Escaped of string] list
(** Get the compilation database files that contain the compilation given by the xcodebuild command,
    using xcpretty. *)

val capture :
     changed_files:SourceFile.Set.t option
  -> db_files:[< `Escaped of string | `Raw of string] list
  -> unit
(** Run the capture of the files for which we have compilation commands in [db_files] and
    [changed_files], if specified. *)
