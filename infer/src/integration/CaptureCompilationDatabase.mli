(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** capture_files_in_database file runs the capture of the files for which
    we have compilation commands in the database. If the option changed-files-index
    is passed, we only capture the files there *)
val capture_files_in_database : CompilationDatabase.t -> unit

val capture_file_in_database : CompilationDatabase.t -> SourceFile.t -> unit

(** Get the compilation database files that contain the compilation given by the
    buck command. It will be the compilation of the passed targets only or also
    the dependencies according to the flag --buck-compilation-database deps | no-deps *)
val get_compilation_database_files_buck : unit -> [> `Raw of string ] list

(** Get the compilation database files that contain the compilation given by the
    xcodebuild command, using xcpretty. *)
val get_compilation_database_files_xcodebuild : unit -> [> `Escaped of string ] list
