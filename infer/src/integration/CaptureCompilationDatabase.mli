(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture_files_in_database :
  changed_files:SourceFile.Set.t option -> CompilationDatabase.t -> unit
(** Run the capture of the files for which we have compilation commands in the database and in
    [changed_files], if specified. *)

val capture_file_in_database : CompilationDatabase.t -> SourceFile.t -> unit

val get_compilation_database_files_buck :
  prog:string -> args:string list -> [> `Raw of string] list
(** Get the compilation database files that contain the compilation given by the
    buck command. It will be the compilation of the passed targets only or also
    the dependencies according to the flag --buck-compilation-database deps | no-deps *)

val get_compilation_database_files_xcodebuild :
  prog:string -> args:string list -> [> `Escaped of string] list
(** Get the compilation database files that contain the compilation given by the
    xcodebuild command, using xcpretty. *)
