(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

open Javalib_pack

(** Jar file containing the models *)
val models_jar : string ref

(** Type environment of the models *)
val models_tenv : Tenv.t ref

(**  Adds the set of procnames for the models of Java libraries so that methods
     with similar names are skipped during the capture *)
val add_models : string -> unit

(** Check if there is a model for the given procname *)
val is_model : Procname.t -> bool

(** create a source file from an absolute path.
    Source files are relative if the project root is specified and absolute otherwise *)
val java_source_file_from_path : string -> DB.source_file

val split_classpath : string -> string list

(** map entry for source files with potential basname collision within the same compiler call *)
type file_entry =
  | Singleton of DB.source_file
  | Duplicate of (string * DB.source_file) list

(** load the list of source files and the list of classes from the javac verbose file *)
val load_sources_and_classes : unit ->
  string * file_entry StringMap.t * JBasics.ClassSet.t

type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t

type program

val get_classmap : program -> classmap

val get_models : program -> classmap

val cleanup : program -> unit

(** load a java program *)
val load_program : string -> JBasics.ClassSet.t -> program

(** retrive a Java node from the classname *)
val lookup_node : JBasics.class_name -> program -> JCode.jcode Javalib.interface_or_class option

(** [collect_classes cmap filename] adds to [cmap] the classes found in the jar file [filename] *)
val collect_classes : classmap -> string -> classmap
