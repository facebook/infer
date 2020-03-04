(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

(** map entry for source files with potential basename collision within the same compiler call *)
type file_entry = Singleton of SourceFile.t | Duplicate of (string * SourceFile.t) list

type t = string * file_entry String.Map.t * JBasics.ClassSet.t

val load_from_verbose_output : string -> t
(** load the list of source files and the list of classes from the javac verbose file *)

val load_from_arguments : string -> t
(** load the list of source files and the list of classes from Config.generated_classes *)

type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t

type program

val get_classmap : program -> classmap

val mem_classmap : JBasics.class_name -> program -> bool

val get_models : program -> classmap

val cleanup : program -> unit

val load_program : string -> JBasics.ClassSet.t -> program
(** load a java program *)

val lookup_node : JBasics.class_name -> program -> JCode.jcode Javalib.interface_or_class option
(** retrieve a Java node from the classname *)

val add_missing_callee :
  program -> Procname.t -> JBasics.class_name -> JBasics.method_signature -> unit
(** add the class name of method signature to the list of callees *)

val set_callee_translated : program -> Procname.t -> unit
(** set that the CFG for the procedure has been created *)

val iter_missing_callees :
  program -> f:(Procname.t -> JBasics.class_name -> JBasics.method_signature -> unit) -> unit
