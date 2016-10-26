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

(** [path_of_cached_classname cn] returns the path of a cached classname *)
val path_of_cached_classname : JBasics.class_name -> string

(** [cache_classname cn] stores the classname to the disk *)
val cache_classname : JBasics.class_name -> unit

(** [is_classname_cached cn]  *)
val is_classname_cached : JBasics.class_name -> bool

(** [compute_icfg linereader classes program tenv source_basename source_file] create the call graph and control flow graph for the file [source_file] by translating all the classes in [program] originating from [source_file] *)
val compute_source_icfg :
  Printer.LineReader.t ->
  JBasics.ClassSet.t ->
  JClasspath.program ->
  Tenv.t ->
  string ->
  string option ->
  DB.source_file ->
  Cg.t * Cfg.cfg

(** Compute the CFG for a class *)
val compute_class_icfg :
  DB.source_file ->
  Printer.LineReader.t ->
  JClasspath.program ->
  Tenv.t ->
  JCode.jcode Javalib.interface_or_class ->
  Cg.t * Cfg.cfg
