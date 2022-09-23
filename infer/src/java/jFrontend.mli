(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

val path_of_cached_classname : JBasics.class_name -> string
(** [path_of_cached_classname cn] returns the path of a cached classname *)

val cache_classname : JBasics.class_name -> unit
(** [cache_classname cn] stores the classname to the disk *)

val is_classname_cached : JBasics.class_name -> bool
(** [is_classname_cached cn] *)

val compute_source_icfg :
  JProgramDesc.t -> Tenv.t -> string -> string list option -> SourceFile.t -> Cfg.t
(** [compute_cfg linereader program tenv source_basename source_file] create the control flow graph
    for the file [source_file] by translating all the classes in [program] originating from
    [source_file] *)

val compute_class_icfg :
  SourceFile.t -> JProgramDesc.t -> Tenv.t -> JCode.jcode Javalib.interface_or_class -> Cfg.t
(** Compute the CFG for a class *)
