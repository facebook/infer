(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

type classmap = JCode.jcode Javalib.interface_or_class JBasics.ClassMap.t

type t

val get_classmap : t -> classmap

val set_java_location : t -> JBasics.class_name -> Location.t -> unit

val get_java_location : t -> JBasics.class_name -> Location.t option

val mem_classmap : JBasics.class_name -> t -> bool

val load : JClasspath.t -> t
(** load a java program *)

val lookup_node : JBasics.class_name -> t -> JCode.jcode Javalib.interface_or_class option
(** retrieve a Java node from the classname *)

val add_missing_callee : t -> Procname.t -> JBasics.class_name -> JBasics.method_signature -> unit
(** add the class name of method signature to the list of callees *)

val set_callee_translated : t -> Procname.t -> unit
(** set that the CFG for the procedure has been created *)

val iter_missing_callees :
  t -> f:(Procname.t -> JBasics.class_name -> JBasics.method_signature -> unit) -> unit
