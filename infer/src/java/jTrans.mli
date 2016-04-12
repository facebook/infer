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
open Sawja_pack

(** If active, disable special treatment of static final fields. *)
val no_static_final : bool ref

(** Data structure for storing the results of the translation of an instruction.   *)
type translation =
  | Skip
  | Instr of Cfg.Node.t
  | Prune of Cfg.Node.t * Cfg.Node.t
  | Loop of Cfg.Node.t * Cfg.Node.t * Cfg.Node.t

(** data structure to identify whether a method is defined in the given program  *)
type defined_status =
  | Defined of Cfg.Procdesc.t
  | Called of Cfg.Procdesc.t

(** returns the procedure description of the given method and creates it if it hasn't been created before *)
val get_method_procdesc : JClasspath.program -> Cfg.cfg -> Tenv.t -> JBasics.class_name ->
  JBasics.method_signature -> Procname.method_kind -> defined_status

(** [create_local_procdesc linereader cfg tenv program m] creates a procedure description for the method m and adds it to cfg  *)
val create_local_procdesc :
  JClasspath.program -> Printer.LineReader.t -> Cfg.cfg -> Tenv.t ->
  JCode.jcode Javalib.interface_or_class -> JCode.jcode Javalib.jmethod -> unit

(** returns the implementation of a given method *)
val get_implementation : JCode.jcode Javalib.concrete_method -> JBir.t

(** translates an instruction into a statement node or prune nodes in the cfg *)
val instruction : JContext.t -> int -> JBir.instr -> translation

exception Frontend_error of string
