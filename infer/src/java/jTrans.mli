(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack
open Sawja_pack

(** Data structure for storing the results of the translation of an instruction.   *)
type translation =
  | Skip
  | Instr of Procdesc.Node.t
  | Prune of Procdesc.Node.t * Procdesc.Node.t
  | Loop of Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t

val is_java_native : JCode.jcode Javalib.concrete_method -> bool

val create_callee_attributes :
     Tenv.t
  -> JClasspath.program
  -> JBasics.class_name
  -> JBasics.method_signature
  -> Typ.Procname.t
  -> ProcAttributes.t option

val create_am_procdesc :
     SourceFile.t
  -> JClasspath.program
  -> JContext.icfg
  -> Javalib.abstract_method
  -> Typ.Procname.t
  -> Procdesc.t
(** Create the procedure description for an abstract method *)

val create_native_procdesc :
     SourceFile.t
  -> JClasspath.program
  -> JContext.icfg
  -> JCode.jcode Javalib.concrete_method
  -> Typ.Procname.t
  -> Procdesc.t
(** Create the procedure description for a concrete method *)

val create_empty_procdesc :
     SourceFile.t
  -> JClasspath.program
  -> Printer.LineReader.t
  -> JContext.icfg
  -> JCode.jcode Javalib.concrete_method
  -> Typ.Procname.t
  -> Procdesc.t

val create_cm_procdesc :
     SourceFile.t
  -> JClasspath.program
  -> Printer.LineReader.t
  -> JContext.icfg
  -> JCode.jcode Javalib.concrete_method
  -> Typ.Procname.t
  -> (Procdesc.t * Procdesc.Node.t * Procdesc.Node.t * Procdesc.Node.t * JBir.t) option
(** [create_procdesc source_file program linereader icfg cm proc_name] creates
    a procedure description for the concrete method cm and adds it to cfg *)

val instruction : JContext.t -> int -> JBir.instr -> translation
(** translates an instruction into a statement node or prune nodes in the cfg *)

exception Frontend_error of string
