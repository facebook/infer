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

(** Data structure for storing the results of the translation of an instruction.   *)
type translation =
  | Skip
  | Instr of Cfg.Node.t
  | Prune of Cfg.Node.t * Cfg.Node.t
  | Loop of Cfg.Node.t * Cfg.Node.t * Cfg.Node.t

val is_java_native : JCode.jcode Javalib.concrete_method  -> bool

(** [create_procdesc linereader cfg tenv program m] creates a procedure description
    for the method m and adds it to cfg  *)
val create_procdesc :
  DB.source_file ->
  JClasspath.program ->
  Printer.LineReader.t ->
  JContext.icfg ->
  JCode.jcode Javalib.jmethod ->
  Cfg.Procdesc.t option

(** returns the implementation of a given method *)
val get_implementation : JCode.jcode Javalib.concrete_method -> JBir.t

(** translates an instruction into a statement node or prune nodes in the cfg *)
val instruction : JContext.t -> int -> JBir.instr -> translation

exception Frontend_error of string
