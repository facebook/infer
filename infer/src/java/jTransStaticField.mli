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


val is_static_final_field : JContext.t -> JBasics.class_name -> JBasics.field_signature -> bool

val has_static_final_fields : JCode.jcode Javalib.interface_or_class -> bool

val translate_instr_static_field : JContext.t -> Cfg.Procdesc.t -> JBasics.field_signature -> Sil.typ ->
  Location.t -> Ident.t list * Sil.instr list * Sil.exp


val static_field_init : JCode.jcode Javalib.interface_or_class -> JBasics.class_name -> JBir.instr array -> JBir.instr array
