(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module F = Format
module Type = Llair2TextualType
module TypeName = Llair2TextualTypeName
module Field = Llair2TextualField
module L = Logging
module State = Llair2TextualState
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState
module Globals = Llair2TextualGlobals
module Proc = Llair2TextualProc
module VarMap = Textual.VarName.Map

val reg_to_annot_typ :
  Textual.Lang.t -> struct_map:State.struct_map -> Reg.t -> Textual.Typ.annotated

val reg_to_textual_var : proc_state:ProcState.t -> Reg.t -> Textual.Exp.t * Textual.Typ.t option

val add_fresh_id : proc_state:ProcState.t -> unit -> Textual.Ident.t

val find_formal_type : proc_state:ProcState.t -> VarMap.key -> Textual.Typ.t option

val reg_to_id : proc_state:ProcState.t -> Reg.t -> Textual.Ident.t * Textual.Typ.t

val reg_to_var_name : Reg.t -> VarMap.key
