(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module ModuleState = Llair2TextualState.ModuleState
module ProcState = Llair2TextualState.ProcState

val witness_protocol_suffix : string

val class_virtual_table_suffix : string

val build_globals_map : Llair.GlobalDefn.t NS.iarray -> Llair.GlobalDefn.t Textual.VarName.Map.t

val process_globals :
     Textual.Lang.t
  -> (Textual.QualifiedProcName.t * int) list Textual.TypeName.Hashtbl.t
  -> Textual.TypeName.t Textual.ProcName.Hashtbl.t
  -> mangled_map:Llair2TextualState.mangled_map
  -> struct_map:Llair2TextualState.struct_map
  -> Llair.GlobalDefn.t Llair2TextualState.VarMap.t
  -> Llair2TextualState.struct_map

val to_textual_global :
     f_to_textual_loc:(Llair.Loc.t -> Textual.Location.t)
  -> f_to_textual_exp:
       (   proc_state:ProcState.t
        -> Textual.Location.t
        -> Llair.Exp.t
        -> Textual.Exp.t * 'a * Textual.Instr.t list )
  -> module_state:ModuleState.t
  -> SourceFile.t
  -> Llair.GlobalDefn.t
  -> Textual.Global.t * Textual.Module.decl option

val translate_global :
     proc_state:ProcState.t
  -> lang:Textual.Lang.t
  -> struct_map:Textual.Struct.t Textual.TypeName.Map.t
  -> mangled_map:Llair2TextualState.mangled_map
  -> name:string
  -> typ:Llair.Typ.t
  -> Textual.Exp.t * Textual.Typ.t option * 'a list
