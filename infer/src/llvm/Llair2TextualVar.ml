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

let string_name_of_reg reg =
  let name = Reg.name reg in
  match Int.of_string_opt name with Some i -> Format.sprintf "var%d" (i + 1) | None -> name


let reg_to_var_name reg =
  let id = Reg.id reg in
  let mangled = Mangled.mangled (string_name_of_reg reg) (Int.to_string id) in
  Textual.VarName.of_mangled mangled


let reg_to_id ~(proc_state : ProcState.t) reg =
  let id = ProcState.mk_fresh_id ~reg proc_state in
  let reg_typ =
    Type.to_textual_typ proc_state.module_state.lang
      ~mangled_map:proc_state.module_state.mangled_map
      ~struct_map:proc_state.module_state.struct_map (Reg.typ reg)
  in
  (id, reg_typ)


let add_fresh_id ~proc_state () = ProcState.mk_fresh_id proc_state

let find_formal_type ~(proc_state : ProcState.t) reg_var_name =
  Textual.VarName.Hashtbl.find_opt proc_state.local_map reg_var_name


let reg_to_textual_var ~(proc_state : ProcState.t) reg =
  let reg_var_name = reg_to_var_name reg in
  match VarMap.find_opt reg_var_name proc_state.formals with
  | Some {typ= annot_typ; assoc_local= Some local} ->
      (Textual.Exp.Lvar local, Some annot_typ.Textual.Typ.typ)
  | Some {typ= annot_typ; assoc_local= None} ->
      ProcState.update_formals ~proc_state reg_var_name (annot_typ, None) ProcState.Read ;
      (Textual.Exp.Lvar reg_var_name, Some annot_typ.Textual.Typ.typ)
  | None when VarMap.mem reg_var_name proc_state.locals ->
      let typ = find_formal_type ~proc_state reg_var_name in
      let typ =
        if Option.is_some typ then typ
        else
          let annot_typ_opt = VarMap.find_opt reg_var_name proc_state.locals in
          Option.map ~f:(fun Textual.Typ.{typ} -> typ) annot_typ_opt
      in
      (Textual.Exp.Lvar reg_var_name, typ)
  | None ->
      (Textual.Exp.Var (reg_to_id ~proc_state reg |> fst), None)


let reg_to_annot_typ lang ~struct_map reg =
  Type.to_annotated_textual_typ_without_mangled_map lang ~struct_map (Reg.typ reg)
