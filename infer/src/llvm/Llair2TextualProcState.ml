(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module VarMap = Textual.VarName.Map
module IdentMap = Textual.Ident.Map

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: Textual.Typ.annotated VarMap.t
  ; mutable ids: Textual.Typ.annotated IdentMap.t }

let pp_ids fmt current_ids =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.Ident.pp ~snd:Textual.Typ.pp_annotated))
    (IdentMap.bindings current_ids)


let update_locals ~proc_state varname typ =
  proc_state.locals <- VarMap.add varname typ proc_state.locals


let update_formals ~proc_state varname typ =
  proc_state.formals <- VarMap.add varname typ proc_state.formals


let update_ids ~proc_state id typ = proc_state.ids <- IdentMap.add id typ proc_state.ids

let update_local_or_formal_type ~(proc_state : t) exp typ =
  match exp with
  | Textual.Exp.Lvar var_name when VarMap.mem var_name proc_state.locals ->
      let typ = Textual.Typ.mk_without_attributes typ in
      update_locals ~proc_state var_name typ
  | Textual.Exp.Lvar var_name when VarMap.mem var_name proc_state.formals ->
      let typ = Textual.Typ.mk_without_attributes typ in
      update_formals ~proc_state var_name typ
  | Textual.Exp.Var id when IdentMap.mem id proc_state.ids ->
      let new_ptr_typ = Textual.Typ.Ptr typ in
      update_ids ~proc_state id (Textual.Typ.mk_without_attributes new_ptr_typ)
  | _ ->
      ()
