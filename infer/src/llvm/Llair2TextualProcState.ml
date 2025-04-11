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

type structMap = Textual.Struct.t Textual.TypeName.Map.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: Textual.Typ.annotated VarMap.t
  ; mutable ids: Textual.Typ.annotated IdentMap.t
  ; struct_map: structMap }

let pp_ids fmt current_ids =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.Ident.pp ~snd:Textual.Typ.pp_annotated))
    (IdentMap.bindings current_ids)


let pp_vars fmt vars =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.VarName.pp ~snd:Textual.Typ.pp_annotated))
    (VarMap.bindings vars)


let pp_struct_map fmt struct_map =
  F.fprintf fmt "%a"
    (Pp.comma_seq (Pp.pair ~fst:Textual.TypeName.pp ~snd:Textual.Struct.pp))
    (Textual.TypeName.Map.bindings struct_map)


let pp fmt proc_state =
  F.fprintf fmt
    "@[<v>@[<v>qualified_name: %a@]@;\
     @[loc: %a@]@;\
     @[locals: %a@]@;\
     @[formals: %a@]@;\
     @[ids: %a@]@@[types: %a@]@;\
     ]@]"
    Textual.QualifiedProcName.pp proc_state.qualified_name Textual.Location.pp proc_state.loc
    pp_vars proc_state.locals pp_vars proc_state.formals pp_ids proc_state.ids pp_struct_map
    proc_state.struct_map


let update_locals ~proc_state varname typ =
  proc_state.locals <- VarMap.add varname typ proc_state.locals


let update_formals ~proc_state varname typ =
  proc_state.formals <- VarMap.add varname typ proc_state.formals


let update_ids ~proc_state id typ = proc_state.ids <- IdentMap.add id typ proc_state.ids

type typ_modif = NoModif | PtrModif | RemovePtrModif

let update_local_or_formal_type ~(proc_state : t) ~typ_modif exp typ =
  match exp with
  | Textual.Exp.Lvar var_name when VarMap.mem var_name proc_state.locals ->
      let typ = Textual.Typ.mk_without_attributes typ in
      update_locals ~proc_state var_name typ
  | Textual.Exp.Lvar var_name when VarMap.mem var_name proc_state.formals ->
      let typ = Textual.Typ.mk_without_attributes typ in
      update_formals ~proc_state var_name typ
  | Textual.Exp.Var id when IdentMap.mem id proc_state.ids ->
      let new_typ =
        match typ_modif with
        | NoModif ->
            typ
        | PtrModif ->
            Textual.Typ.Ptr typ
        | RemovePtrModif -> (
          match typ with Textual.Typ.Ptr typ -> typ | _ -> typ )
      in
      update_ids ~proc_state id (Textual.Typ.mk_without_attributes new_typ)
  | _ ->
      ()


let get_local_or_formal_type ~(proc_state : t) exp =
  match exp with
  | Textual.Exp.Lvar var_name -> (
    match VarMap.find_opt var_name proc_state.locals with
    | Some typ ->
        Some typ
    | None ->
        VarMap.find_opt var_name proc_state.formals )
  | Textual.Exp.Var id ->
      IdentMap.find_opt id proc_state.ids
  | _ ->
      None
