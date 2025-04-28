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

type globalMap = Llair.GlobalDefn.t Textual.VarName.Map.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: Textual.Typ.annotated VarMap.t
  ; mutable ids: Textual.Typ.annotated IdentMap.t
  ; struct_map: structMap
  ; globals: globalMap }

val update_locals : proc_state:t -> VarMap.key -> Textual.Typ.annotated -> unit

val update_ids : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

type typ_modif = NoModif | PtrModif | RemovePtrModif

val update_local_or_formal_type :
  proc_state:t -> typ_modif:typ_modif -> Textual.Exp.t -> Textual.Typ.t -> unit

val get_local_or_formal_type : proc_state:t -> Textual.Exp.t -> Textual.Typ.annotated option

val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]
