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
module RegMap = Llair.Exp.Reg.Map

type structMap = Textual.Struct.t Textual.TypeName.Map.t

type globalMap = Llair.GlobalDefn.t Textual.VarName.Map.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: Textual.Typ.annotated VarMap.t
  ; mutable ids: Textual.Typ.annotated IdentMap.t
  ; mutable reg_map: Textual.Ident.t RegMap.t
  ; mutable last_id: Textual.Ident.t
  ; struct_map: structMap
  ; globals: globalMap
  ; lang: Textual.Lang.t }

val mk_fresh_id : ?reg:Llair.Reg.t -> t -> IdentMap.key

val update_locals : proc_state:t -> VarMap.key -> Textual.Typ.annotated -> unit

val update_ids : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

val pp : F.formatter -> print_types:bool -> t -> unit [@@warning "-unused-value-declaration"]

val global_proc_state : Textual.Lang.t -> Textual.Location.t -> string -> t

val get_fresh_fake_line : unit -> int
