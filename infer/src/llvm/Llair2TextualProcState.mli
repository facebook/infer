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

val pp_ids : F.formatter -> Textual.Typ.annotated IdentMap.t -> unit
[@@warning "-unused-value-declaration"]

val update_locals : proc_state:t -> VarMap.key -> Textual.Typ.annotated -> unit

val update_ids : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

val update_local_or_formal_type : proc_state:t -> Textual.Exp.t -> Textual.Typ.t -> unit
