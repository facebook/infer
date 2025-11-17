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

type procMap = Textual.ProcDecl.t Textual.QualifiedProcName.Map.t

type classMethodIndex = (Textual.QualifiedProcName.t * int) list Textual.TypeName.Map.t ref

val class_method_index : classMethodIndex

type methodClassIndex = Textual.TypeName.t Textual.ProcName.Map.t ref

val method_class_index : methodClassIndex

module ClassNameOffset : sig
  type t = {class_name: Textual.TypeName.t; offset: int} [@@deriving compare]
end

module ClassNameOffsetMap : Stdlib.Map.S with type key = ClassNameOffset.t

type classNameOffsetMap = Textual.QualifiedProcName.t ClassNameOffsetMap.t

type t =
  { qualified_name: Textual.QualifiedProcName.t
  ; loc: Textual.Location.t
  ; mutable locals: Textual.Typ.annotated VarMap.t
  ; mutable formals: (Textual.Typ.annotated * Textual.VarName.t option) VarMap.t
  ; mutable ids_move: Textual.Typ.annotated IdentMap.t
  ; mutable ids_types: Textual.Typ.annotated IdentMap.t
  ; mutable id_offset: (Textual.Ident.t * int) option
  ; mutable get_element_ptr_offset: (Textual.VarName.t * int) option
  ; mutable reg_map: Textual.Ident.t RegMap.t
  ; mutable last_id: Textual.Ident.t
  ; mutable last_tmp_var: int
  ; struct_map: structMap
  ; globals: globalMap
  ; lang: Textual.Lang.t
  ; proc_map: procMap
  ; class_name_offset_map: classNameOffsetMap }

val get_element_ptr_offset_prefix : string

val mk_fresh_id : ?reg:Llair.Reg.t -> t -> IdentMap.key

val mk_fresh_tmp_var : string -> t -> VarMap.key

val update_locals : proc_state:t -> VarMap.key -> Textual.Typ.annotated -> unit

val update_ids_move : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

val update_ids_types : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

val update_id_offset : proc_state:t -> IdentMap.key -> Textual.Exp.t -> unit

val update_var_offset : proc_state:t -> VarMap.key -> int -> unit

val subst_formal_local : proc_state:t -> formal:VarMap.key -> local:VarMap.key -> unit

val reset_offsets : proc_state:t -> unit

val pp : F.formatter -> print_types:bool -> t -> unit [@@warning "-unused-value-declaration"]

val global_proc_state : Textual.Lang.t -> Textual.Location.t -> string -> t

val get_fresh_fake_line : unit -> int

val pp_struct_map : F.formatter -> Textual.Struct.t Textual.TypeName.Map.t -> unit
[@@warning "-unused-value-declaration"]

val find_method_with_offset :
  proc_state:t -> Textual.TypeName.t -> int -> Textual.QualifiedProcName.t option

val fill_class_name_offset_map :
  classMethodIndex -> Textual.QualifiedProcName.t ClassNameOffsetMap.t

val compute_locals : proc_state:t -> (VarMap.key * Textual.Typ.annotated) list
