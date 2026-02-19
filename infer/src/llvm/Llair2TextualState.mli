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

val get_element_ptr_offset_prefix : string

val get_fresh_fake_line : unit -> int

type struct_map = Textual.Struct.t Textual.TypeName.Map.t

type globals_map = Llair.GlobalDefn.t Textual.VarName.Map.t

type proc_map = Textual.ProcDecl.t Textual.QualifiedProcName.Map.t

type mangled_map = Textual.TypeName.t IString.Map.t

type plain_map = Textual.TypeName.t IString.Map.t

type method_class_index = Textual.TypeName.t Textual.ProcName.Hashtbl.t

module ClassNameOffset : sig
  type t = {class_name: Textual.TypeName.t; offset: int}
end

module ClassNameOffsetMap : Stdlib.Hashtbl.S with type key = ClassNameOffset.t

type class_name_offset_map = Textual.QualifiedProcName.t ClassNameOffsetMap.t

module FieldOffset : sig
  type t = {class_name: Textual.TypeName.t; offset: int}
end

module FieldOffsetMap : Stdlib.Hashtbl.S with type key = FieldOffset.t

type field_offset_map = Textual.FieldName.t FieldOffsetMap.t

module ClassMethodIndex : sig
  type t = (Textual.QualifiedProcName.t * int) list Textual.TypeName.Hashtbl.t

  val pp : F.formatter -> t -> unit

  val fill_class_name_offset_map : t -> class_name_offset_map
end

module ModuleState : sig
  type t = private
    { functions: (Llair.FuncName.t * Llair.func) list
    ; struct_map: struct_map
    ; mangled_map: mangled_map
    ; plain_map: plain_map
    ; proc_decls: Textual.ProcDecl.t list
    ; proc_map: proc_map
    ; globals_map: globals_map
    ; lang: Textual.Lang.t
    ; method_class_index: method_class_index
    ; class_name_offset_map: class_name_offset_map
    ; field_offset_map: field_offset_map }

  val init :
       functions:(Llair.FuncName.t * Llair.func) list
    -> struct_map:struct_map
    -> mangled_map:mangled_map
    -> plain_map:plain_map
    -> proc_decls:Textual.ProcDecl.t list
    -> proc_map:proc_map
    -> globals_map:globals_map
    -> lang:Textual.Lang.t
    -> method_class_index:method_class_index
    -> class_name_offset_map:class_name_offset_map
    -> field_offset_map:field_offset_map
    -> t
end

module ProcState : sig
  type id_data = {typ: Textual.Typ.annotated option; loaded_var: bool; deref_needed: bool}

  type read = Read | NotRead

  type formal_data = {typ: Textual.Typ.annotated; assoc_local: Textual.VarName.t option; read: read}

  type t = private
    { qualified_name: Textual.QualifiedProcName.t
    ; sourcefile: SourceFile.t
    ; loc: Textual.Location.t
    ; mutable locals: Textual.Typ.annotated VarMap.t
    ; mutable formals: formal_data VarMap.t
    ; mutable local_map: Textual.Typ.t Textual.VarName.Hashtbl.t
    ; mutable ids_move: id_data IdentMap.t
    ; mutable ids_types: Textual.Typ.annotated IdentMap.t
    ; mutable id_offset: (Textual.Ident.t * int) option
    ; mutable get_element_ptr_offset: (Textual.VarName.t * int) option
    ; mutable reg_map: Textual.Ident.t RegMap.t
    ; mutable last_id: Textual.Ident.t
    ; mutable last_tmp_var: int
    ; mutable metadata_ids: Textual.Ident.Set.t
    ; module_state: ModuleState.t }

  val init :
       qualified_name:Textual.QualifiedProcName.t
    -> sourcefile:SourceFile.t
    -> loc:Textual.Location.t
    -> formals:formal_data VarMap.t
    -> module_state:ModuleState.t
    -> t

  val mk_fresh_id : ?reg:Llair.Reg.t -> t -> IdentMap.key

  val mk_fresh_tmp_var : string -> t -> VarMap.key

  val update_locals : proc_state:t -> VarMap.key -> Textual.Typ.annotated -> unit

  val update_formals :
    proc_state:t -> VarMap.key -> Textual.Typ.annotated * VarMap.key option -> read -> unit

  val update_ids_move :
       proc_state:t
    -> IdentMap.key
    -> Textual.Typ.annotated option
    -> loaded_var:bool
    -> deref_needed:bool
    -> unit

  val update_ids_types : proc_state:t -> IdentMap.key -> Textual.Typ.annotated -> unit

  val update_id_offset : proc_state:t -> IdentMap.key -> Textual.Exp.t -> unit

  val update_var_offset : proc_state:t -> VarMap.key -> int -> unit

  val subst_formal_local :
    proc_state:t -> formal:VarMap.key -> local:VarMap.key * Textual.Typ.annotated -> unit

  val reset_offsets : proc_state:t -> unit

  val pp : F.formatter -> print_types:bool -> t -> unit

  val global_proc_state : SourceFile.t -> Textual.Location.t -> ModuleState.t -> string -> t

  val find_method_with_offset :
    proc_state:t -> Textual.TypeName.t -> int -> Textual.QualifiedProcName.t option

  val compute_locals : proc_state:t -> (VarMap.key * Textual.Typ.annotated) list

  val mark_as_metadata : proc_state:t -> Textual.Ident.t -> unit

  val is_metadata_id : proc_state:t -> Textual.Ident.t -> bool
end
