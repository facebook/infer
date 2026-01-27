(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair
module ProcState = Llair2TextualState

val field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val tuple_field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val to_annotated_textual_typ :
     Textual.Lang.t
  -> mangled_map:ProcState.mangled_map
  -> struct_map:ProcState.struct_map
  -> Typ.t
  -> Textual.Typ.annotated

val to_annotated_textual_typ_without_mangled_map :
  Textual.Lang.t -> struct_map:ProcState.struct_map -> Typ.t -> Textual.Typ.annotated

val to_textual_typ :
     Textual.Lang.t
  -> mangled_map:ProcState.mangled_map
  -> struct_map:ProcState.struct_map
  -> Typ.t
  -> Textual.Typ.t

val to_textual_typ_without_mangled_map :
  Textual.Lang.t -> struct_map:ProcState.struct_map -> Typ.t -> Textual.Typ.t

val join_typ : Textual.Typ.t option -> Textual.Typ.t option -> Textual.Typ.t option

val is_compatible : Textual.Typ.t -> Textual.Typ.t -> bool

val translate_types_env : Textual.Lang.t -> Llair.Typ.t list -> ProcState.struct_map

val is_ptr_struct : Textual.Typ.t -> bool

val lookup_field_type :
     struct_map:ProcState.struct_map
  -> Textual.TypeName.t
  -> Textual.qualified_fieldname
  -> Textual.Typ.t option

val signature_type_to_textual_typ :
  string Hash_set.t -> Textual.Lang.t -> string -> Textual.Typ.t option

val update_struct_map : string Hash_set.t -> ProcState.struct_map -> ProcState.struct_map

val update_struct_map_with_field_names :
  ProcState.field_offset_map -> ProcState.struct_map -> ProcState.struct_map

val update_type :
  update_struct_name:(Textual.TypeName.t -> Textual.TypeName.t) -> Textual.Typ.t -> Textual.Typ.t

val update_signature_types :
     Textual.Lang.t
  -> mangled_map:ProcState.mangled_map
  -> struct_map:ProcState.struct_map
  -> plain_map:ProcState.plain_map
  -> Textual.Typ.annotated list option
  -> Textual.Typ.annotated
  -> Textual.Typ.annotated list option * Textual.Typ.annotated

val pp_signature_structs : Format.formatter -> string Hash_set.t -> unit
