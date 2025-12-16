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
  -> mangled_map:Textual.TypeName.t IString.Map.t
  -> struct_map:ProcState.structMap
  -> Typ.t
  -> Textual.Typ.annotated

val to_annotated_textual_typ_without_mangled_map :
  Textual.Lang.t -> struct_map:ProcState.structMap -> Typ.t -> Textual.Typ.annotated

val to_textual_typ :
     Textual.Lang.t
  -> mangled_map:Textual.TypeName.t IString.Map.t
  -> struct_map:ProcState.structMap
  -> Typ.t
  -> Textual.Typ.t

val to_textual_typ_without_mangled_map :
  Textual.Lang.t -> struct_map:ProcState.structMap -> Typ.t -> Textual.Typ.t

val join_typ : Textual.Typ.t option -> Textual.Typ.t option -> Textual.Typ.t option

val translate_types_env :
  Textual.Lang.t -> Llair.Typ.t list -> Textual.Struct.t Textual.TypeName.Map.t

val lookup_field_type :
     struct_map:Textual.Struct.t Textual.TypeName.Map.t
  -> Textual.TypeName.t
  -> Textual.qualified_fieldname
  -> Textual.Typ.t option

val signature_type_to_textual_typ :
  string Hash_set.t -> Textual.Lang.t -> string -> Textual.Typ.t option

val update_struct_map : string Hash_set.t -> ProcState.structMap -> ProcState.structMap

val update_struct_map_with_field_names :
  ProcState.fieldOffsetMap -> ProcState.structMap -> ProcState.structMap

val update_type :
  update_struct_name:(Textual.TypeName.t -> Textual.TypeName.t) -> Textual.Typ.t -> Textual.Typ.t

val update_signature_types :
     Textual.Lang.t
  -> mangled_map:Textual.TypeName.t IString.Map.t
  -> struct_map:'a Textual.TypeName.Map.t
  -> Textual.Typ.annotated list option
  -> Textual.Typ.annotated
  -> Textual.Typ.annotated list option * Textual.Typ.annotated

val pp_signature_structs : Format.formatter -> string Hash_set.t -> unit
