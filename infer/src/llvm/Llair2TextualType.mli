(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair
module ProcState = Llair2TextualState

val to_annotated_textual_typ :
  Textual.Lang.t -> struct_map:ProcState.structMap -> Typ.t -> Textual.Typ.annotated

val to_textual_typ : Textual.Lang.t -> struct_map:ProcState.structMap -> Typ.t -> Textual.Typ.t

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

val update_signature_type :
  Textual.Lang.t -> ProcState.structMap -> Textual.TypeName.t -> Textual.TypeName.t

val pp_signature_structs : Format.formatter -> string Hash_set.t -> unit
