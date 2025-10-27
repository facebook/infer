(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair

val field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val tuple_field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val to_annotated_textual_typ : Textual.Lang.t -> struct_map:'a -> Typ.t -> Textual.Typ.annotated

val to_textual_typ : Textual.Lang.t -> ?struct_map:'a -> Typ.t -> Textual.Typ.t

val join_typ : Textual.Typ.t option -> Textual.Typ.t option -> Textual.Typ.t option

val translate_types_env :
  Textual.Lang.t -> Llair.Typ.t list -> Textual.Struct.t Textual.TypeName.Map.t

val lookup_field_type :
     struct_map:Textual.Struct.t Textual.TypeName.Map.t
  -> Textual.TypeName.t
  -> Textual.qualified_fieldname
  -> Textual.Typ.t option

val tuple_field_prefix : string

val signature_type_to_textual_typ : Textual.Lang.t -> string -> Textual.Typ.t option
