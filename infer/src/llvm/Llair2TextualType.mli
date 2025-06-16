(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair
module ProcState = Llair2TextualProcState

val field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val tuple_field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val to_annotated_textual_typ : Textual.Lang.t -> struct_map:'a -> Typ.t -> Textual.Typ.annotated

val to_textual_typ : Textual.Lang.t -> ?struct_map:'a -> Typ.t -> Textual.Typ.t

val type_inference : proc_state:ProcState.t -> Textual.Instr.t list -> unit

val join_typ : Textual.Typ.t option -> Textual.Typ.t option -> Textual.Typ.t option

val translate_types_env :
  Textual.Lang.t -> Llair.Typ.t list -> Textual.Struct.t Textual.TypeName.Map.t
