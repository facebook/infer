(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Llair
module State = Llair2TextualState

val get_suffix : string

val set_suffix : string

val modify_suffix : string

val tuple_field_prefix : string

val field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

val field_of_pos_with_map :
  Llair2TextualState.field_offset_map -> Textual.TypeName.t -> int -> Textual.qualified_fieldname

val tuple_field_of_pos : Textual.TypeName.t -> int -> Textual.qualified_fieldname

module OffsetIndex : sig
  val build_field_offset_map :
       Textual.Lang.t
    -> mangled_map:State.mangled_map
    -> plain_map:State.plain_map
    -> State.struct_map
    -> (FuncName.t * func) list
    -> Textual.FieldName.t State.FieldOffsetMap.t
end
