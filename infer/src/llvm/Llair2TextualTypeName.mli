(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open! Llair
module State = Llair2TextualState

val to_textual_type_name : Textual.Lang.t -> ?plain_name:label -> label -> Textual.TypeName.t

val mangled_name_of_type_name : Textual.TypeName.t -> label option

val plain_name_of_type_name : Textual.TypeName.t -> label option

val struct_name_of_mangled_name :
     Textual.Lang.t
  -> mangled_map:State.mangled_map option
  -> State.struct_map
  -> string
  -> Textual.TypeName.t

val struct_name_of_plain_name : State.plain_map -> string -> Textual.TypeName.t option

val compute_mangled_map : State.struct_map -> State.mangled_map

val update_type_name_with_mangled_name :
  mangled_name:string -> Textual.TypeName.t -> Textual.TypeName.t

val update_type_name_with_plain_name : plain_name:string -> Textual.TypeName.t -> Textual.TypeName.t

val compute_plain_map : State.struct_map -> State.plain_map
