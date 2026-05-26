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

val swift_class_header_bytes : int
(** Number of bytes consumed by a Swift class header (metadata pointer + reference count) — these
    bytes precede the first declared field in LLVM's view but are not represented as fields in
    Textual structs. *)

val lookup_field_by_byte_offset :
     ?field_byte_offset_map:State.field_byte_offset_map
  -> State.struct_map
  -> Textual.TypeName.t
  -> int
  -> Textual.qualified_fieldname option
(** Given a Swift class struct typename and an LLVM byte offset, return the qualified field that
    starts exactly at that offset, or [None] if no field's start aligns with [byte_offset]. Prefers
    the [field_byte_offset_map] (populated from Wvd field-offset descriptor globals) when supplied,
    falling back to walking the struct's declared fields from [swift_class_header_bytes] with a
    conservative per-field byte-size estimator. *)

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

val build_field_byte_offset_map :
     Textual.Lang.t
  -> mangled_map:State.mangled_map
  -> State.struct_map
  -> State.globals_map
  -> State.field_byte_offset_map

val extract_class_and_field_from_wvd : string -> string option * string
