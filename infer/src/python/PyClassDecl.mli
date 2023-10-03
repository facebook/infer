(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Error : sig
  type kind

  val pp_kind : Format.formatter -> kind -> unit
end

module State : sig
  type t = private
    { members: PyCommon.annotated_name list
    ; methods: PyCommon.method_info list
    ; static_methods: PyCommon.method_info list
    ; has_init: PyCommon.annotated_name list option
    ; has_new: PyCommon.annotated_name list option }
end

val parse_class_declaration :
  FFI.Code.t -> string -> FFI.Instruction.t list -> (State.t, Error.kind) result
