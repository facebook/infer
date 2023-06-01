(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val parse_class_declaration :
  FFI.Code.t -> FFI.Instruction.t list -> PyCommon.annotated_name list * PyCommon.method_info list
