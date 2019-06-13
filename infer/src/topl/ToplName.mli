(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val topl_property : string

val transition : int -> string

val arg : int -> string

val retval : string

val saved_arg : int -> string

val reg : string -> string

val state : string

val maybe : string

val execute : string

val execute_state : int -> string

val save_args : string
