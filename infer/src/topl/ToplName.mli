(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = string

val topl_property : t

val transition : int -> t

val arg : int -> t

val saved_arg : int -> t

val reg : string -> t

val state : t

val maybe : t

val execute : t

val execute_state : int -> t

val save_args : t
