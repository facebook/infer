(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val executable_name : string
val getenv : string -> string option
external getcwd : unit -> string = "caml_sys_getcwd"
val word_size : int
val ocaml_version : string
