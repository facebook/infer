(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val rename : src:string -> dst:string -> unit

val mkdir_p : ?perm:int -> string -> unit

val nanosleep : float -> unit

val readdir_opt : Caml_unix.dir_handle -> string option
