(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val open_temp_file : ?perm:int -> ?in_dir:string -> string -> string -> string * Out_channel.t
(** escapes [prefix] and [suffix] to avoid failures related to invalid characters in filenames *)

val temp_file : ?perm:int -> ?in_dir:string -> string -> string -> string
(** escapes [prefix] and [suffix] to avoid failures related to invalid characters in filenames *)

val temp_dir : ?perm:int -> ?in_dir:string -> string -> string -> string
