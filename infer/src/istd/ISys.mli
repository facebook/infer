(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val file_exists : ?follow_symlinks:bool -> string -> bool
(** Similar to [Sys.file_exists_exn], but it returns [false] when the result is unknown, instead of
    raising an exception. [follow_symlinks] is true by default. *)
