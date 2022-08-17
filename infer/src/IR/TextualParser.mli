(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val capture : ?source_path:string -> string -> unit
(** Capture textual from the provided path. When the textual is a result of translation of another
    source file, [source_path] specifies the orignal source. *)
