(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type error

val parse_string : SourceFile.t -> string -> (Textual.Module.t, error list) result [@@warning "-32"]

val pp_error : SourceFile.t -> F.formatter -> error -> unit [@@warning "-32"]

val capture : ?source_path:string -> string -> unit
(** Capture textual from the provided path. When the textual is a result of translation of another
    source file, [source_path] specifies the orignal source. *)
