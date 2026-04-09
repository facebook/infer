(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type error

val pp_error : Format.formatter -> error -> unit [@@warning "-unused-value-declaration"]

val pp_error_with_sourcefile : Textual.SourceFile.t -> Format.formatter -> error -> unit

val verify_strict : Textual.Module.t -> (Textual.Module.t, error list) result
(** perform verification (some basic rules, simple type checking, no duplicate declaration) and
    returns a new version (after type inference) *)

val verify_keep_going :
  ?lenient:bool -> Textual.Module.t -> (Textual.Module.t * error list, error list) result
(** Same as [verify_strict], but we try to generate a filtered module that contains a subset of the
    original input module, but pass the whole verification. When [lenient] is true, basic
    verification errors (e.g. arity mismatches on undeclared functions) are also treated as
    recoverable, allowing capture to proceed with warnings. Default is [false]. *)
