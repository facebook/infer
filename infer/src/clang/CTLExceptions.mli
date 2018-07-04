(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Raised when the parser encounters a violation of a certain invariant  *)
exception ALParserInvariantViolationException of string

type exc_info

(** Raised when any exception from the lexer/parser of AL is caught, to include source-location info *)
exception ALFileException of exc_info

val create_exc_info : string -> Lexing.lexbuf -> exc_info

val json_of_exc_info : exc_info -> Yojson.Basic.json
