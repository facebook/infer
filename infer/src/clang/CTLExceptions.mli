(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

exception
  ALParserInvariantViolationException of
  string(** Raised when the parser encounters a violation of a certain invariant  *)

type exc_info

exception
  ALFileException of
  exc_info(** Raised when any exception from the lexer/parser of AL is caught, to include source-location info *)

val create_exc_info : string -> Lexing.lexbuf -> exc_info

val hum_string_of_exc_info : exc_info -> string
(** human-readable version of exc_info *)

val json_of_exc_info : exc_info -> Yojson.Basic.json
