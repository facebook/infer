(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type lexbuf

exception LexingError of Textual.Location.t * string

module Lexbuf : sig
  val from_gen : char Gen.t -> lexbuf

  val from_channel : In_channel.t -> lexbuf

  val lexeme : lexbuf -> string

  val lexing_positions : lexbuf -> Lexing.position * Lexing.position

  val with_tokenizer : (lexbuf -> 'a) -> lexbuf -> unit -> 'a * Lexing.position * Lexing.position
end

val textual_mainlex : lexbuf -> TextualMenhir.token
