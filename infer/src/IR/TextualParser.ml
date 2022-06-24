(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let run path =
  if String.is_suffix path ~suffix:".sil" then (
    let cin = In_channel.create path in
    let filebuf = Lexing.from_channel cin in
    let filename = Filename.basename path in
    ( try
        let lexer = TextualLexer.main in
        let _ = TextualMenhir.main lexer filebuf in
        Printf.printf "SIL parsing of %s succeeded.\n" filename
      with TextualMenhir.Error ->
        let pos = filebuf.Lexing.lex_curr_p in
        let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
        Printf.eprintf "SIL syntax error in file %s at line %d, column %d.\n%!" filename line col ) ;
    In_channel.close cin )
