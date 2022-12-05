(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let run path =
  if String.is_suffix path ~suffix:".doli" then (
    let cin = In_channel.create path in
    let filebuf = CombinedLexer.Lexbuf.from_channel cin in
    let filename = Filename.basename path in
    ( try
        let lexer = CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.mainlex filebuf in
        let _ = MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.doliProgram lexer in
        Printf.printf "doli parsing of %s succeeded.\n" filename
      with CombinedMenhir.Error ->
        let token = CombinedLexer.Lexbuf.lexeme filebuf in
        let pos, _ = CombinedLexer.Lexbuf.lexing_positions filebuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
        Printf.eprintf "doli syntax error: unexpected token \"%s\" in %s (line %d, column %d)\n%!"
          token filename line col ) ;
    In_channel.close cin )
