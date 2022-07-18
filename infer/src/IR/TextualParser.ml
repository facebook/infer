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
    let sourcefile = SourceFile.create filename in
    ( try
        let lexer = TextualLexer.main in
        let m = TextualMenhir.main lexer filebuf sourcefile in
        let errors = Textual.Verification.run m in
        if not (List.is_empty errors) then
          List.iter errors ~f:(Textual.Verification.pp_error Format.std_formatter sourcefile)
        else Printf.printf "SIL parsing of %s succeeded.\n" filename
      with TextualMenhir.Error ->
        let pos = filebuf.Lexing.lex_curr_p in
        let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
        let line = pos.Lexing.pos_lnum in
        let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
        Printf.eprintf "SIL syntax error in file %s at line %d, column %d.\n%!" filename line col ) ;
    In_channel.close cin )
