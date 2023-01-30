(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let parse_buf (filebuf : CombinedLexer.lexbuf) =
  try
    let lexer = CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.mainlex filebuf in
    let doliProgram =
      MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.doliProgram lexer
    in
    Ok doliProgram
  with
  | CombinedMenhir.Error ->
      let token = CombinedLexer.Lexbuf.lexeme filebuf in
      let Lexing.{pos_lnum; pos_cnum; pos_bol}, _ = CombinedLexer.Lexbuf.lexing_positions filebuf in
      let loc = Textual.Location.known ~line:pos_lnum ~col:(pos_cnum - pos_bol) in
      Error [TextualParser.SyntaxError {loc; msg= "unexpected token " ^ token}]
  | CombinedLexer.LexingError (loc, lexeme) ->
      Error [TextualParser.SyntaxError {loc; msg= "unexpected token " ^ lexeme}]


let just_parse path =
  Utils.with_file_in path ~f:(fun cin ->
      let filebuf = CombinedLexer.Lexbuf.from_channel cin in
      match parse_buf filebuf with
      | Ok _ ->
          L.progress "%s, parsing succeeded.\n" (Filename.basename path)
      | Error errs ->
          List.iter errs ~f:(fun error ->
              L.external_error "%a@\n"
                (TextualParser.pp_error (Textual.SourceFile.create path))
                error ) )
