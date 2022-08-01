(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let run path =
  let cin = In_channel.create path in
  let filebuf = Lexing.from_channel cin in
  let filename = Filename.basename path in
  let sourcefile = SourceFile.create filename in
  let result =
    try
      let lexer = TextualLexer.main in
      let m = TextualMenhir.main lexer filebuf sourcefile in
      let errors = Textual.Verification.run m in
      if List.is_empty errors then (
        L.result "SIL parsing of %s succeeded.@\n" filename ;
        Ok m )
      else (
        List.iter errors ~f:(fun error ->
            L.external_error "%a" (Textual.Verification.pp_error sourcefile) error ) ;
        Error () )
    with TextualMenhir.Error ->
      let pos = filebuf.Lexing.lex_curr_p in
      let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
      L.external_error "SIL syntax error in file %s at line %d, column %d.\n%!" filename line col ;
      Error ()
  in
  In_channel.close cin ;
  result


let capture path =
  match run path with
  | Error () ->
      ()
  | Ok module_ ->
      let source_file = module_.sourcefile in
      let cfg, tenv = Textual.Module.to_sil module_ in
      SourceFiles.add source_file cfg (FileLocal tenv) None ;
      if Config.debug_mode then Tenv.store_debug_file_for_source source_file tenv ;
      if
        Config.debug_mode || Config.testing_mode || Config.frontend_tests
        || Option.is_some Config.icfg_dotty_outfile
      then DotCfg.emit_frontend_cfg source_file cfg ;
      ()
