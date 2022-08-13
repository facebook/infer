(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let pp_error sourcefile fmt = function
  | `VerificationError err ->
      Textual.Verification.pp_error sourcefile fmt err
  | `SyntaxError err ->
      F.fprintf fmt "%s" err


let parse sourcefile ic =
  let filebuf = Lexing.from_channel ic in
  try
    let lexer = TextualLexer.main in
    let m = TextualMenhir.main lexer filebuf sourcefile in
    let errors = Textual.Verification.run m in
    if List.is_empty errors then Ok m else Error (List.map errors ~f:(fun x -> `VerificationError x))
  with TextualMenhir.Error ->
    let pos = filebuf.Lexing.lex_curr_p in
    let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
    let err_msg =
      sprintf "SIL syntax error in file %s at line %d, column %d.\n%!"
        (SourceFile.to_string sourcefile) line col
    in
    Error [`SyntaxError err_msg]


let run ?source_path textual_path =
  let cin = In_channel.create textual_path in
  let filename = Filename.basename textual_path in
  let sourcefile =
    match source_path with
    | Some original ->
        SourceFile.create original
    | None ->
        SourceFile.create filename
  in
  let result =
    match parse sourcefile cin with
    | Ok module_ ->
        L.result "SIL parsing of %s succeeded.@\n" filename ;
        Ok module_
    | Error errs ->
        List.iter errs ~f:(fun err -> L.external_error "%a" (pp_error sourcefile) err) ;
        Error ()
  in
  In_channel.close cin ;
  result


let capture ?source_path textual_path =
  match run ?source_path textual_path with
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
