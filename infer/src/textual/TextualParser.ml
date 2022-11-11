(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type error =
  | SyntaxError of {loc: Textual.Location.t; msg: string}
  | VerificationError of TextualVerification.error
  | TypeError of TextualTypeVerification.error
  | TransformError of Textual.transform_error list

let pp_error sourcefile fmt = function
  | SyntaxError {loc; msg} ->
      F.fprintf fmt "%a, %a: %s" SourceFile.pp sourcefile Textual.Location.pp loc msg
  | VerificationError err ->
      TextualVerification.pp_error sourcefile fmt err
  | TypeError err ->
      TextualTypeVerification.pp_error sourcefile fmt err
  | TransformError errs ->
      List.iter errs ~f:(Textual.pp_transform_error sourcefile fmt)


let log_error sourcefile error = L.external_error "%a@." (pp_error sourcefile) error

let parse_buf sourcefile filebuf =
  try
    let lexer = TextualLexer.main in
    let m = TextualMenhir.main lexer filebuf sourcefile in
    let errors = TextualVerification.run m |> List.map ~f:(fun x -> VerificationError x) in
    if List.is_empty errors then
      let errors = TextualTypeVerification.run m |> List.map ~f:(fun x -> TypeError x) in
      if List.is_empty errors then Ok m else Error errors
    else Error errors
  with TextualMenhir.Error ->
    let pos = filebuf.Lexing.lex_curr_p in
    let buf_length = Lexing.lexeme_end filebuf - Lexing.lexeme_start filebuf in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol - buf_length in
    let msg = "SIL syntax error" in
    Error [SyntaxError {loc= Textual.Location.known ~line ~col; msg}]


let parse_string sourcefile text =
  let filebuf = Lexing.from_string text in
  parse_buf sourcefile filebuf


let parse_chan sourcefile ic =
  let filebuf = Lexing.from_channel ic in
  parse_buf sourcefile filebuf


module TextualFile = struct
  type t = StandaloneFile of string | TranslatedFile of {source_path: string; content: string}

  let source_path = function
    | StandaloneFile path ->
        path
    | TranslatedFile {source_path; _} ->
        source_path


  type sil = {sourcefile: SourceFile.t; cfg: Cfg.t; tenv: Tenv.t}

  let translate textual_file =
    let sourcefile = SourceFile.create (source_path textual_file) in
    let parsed =
      match textual_file with
      | StandaloneFile path ->
          let cin = In_channel.create path in
          let result = parse_chan sourcefile cin in
          In_channel.close cin ;
          result
      | TranslatedFile {content; _} ->
          parse_string sourcefile content
    in
    match parsed with
    | Ok module_ -> (
      try
        let cfg, tenv = TextualSil.module_to_sil module_ in
        Ok {sourcefile; cfg; tenv}
      with Textual.TextualTransformError errors -> Error (sourcefile, [TransformError errors]) )
    | Error errs ->
        Error (sourcefile, errs)


  let capture {sourcefile; cfg; tenv} =
    DB.Results_dir.init sourcefile ;
    SourceFiles.add sourcefile cfg (FileLocal tenv) None ;
    if Config.debug_mode then Tenv.store_debug_file_for_source sourcefile tenv ;
    if
      Config.debug_mode || Config.testing_mode || Config.frontend_tests
      || Option.is_some Config.icfg_dotty_outfile
    then DotCfg.emit_frontend_cfg sourcefile cfg ;
    Tenv.store_global tenv ;
    ()
end

let capture_one textual_file =
  match TextualFile.translate textual_file with
  | Error (sourcefile, errs) ->
      List.iter errs ~f:(log_error sourcefile)
  | Ok sil ->
      TextualFile.capture sil


let capture textual_files = List.iter textual_files ~f:capture_one
