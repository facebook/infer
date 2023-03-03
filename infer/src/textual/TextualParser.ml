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
  | BasicError of TextualBasicVerification.error
  | TypeError of TextualTypeVerification.error
  | TransformError of Textual.transform_error list
  | DeclaredTwiceError of TextualDecls.error

type whichCapture = DoliCapture | TextualCapture

let pp_error sourcefile fmt = function
  | SyntaxError {loc; msg} ->
      F.fprintf fmt "%a, %a: SIL syntax error: %s" Textual.SourceFile.pp sourcefile
        Textual.Location.pp loc msg
  | BasicError err ->
      TextualBasicVerification.pp_error sourcefile fmt err
  | TypeError err ->
      TextualTypeVerification.pp_error sourcefile fmt err
  | TransformError errs ->
      List.iter errs ~f:(Textual.pp_transform_error sourcefile fmt)
  | DeclaredTwiceError err ->
      TextualDecls.pp_error sourcefile fmt err


let parse_buf ~capture sourcefile filebuf =
  try
    let lexer =
      match capture with
      | TextualCapture ->
          CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.textual_mainlex filebuf
      | DoliCapture ->
          CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.doli_mainlex filebuf
    in
    let parsed =
      match capture with
      | TextualCapture ->
          MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.main lexer sourcefile
      | DoliCapture ->
          let doliModule =
            MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.doliProgram lexer
          in
          DoliToTextual.program_to_textual_module sourcefile doliModule
    in
    let twice_declared_errors, decls_env = TextualDecls.make_decls parsed in
    let twice_declared_errors = List.map twice_declared_errors ~f:(fun x -> DeclaredTwiceError x) in
    (* even if twice_declared_errors is not empty we can continue the other verifications *)
    let errors =
      TextualBasicVerification.run parsed decls_env |> List.map ~f:(fun x -> BasicError x)
    in
    if List.is_empty errors then
      let errors =
        TextualTypeVerification.run parsed decls_env |> List.map ~f:(fun x -> TypeError x)
      in
      let errors = twice_declared_errors @ errors in
      if List.is_empty errors then Ok parsed else Error errors
    else Error (twice_declared_errors @ errors)
  with
  | CombinedMenhir.Error ->
      let token = CombinedLexer.Lexbuf.lexeme filebuf in
      let Lexing.{pos_lnum; pos_cnum; pos_bol}, _ = CombinedLexer.Lexbuf.lexing_positions filebuf in
      let loc = Textual.Location.known ~line:pos_lnum ~col:(pos_cnum - pos_bol) in
      Error [SyntaxError {loc; msg= "unexpected token " ^ token}]
  | CombinedLexer.LexingError (loc, lexeme) ->
      Error [SyntaxError {loc; msg= "unexpected token " ^ lexeme}]


let parse_string sourcefile text =
  let filebuf = CombinedLexer.Lexbuf.from_gen (Gen.of_string text) in
  parse_buf ~capture:TextualCapture sourcefile filebuf


let parse_chan ~capture sourcefile ic =
  let filebuf = CombinedLexer.Lexbuf.from_channel ic in
  parse_buf ~capture sourcefile filebuf


module TextualFile = struct
  type t =
    | StandaloneFile of string
    | TranslatedFile of {source_path: string; content: string; line_map: LineMap.t}

  let source_path = function
    | StandaloneFile path ->
        path
    | TranslatedFile {source_path; _} ->
        source_path


  let line_map textual_file =
    match textual_file with StandaloneFile _ -> None | TranslatedFile {line_map} -> Some line_map


  type sil = {sourcefile: Textual.SourceFile.t; cfg: Cfg.t; tenv: Tenv.t}

  let translate_textual_or_doli ~capture file =
    let sourcefile, parsed =
      match file with
      | StandaloneFile path ->
          Utils.with_file_in path ~f:(fun cin ->
              let sourcefile = Textual.SourceFile.create (source_path file) in
              let result = parse_chan ~capture sourcefile cin in
              (sourcefile, result) )
      | TranslatedFile {content; line_map} ->
          let sourcefile = Textual.SourceFile.create ~line_map (source_path file) in
          (sourcefile, parse_string sourcefile content)
    in
    match parsed with
    | Ok module_ -> (
      try
        let cfg, tenv = TextualSil.module_to_sil module_ in
        Ok {sourcefile; cfg; tenv}
      with Textual.TextualTransformError errors -> Error (sourcefile, [TransformError errors]) )
    | Error errs ->
        Error (sourcefile, errs)


  let translate file = translate_textual_or_doli ~capture:TextualCapture file

  let capture {sourcefile; cfg; tenv} =
    let sourcefile = Textual.SourceFile.file sourcefile in
    DB.Results_dir.init sourcefile ;
    SourceFiles.add sourcefile cfg (FileLocal tenv) None ;
    if Config.debug_mode then Tenv.store_debug_file_for_source sourcefile tenv ;
    if
      Config.debug_mode || Config.testing_mode || Config.frontend_tests
      || Option.is_some Config.icfg_dotty_outfile
    then DotCfg.emit_frontend_cfg sourcefile cfg ;
    tenv
end

(* This code is used only by the --capture-textual integration, which turn textual files into
   a SIL-Java program. The Hack driver doesn't use this function. *)
let capture ~capture files =
  let global_tenv = Tenv.create () in
  let capture_one file =
    match TextualFile.translate_textual_or_doli ~capture file with
    | Error (sourcefile, errs) ->
        List.iter errs ~f:(fun error -> L.external_error "%a@\n" (pp_error sourcefile) error)
    | Ok sil ->
        let tenv = TextualFile.capture sil in
        Tenv.merge ~src:tenv ~dst:global_tenv
  in
  List.iter files ~f:capture_one ;
  Tenv.store_global global_tenv
