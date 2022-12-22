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
  | DeclaredTwiceError of TextualDecls.error

let pp_error sourcefile fmt = function
  | SyntaxError {loc; msg} ->
      F.fprintf fmt "%a, %a: SIL syntax error: %s" Textual.SourceFile.pp sourcefile
        Textual.Location.pp loc msg
  | VerificationError err ->
      TextualVerification.pp_error sourcefile fmt err
  | TypeError err ->
      TextualTypeVerification.pp_error sourcefile fmt err
  | TransformError errs ->
      List.iter errs ~f:(Textual.pp_transform_error sourcefile fmt)
  | DeclaredTwiceError err ->
      TextualDecls.pp_error sourcefile fmt err


let log_error sourcefile error =
  if Config.keep_going then L.debug Capture Quiet "%a@\n" (pp_error sourcefile) error
  else L.external_error "%a@\n" (pp_error sourcefile) error


let parse_buf sourcefile (filebuf : CombinedLexer.lexbuf) =
  try
    let lexer = CombinedLexer.Lexbuf.with_tokenizer CombinedLexer.mainlex filebuf in
    let m = MenhirLib.Convert.Simplified.traditional2revised CombinedMenhir.main lexer sourcefile in
    let twice_declared_errors, decls_env = TextualDecls.make_decls m in
    let twice_declared_errors = List.map twice_declared_errors ~f:(fun x -> DeclaredTwiceError x) in
    (* even if twice_declared_errors is not empty we can continue the other verifications *)
    let errors =
      TextualVerification.run m decls_env |> List.map ~f:(fun x -> VerificationError x)
    in
    if List.is_empty errors then
      let errors = TextualTypeVerification.run m decls_env |> List.map ~f:(fun x -> TypeError x) in
      let errors = twice_declared_errors @ errors in
      if List.is_empty errors then Ok m else Error errors
    else Error (twice_declared_errors @ errors)
  with
  | CombinedMenhir.Error ->
      let token = CombinedLexer.Lexbuf.lexeme filebuf in
      let msg = Format.sprintf "unexpected token %s" token in
      let pos, _ = CombinedLexer.Lexbuf.lexing_positions filebuf in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      Error [SyntaxError {loc= Textual.Location.known ~line ~col; msg}]
  | CombinedLexer.LexingError (loc, lexeme) ->
      let msg = sprintf "unexpected token %s" lexeme in
      Error [SyntaxError {loc; msg}]


let parse_string sourcefile text =
  let filebuf = CombinedLexer.Lexbuf.from_gen (Gen.of_string text) in
  parse_buf sourcefile filebuf


let parse_chan sourcefile ic =
  let filebuf = CombinedLexer.Lexbuf.from_channel ic in
  parse_buf sourcefile filebuf


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

  let translate textual_file =
    let sourcefile, parsed =
      match textual_file with
      | StandaloneFile path ->
          let sourcefile = Textual.SourceFile.create (source_path textual_file) in
          let cin = In_channel.create path in
          let result = parse_chan sourcefile cin in
          In_channel.close cin ;
          (sourcefile, result)
      | TranslatedFile {content; line_map} ->
          let sourcefile = Textual.SourceFile.create ~line_map (source_path textual_file) in
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

(* This code is used only by the --capture-textual integration, which includes Java which requires a
   global tenv. The Hack driver doesn't use this function. *)
let capture textual_files =
  let global_tenv = Tenv.create () in
  let capture_one textual_file =
    match TextualFile.translate textual_file with
    | Error (sourcefile, errs) ->
        List.iter errs ~f:(log_error sourcefile)
    | Ok sil ->
        let tenv = TextualFile.capture sil in
        Tenv.merge ~src:tenv ~dst:global_tenv
  in
  List.iter textual_files ~f:capture_one ;
  Tenv.store_global global_tenv
