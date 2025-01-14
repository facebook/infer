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
  | VerificationError of TextualVerification.error list
  | TransformError of Textual.transform_error list

let pp_error sourcefile fmt err =
  let open Textual in
  match err with
  | SyntaxError {loc; msg} ->
      F.fprintf fmt "%a, %a: SIL syntax error: %s@\n" SourceFile.pp sourcefile Location.pp loc msg
  | VerificationError errs ->
      List.iter errs ~f:(F.printf "%a@\n" (TextualVerification.pp_error_with_sourcefile sourcefile))
  | TransformError errs ->
      List.iter errs ~f:(pp_transform_error sourcefile fmt)


let error_to_string sourcefile error = Format.asprintf "%a" (pp_error sourcefile) error

let parse_buf sourcefile filebuf =
  try
    let lexer = TextualLexer.Lexbuf.with_tokenizer TextualLexer.textual_mainlex filebuf in
    let parsed =
      MenhirLib.Convert.Simplified.traditional2revised TextualMenhir.main lexer sourcefile
    in
    (* the parser needs context to understand ident(args) expression because ident may be
       a variable or a procname *)
    Ok (TextualTransform.fix_closure_app parsed)
  with
  | TextualMenhir.Error ->
      let token = TextualLexer.Lexbuf.lexeme filebuf in
      let Lexing.{pos_lnum; pos_cnum; pos_bol}, _ = TextualLexer.Lexbuf.lexing_positions filebuf in
      let loc = Textual.Location.known ~line:pos_lnum ~col:(pos_cnum - pos_bol) in
      Error [SyntaxError {loc; msg= "unexpected token " ^ token}]
  | TextualLexer.LexingError (loc, lexeme) ->
      Error [SyntaxError {loc; msg= "unexpected token " ^ lexeme}]
  | Textual.SpecialSyntaxError (loc, msg) ->
      Error [SyntaxError {loc; msg}]


let parse_string sourcefile text =
  let filebuf = TextualLexer.Lexbuf.from_gen (Gen.of_string text) in
  parse_buf sourcefile filebuf


let parse_chan sourcefile ic =
  let filebuf = TextualLexer.Lexbuf.from_channel ic in
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

  let parse file =
    let sourcefile, parsed =
      match file with
      | StandaloneFile path ->
          Utils.with_file_in path ~f:(fun cin ->
              let sourcefile = Textual.SourceFile.create (source_path file) in
              let result = parse_chan sourcefile cin in
              (sourcefile, result) )
      | TranslatedFile {content; line_map} ->
          let sourcefile = Textual.SourceFile.create ~line_map (source_path file) in
          (sourcefile, parse_string sourcefile content)
    in
    match parsed with
    | Ok module_ ->
        Ok (sourcefile, module_)
    | Error errs ->
        Error (sourcefile, errs)


  let verify sourcefile textual =
    TextualVerification.verify textual
    |> Result.map_error ~f:(fun err -> (sourcefile, [VerificationError err]))


  let lang sourcefile module_ =
    match Textual.Module.lang module_ with
    | None ->
        Error
          ( sourcefile
          , [ TransformError
                [ { loc= Textual.Location.Unknown
                  ; msg= lazy "Missing or unsupported source_language attribute" } ] ] )
    | Some lang ->
        Ok lang


  let textual_to_sil sourcefile module_ =
    let open IResult.Let_syntax in
    let* lang = lang sourcefile module_ in
    let module_, decls_env = TextualTransform.run lang module_ in
    let* cfg, tenv =
      TextualSil.module_to_sil lang module_ decls_env
      |> Result.map_error ~f:(fun errors -> (sourcefile, [TransformError errors]))
    in
    Ok {sourcefile; cfg; tenv}


  let translate file =
    let open IResult.Let_syntax in
    let* sourcefile, textual = parse file in
    let* textual_verified = verify sourcefile textual in
    textual_to_sil sourcefile textual_verified


  let capture ~use_global_tenv {sourcefile; cfg; tenv} =
    let sourcefile = Textual.SourceFile.file sourcefile in
    DB.Results_dir.init sourcefile ;
    let per_file_tenv = if use_global_tenv then Tenv.Global else Tenv.FileLocal tenv in
    SourceFiles.add sourcefile cfg per_file_tenv None ;
    if Config.debug_mode then Tenv.store_debug_file_for_source sourcefile tenv ;
    if
      Config.debug_mode || Config.testing_mode || Config.frontend_tests
      || Option.is_some Config.icfg_dotty_outfile
    then DotCfg.emit_frontend_cfg sourcefile cfg ;
    ()
end

(* This code is used only by the --capture-textual integration, which turn textual files into
   a SIL-Java program. The Hack driver doesn't use this function. *)
let capture files =
  let global_tenv = Tenv.create () in
  let capture_one file =
    match TextualFile.translate file with
    | Error (sourcefile, errs) ->
        List.iter errs ~f:(fun error -> L.external_error "%a" (pp_error sourcefile) error)
    | Ok sil ->
        TextualFile.capture ~use_global_tenv:true sil ;
        Tenv.merge ~src:sil.tenv ~dst:global_tenv
  in
  List.iter files ~f:capture_one ;
  Tenv.Global.store ~normalize:true global_tenv
