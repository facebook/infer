(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let dump_textual_file source_file module_ =
  let suffix = ".sil" in
  let filename =
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary)
      (TextualSil.to_filename source_file)
      suffix
  in
  TextualSil.dump_module ~show_location:true ~filename module_ ;
  L.debug Capture Quiet "Wrote textual SIL to %s@\n" filename


let preprocess source_file preprocessor_cmd =
  let pp_file =
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary)
      (Filename.chop_extension (Filename.basename source_file))
      ".c"
  in
  let cmd =
    F.sprintf "%s %s > %s" preprocessor_cmd (Escape.escape_shell source_file)
      (Escape.escape_shell pp_file)
  in
  let {IUnix.Process_info.stdin; stdout; stderr; pid} =
    IUnix.create_process ~prog:"sh" ~args:["-c"; cmd]
  in
  Unix.close stdin ;
  Unix.close stdout ;
  Unix.close stderr ;
  match IUnix.waitpid pid with
  | Ok () ->
      pp_file
  | Error _ as status ->
      L.die ExternalError "Preprocessing failed for %s: %s" source_file
        (IUnix.Exit_or_signal.to_string_hum status)


(** Determine the source language from a file extension. Only C is currently supported. *)
let lang_of_file source_file =
  match Filename.split_extension source_file with
  | _, Some "c" | _, Some "h" ->
      `C
  | _, Some ext ->
      L.die UserError
        "tree-sitter capture: unsupported file extension '.%s' for %s.@\n\
         Currently only C (.c, .h) files are supported."
        ext source_file
  | _, None ->
      L.die UserError "tree-sitter capture: cannot determine language for %s (no extension)."
        source_file


let capture_file source_file =
  L.progress "Capturing %s@\n" source_file ;
  try
    let lang = lang_of_file source_file in
    let parse_file =
      match Config.tree_sitter_preprocessor with
      | Some cmd ->
          preprocess source_file cmd
      | None ->
          source_file
    in
    let textual =
      match lang with
      | `C ->
          let root = TreeSitterFFI.parse_file parse_file in
          TreeSitterCTranslator.translate_cst root ~file_name:source_file
    in
    if Config.debug_mode || Config.dump_textual then dump_textual_file source_file textual ;
    let verified_textual =
      match TextualVerification.verify_keep_going ~lenient:true textual with
      | Ok (vt, errors) ->
          if not (List.is_empty errors) then
            List.iter errors ~f:(fun err ->
                L.debug Capture Quiet "%a@\n" TextualVerification.pp_error err ) ;
          Some vt
      | Error errs ->
          List.iter errs ~f:(fun err ->
              L.debug Capture Quiet "%a@\n" TextualVerification.pp_error err ) ;
          if Config.debug_mode then dump_textual_file source_file textual ;
          L.progress "Textual verification failed for %s, skipping@\n" source_file ;
          None
    in
    match verified_textual with
    | None ->
        Error ()
    | Some verified_textual ->
        let transformed_textual, decls = TextualTransform.run_exn C verified_textual in
        if Config.debug_mode || Config.dump_textual then
          dump_textual_file source_file transformed_textual ;
        let sourcefile = Textual.SourceFile.create source_file in
        let cfg, tenv =
          match TextualSil.module_to_sil C transformed_textual decls with
          | Ok s ->
              s
          | Error errs ->
              List.iter errs ~f:(fun err ->
                  L.external_error "%a@\n" (Textual.pp_transform_error sourcefile) err ) ;
              L.die ExternalError "Textual to SIL conversion failed for %s" source_file
        in
        let sil = {TextualParser.TextualFile.sourcefile; cfg; tenv} in
        TextualParser.TextualFile.capture ~textual_module:transformed_textual ~use_global_tenv:true
          sil ;
        Ok tenv
  with exn ->
    L.progress "Tree-sitter capture failed for %s: %s@\n" source_file (Exn.to_string exn) ;
    Error ()


let capture ~files =
  let global_tenv = Tenv.create () in
  let n_captured = ref 0 in
  let n_error = ref 0 in
  List.iter files ~f:(fun file ->
      match capture_file file with
      | Ok file_tenv ->
          Tenv.merge ~src:file_tenv ~dst:global_tenv ;
          incr n_captured
      | Error () ->
          incr n_error ) ;
  Tenv.Global.store ~normalize:true global_tenv ;
  L.progress "Finished tree-sitter capture: %d files captured, %d errors.@\n" !n_captured !n_error
