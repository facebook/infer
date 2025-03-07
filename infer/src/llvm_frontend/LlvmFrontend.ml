(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module Error = struct
  type t =
    | TextualVerification of (Textual.SourceFile.t * TextualVerification.error list)
    | TextualTransformation of (Textual.SourceFile.t * Textual.transform_error list)

  let log level =
    let level = if Config.keep_going then L.InternalError else level in
    match (level : L.error) with
    | InternalError ->
        L.internal_error
    | ExternalError ->
        L.external_error
    | UserError ->
        L.user_error


  let format_error error =
    match error with
    | TextualVerification (source_file, errs) ->
        List.iter errs
          ~f:
            (log L.InternalError "%a@\n" (TextualVerification.pp_error_with_sourcefile source_file))
    | TextualTransformation (source_file, errs) ->
        List.iter errs ~f:(log L.InternalError "%a@\n" (Textual.pp_transform_error source_file))


  let textual_verification sourcefile list = TextualVerification (sourcefile, list)

  let textual_transformation sourcefile list = TextualTransformation (sourcefile, list)
end

let dump_textual_file source_file module_ =
  let source_file =
    if Config.frontend_tests then
      let suffix = ".test.sil" in
      String.append source_file suffix
    else
      let suffix = ".sil" in
      String.append source_file suffix
  in
  TextualSil.dump_module ~filename:source_file module_


let should_dump_textual () = Config.debug_mode || Config.dump_textual || Config.frontend_tests

let to_module source_file llair_program =
  let sourcefile = Textual.SourceFile.create source_file in
  let module_ = Llair2Textual.translate sourcefile llair_program in
  module_


let capture source_file _llvm_bitcode =
  let dummy_program = Llair.Program.mk ~globals:[] ~functions:[] in
  let module_ = to_module source_file dummy_program in
  if should_dump_textual () then dump_textual_file source_file module_


let language_of_source_file source_file =
  if String.is_suffix source_file ~suffix:".c" then Textual.Lang.C
  else L.die UserError "Currently the llvm frontend is only enabled for C programs@."


let capture_llair source_file llair_program =
  let open IResult.Let_syntax in
  let result =
    let textual = to_module source_file llair_program in
    let textual_source_file = Textual.SourceFile.create source_file in
    let* verified_textual =
      let f = Error.textual_verification textual_source_file in
      TextualVerification.verify textual |> Result.map_error ~f
    in
    if should_dump_textual () then dump_textual_file source_file verified_textual ;
    let lang = language_of_source_file source_file in
    let transformed_textual, decls = TextualTransform.run lang verified_textual in
    let* cfg, tenv =
      let f = Error.textual_transformation textual_source_file in
      TextualSil.module_to_sil lang transformed_textual decls |> Result.map_error ~f
    in
    let sil = {TextualParser.TextualFile.sourcefile= textual_source_file; cfg; tenv} in
    TextualParser.TextualFile.capture ~use_global_tenv:false sil ;
    Ok ()
  in
  match result with
  | Ok () ->
      ()
  | Error err ->
      Error.format_error err ;
      ()
