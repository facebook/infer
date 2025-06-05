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


let language_of_source_file source_file =
  if String.is_suffix source_file ~suffix:".c" then Textual.Lang.C
  else if String.is_suffix source_file ~suffix:".swift" then Textual.Lang.Swift
  else L.die UserError "Currently the llvm frontend is only enabled for C and Swift programs@."


let capture_llair source_file llair_program =
  let open IResult.Let_syntax in
  let lang = language_of_source_file source_file in
  let result =
    let textual = to_module source_file llair_program lang in
    if should_dump_textual () then dump_textual_file ~show_location:true source_file textual ;
    let textual_source_file = Textual.SourceFile.create source_file in
    let map_errors = Error.textual_verification textual_source_file in
    let* verified_textual, warnings =
      match TextualVerification.verify_keep_going textual with
      | Ok (textual, errors) ->
          Ok (textual, map_errors errors)
      | Error errors ->
          Error (map_errors errors)
    in
    let transformed_textual, decls = TextualTransform.run lang verified_textual in
    let* cfg, tenv =
      let f = Error.textual_transformation textual_source_file in
      TextualSil.module_to_sil lang transformed_textual decls |> Result.map_error ~f
    in
    let sil = {TextualParser.TextualFile.sourcefile= textual_source_file; cfg; tenv} in
    TextualParser.TextualFile.capture ~use_global_tenv:false sil ;
    Ok warnings
  in
  match result with
  | Ok warnings ->
      Error.format_error warnings
  | Error err ->
      Error.format_error err


let dump_llair_text llair_program source_file =
  let output_file =
    let suffix = ".llair.text" in
    String.append source_file suffix
  in
  Utils.with_file_out output_file ~f:(fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      Llair.Program.pp fmt llair_program ;
      Format.pp_print_flush fmt () )


let dump_llair llair_program source_file =
  let output_file =
    let suffix = ".llair" in
    String.append source_file suffix
  in
  let marshal program file =
    Utils.with_file_out file ~f:(fun outc -> Marshal.to_channel outc program [])
  in
  marshal llair_program output_file


let capture ~sources llvm_bitcode_in =
  let llvm_program = In_channel.input_all llvm_bitcode_in in
  let llair_program = LlvmSledgeFrontend.translate llvm_program in
  List.iter sources ~f:(fun source_file ->
      if Config.dump_llair then dump_llair llair_program source_file ;
      if Config.dump_llair_text then dump_llair_text llair_program source_file ;
      capture_llair source_file llair_program )
