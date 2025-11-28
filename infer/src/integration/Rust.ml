(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let dump_textual_file file module_ =
  let filename =
    let suffix = "sil" in
    let textual_filename = TextualSil.to_filename file in
    IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) textual_filename suffix
  in
  TextualSil.dump_module ~show_location:true ~filename module_


let run_charon json_file prog args =
  let escaped_cmd = List.map ~f:Escape.escape_shell args |> String.concat ~sep:" " in
  let redirected_cmd =
    F.sprintf "charon %s --ullbc --dest-file %s -- %s" prog json_file escaped_cmd
  in
  let {IUnix.Process_info.stdin; stdout; stderr; pid} =
    IUnix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  Unix.close stderr ;
  let stdout = Unix.in_channel_of_descr stdout in
  (pid, stdout)


let compile prog args =
  let json_file = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "charon" ".ullbc" in
  let pid, stdout = run_charon json_file prog args in
  let output = In_channel.input_lines stdout |> String.concat ~sep:"\n" in
  In_channel.close stdout ;
  match IUnix.waitpid pid with
  | Error _ as status ->
      L.die ExternalError "Rustc exited with: %s@\n Trace: %s\n"
        (IUnix.Exit_or_signal.to_string_hum status)
        output
  | Ok () ->
      json_file


let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let json_filename = compile prog args in
  let json = Yojson.Basic.from_file json_filename in
  let textual =
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok crate ->
        RustFrontend.RustMir2Textual.mk_module crate ~json_filename
    | Error err ->
        L.die UserError "%s: %s" err (Yojson.Basic.to_string json)
  in
  if Config.debug_mode || Config.dump_textual then dump_textual_file json_filename textual ;
  let sourcefile = Textual.SourceFile.create json_filename in
  let verified_textual =
    match TextualVerification.verify_strict textual with
    | Ok vt ->
        vt
    | Error err ->
        L.die UserError "Textual verification failed:%a"
          (F.pp_print_list TextualVerification.pp_error)
          err
  in
  let transformed_textual, decls = TextualTransform.run_exn Rust verified_textual in
  let cfg, tenv =
    match TextualSil.module_to_sil Rust transformed_textual decls with
    | Ok s ->
        s
    | Error err ->
        L.die UserError "Module to sil failed: %a"
          (F.pp_print_list (Textual.pp_transform_error sourcefile))
          err
  in
  let sil = {TextualParser.TextualFile.sourcefile; cfg; tenv} in
  TextualParser.TextualFile.capture ~use_global_tenv:true sil ;
  Tenv.Global.store ~normalize:false sil.tenv
