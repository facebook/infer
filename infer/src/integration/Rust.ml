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


let dump_textual_test_file file module_ =
  let suffix = "test.sil" in
  let filename = Format.asprintf "%s.%s" file suffix in
  TextualSil.dump_module ~show_location:false ~filename module_


let run_charon json_file prog args =
  let escaped_cmd = List.map ~f:Escape.escape_shell args |> String.concat ~sep:" " in
  let redirected_cmd =
    F.sprintf "charon %s --ullbc --dest-file %s -- %s" prog json_file escaped_cmd
  in
  let {IUnix.Process_info.stdin; stdout; stderr; pid} =
    IUnix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  (pid, stdout, stderr)


let compile prog args =
  let json_file = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "charon" ".ullbc" in
  let pid, stdout, stderr = run_charon json_file prog args in
  let output = In_channel.input_lines stdout |> String.concat ~sep:"\n" in
  let error = In_channel.input_lines stderr |> String.concat ~sep:"\n" in
  In_channel.close stdout ;
  In_channel.close stderr ;
  match IUnix.waitpid pid with
  | Error _ as status ->
      L.die ExternalError "Rustc exited with: %s@\n Trace: %s\n%s\n"
        (IUnix.Exit_or_signal.to_string_hum status)
        output error
  | Ok () ->
      json_file


(* 
  In a ullbc file the first item in files is the 
  entry point e.g. the file the command is executed on or 
  the main file in a cargo project. 
  The remaining files are the dependencies.
  TODO: In the future we probably want to do the textual translation on a file per file basis.
*)
let filename_from_json json =
  let file_name =
    json
    |> Yojson.Basic.Util.member "translated"
    |> Yojson.Basic.Util.member "files" |> Yojson.Basic.Util.to_list
    |> List.map ~f:(fun file ->
           Yojson.Basic.Util.member "name" file
           |> Yojson.Basic.Util.member "Local" |> Yojson.Basic.Util.to_string )
    |> List.hd
  in
  match file_name with
  | Some file_name ->
      file_name
  | None ->
      L.die UserError "No file found in crate"


let capture_file json_filename =
  let json = Yojson.Basic.from_file json_filename in
  let file_name = filename_from_json json in
  let textual =
    match Charon.UllbcOfJson.crate_of_json json with
    | Ok crate ->
        RustFrontend.RustMir2Textual.mk_module crate ~file_name
    | Error err ->
        L.die UserError "%s: %s" err (Yojson.Basic.to_string json)
  in
  let sourcefile = Textual.SourceFile.create file_name in
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
  if Config.debug_mode || Config.dump_textual then
    dump_textual_file json_filename transformed_textual ;
  if Config.frontend_tests then dump_textual_test_file json_filename transformed_textual ;
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


let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let json_filename = compile prog args in
  capture_file json_filename


let capture_ullbc (files : string list) = List.iter files ~f:capture_file
