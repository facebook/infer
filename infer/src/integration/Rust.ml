(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format


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
      (json_file)


let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let json_file = compile prog args in
  let json = Yojson.Basic.from_file json_file in
  match Charon.UllbcOfJson.crate_of_json json with
  | Ok _crate ->
      Printf.printf "Success: %s" json_file
  | Error err ->
      L.die UserError "%s: %s" err (Yojson.Basic.to_string json)
