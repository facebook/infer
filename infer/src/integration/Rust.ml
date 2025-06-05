(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
module F = Format

let start_rustc _compiler args =
  let stderr_log = IFilename.temp_file ~in_dir:(ResultsDir.get_path Temporary) "rustc" "stderr" in
  let escaped_cmd = List.map ~f:Escape.escape_shell args |> String.concat ~sep:" " in
  (* We need to set the cwd for cargo the the program so that it uses the nightly toolchain otherwise
     we cant access the internal rust compiler crates in the rust program.*)
  let redirected_cmd = F.sprintf "cd %s/rust && cargo run -- %s" Config.lib_dir escaped_cmd in
  let {IUnix.Process_info.stdin; stdout; stderr; pid} =
    IUnix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  Unix.close stdin ;
  Unix.close stderr ;
  let stdout = Unix.in_channel_of_descr stdout in
  (pid, stdout, stderr_log)


let compile compiler args ~process_output =
  let pid, rustc_stdout, stderr_log = start_rustc compiler args in
  let output = process_output rustc_stdout in
  In_channel.close rustc_stdout ;
  ( match IUnix.waitpid pid with
  | Error _ as status ->
      L.die ExternalError "Rustc exited with: %s@\n Trace: %s\n"
        (IUnix.Exit_or_signal.to_string_hum status)
        output stderr_log
  | Ok () ->
      () ) ;
  output


let process_output_sequentially rustc_stdout =
  In_channel.input_lines rustc_stdout |> String.concat ~sep:"\n"


let capture prog (args : string list) =
  if not (String.equal prog "rustc") then
    L.die UserError "rustc should be explicitly used instead of %s." prog ;
  let textual_output = compile "rustc" args ~process_output:process_output_sequentially in
  (* TODO: Translate textual file and process output*)
  L.die InternalError "Rust.capture not yet implented: prog:%s , arg:%s , output: %s" prog
    (String.concat ~sep:" " args) textual_output
