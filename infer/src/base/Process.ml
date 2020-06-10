(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

[@@@warning "+9"]

(** Prints an error message to a log file, prints a message saying that the error can be found in
    that file, and exits, with default code 1 or a given code. *)
let print_error_and_exit ?(exit_code = 1) fmt =
  F.kfprintf
    (fun _ ->
      L.external_error "%s" (F.flush_str_formatter ()) ;
      L.exit exit_code )
    F.str_formatter fmt


(** Given a command to be executed, create a process to execute this command, and wait for it to
    terminate. The standard out and error are not redirected. If the command fails to execute, print
    an error message and exit. *)
let create_process_and_wait ~prog ~args =
  Unix.fork_exec ~prog ~argv:(prog :: args) ()
  |> Unix.waitpid
  |> function
  | Ok () ->
      ()
  | Error _ as status ->
      L.die ExternalError "Error executing: %a@\n%s@\n" Pp.cli_args (prog :: args)
        (Unix.Exit_or_signal.to_string_hum status)


type action = ReadStdout | ReadStderr

let create_process_and_wait_with_output ~prog ~args action =
  let redirected_fd_name, redirect_spec =
    match action with ReadStderr -> ("stderr", "2>") | ReadStdout -> ("stdout", ">")
  in
  let output_file =
    Filename.temp_file ~in_dir:(ResultsDir.get_path Temporary) prog redirected_fd_name
  in
  let escaped_cmd = List.map ~f:Escape.escape_shell (prog :: args) |> String.concat ~sep:" " in
  let redirected_cmd = Printf.sprintf "exec %s %s'%s'" escaped_cmd redirect_spec output_file in
  let {Unix.Process_info.stdin; stdout; stderr; pid} =
    Unix.create_process ~prog:"sh" ~args:["-c"; redirected_cmd]
  in
  let fd_to_log, redirected_fd =
    match action with ReadStderr -> (stdout, stderr) | ReadStdout -> (stderr, stdout)
  in
  let channel_to_log = Unix.in_channel_of_descr fd_to_log in
  Utils.with_channel_in channel_to_log ~f:(L.progress "%s-%s: %s@." prog redirected_fd_name) ;
  In_channel.close channel_to_log ;
  Unix.close redirected_fd ;
  Unix.close stdin ;
  match Unix.waitpid pid with
  | Ok () ->
      Utils.with_file_in output_file ~f:In_channel.input_all
  | Error _ as status ->
      L.die ExternalError "Error executing: %a@\n%s@\n" Pp.cli_args (prog :: args)
        (Unix.Exit_or_signal.to_string_hum status)


let pipeline ~producer_prog ~producer_args ~consumer_prog ~consumer_args =
  let pipe_in, pipe_out = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
      (* redirect producer's stdout to pipe_out *)
      Unix.dup2 ~src:pipe_out ~dst:Unix.stdout () ;
      (* close producer's copy of pipe ends *)
      Unix.close pipe_out ;
      Unix.close pipe_in ;
      (* exec producer *)
      never_returns (Unix.exec ~prog:producer_prog ~argv:producer_args ())
  | `In_the_parent producer_pid -> (
    match Unix.fork () with
    | `In_the_child ->
        (* redirect consumer's stdin to pipe_in *)
        Unix.dup2 ~src:pipe_in ~dst:Unix.stdin () ;
        (* close consumer's copy of pipe ends *)
        Unix.close pipe_out ;
        Unix.close pipe_in ;
        (* exec consumer *)
        never_returns (Unix.exec ~prog:consumer_prog ~argv:consumer_args ())
    | `In_the_parent consumer_pid ->
        (* close parent's copy of pipe ends *)
        Unix.close pipe_out ;
        Unix.close pipe_in ;
        (* wait for children *)
        let producer_status = Unix.waitpid producer_pid in
        let consumer_status = Unix.waitpid consumer_pid in
        (producer_status, consumer_status) )
