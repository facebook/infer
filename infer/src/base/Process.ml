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
  let {Unix.Process_info.stdin; stdout; stderr; pid} = Unix.create_process ~prog ~args in
  Unix.close stdin ;
  (* NOTE: this simple implementation works well because we only read on *one* of stdout or
     stderr. Reading on both is a lot more difficult: we would have to be careful not to block the
     callee process on writing on either stdout or stderr, so issue non-blocking reads on both
     stdout and stderr until the end of the program, probably using select(2). *)
  let in_chan =
    let redirect_read ~redirect:(dst, src) ~read =
      (* redirect *)
      Unix.dup2 ~src ~dst ; Unix.in_channel_of_descr read
    in
    match action with
    | ReadStdout ->
        redirect_read ~redirect:(stderr, Unix.stderr) ~read:stdout
    | ReadStderr ->
        redirect_read ~redirect:(stdout, Unix.stdout) ~read:stderr
  in
  let res = In_channel.input_all in_chan in
  match Unix.waitpid pid with
  | Ok () ->
      Unix.close (Unix.descr_of_in_channel in_chan) ;
      res
  | Error _ as status ->
      L.die ExternalError "Error executing: %a@\n%s@\n" Pp.cli_args (prog :: args)
        (Unix.Exit_or_signal.to_string_hum status)


let pipeline ~producer_prog ~producer_args ~consumer_prog ~consumer_args =
  let pipe_in, pipe_out = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
      (* redirect producer's stdout to pipe_out *)
      Unix.dup2 ~src:pipe_out ~dst:Unix.stdout ;
      (* close producer's copy of pipe ends *)
      Unix.close pipe_out ;
      Unix.close pipe_in ;
      (* exec producer *)
      never_returns (Unix.exec ~prog:producer_prog ~argv:producer_args ())
  | `In_the_parent producer_pid -> (
    match Unix.fork () with
    | `In_the_child ->
        (* redirect consumer's stdin to pipe_in *)
        Unix.dup2 ~src:pipe_in ~dst:Unix.stdin ;
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
