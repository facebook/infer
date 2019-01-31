(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(** Prints an error message to a log file, prints a message saying that the error can be
    found in that file, and exits, with default code 1 or a given code. *)
let print_error_and_exit ?(exit_code = 1) fmt =
  F.kfprintf
    (fun _ ->
      L.external_error "%s" (F.flush_str_formatter ()) ;
      L.exit exit_code )
    F.str_formatter fmt


(** Given a command to be executed, create a process to execute this command, and wait for it to
    terminate. The standard out and error are not redirected.  If the command fails to execute,
    print an error message and exit. *)
let create_process_and_wait ~prog ~args =
  Unix.fork_exec ~prog ~argv:(prog :: args) ()
  |> Unix.waitpid
  |> function
  | Ok () ->
      ()
  | Error _ as status ->
      L.(die ExternalError)
        "Error executing: %s@\n%s@\n"
        (String.concat ~sep:" " (prog :: args))
        (Unix.Exit_or_signal.to_string_hum status)


let pipeline ~producer_prog ~producer_args ~consumer_prog ~consumer_args =
  let open Unix in
  let pipe_in, pipe_out = pipe () in
  let producer_args = Array.of_list producer_args in
  let consumer_args = Array.of_list consumer_args in
  let producer_pid = caml_create_process producer_prog producer_args stdin pipe_out stderr in
  let consumer_pid = caml_create_process consumer_prog consumer_args pipe_in stdout stderr in
  (* wait for children *)
  let producer_status = waitpid producer_pid in
  let consumer_status = waitpid consumer_pid in
  close pipe_out ;
  close pipe_in ;
  (producer_status, consumer_status)
