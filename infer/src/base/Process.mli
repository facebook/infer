(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val create_process_and_wait : prog:string -> args:string list -> unit
(** Given an command to be executed, creates a process to execute this command,
    and waits for its execution. The standard out and error are not redirected.
    If the commands fails to execute, prints an error message and exits. *)

val print_error_and_exit : ?exit_code:int -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Prints an error message to a log file, prints a message saying that the error can be
    found in that file, and exist, with default code 1 or a given code. *)

val pipeline :
     producer_prog:string
  -> producer_args:string list
  -> consumer_prog:string
  -> consumer_args:string list
  -> Unix.Exit_or_signal.t * Unix.Exit_or_signal.t
(** Pipeline producer program into consumer program *)
