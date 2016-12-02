(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Given an command to be executed, creates a process to execute this command,
    and waits for its execution. The standard out and error are not redirected.
    If the commands fails to execute, prints an error message and exits. *)
val create_process_and_wait : string array -> unit

(** Given an command to be executed, creates a process to execute this command,
    and waits for its execution. The standard out and error are not redirected.
    If the commands fails to execute, prints an error message and exits. *)
val exec_command : string -> string array -> string array -> unit

(** Prints an error message to a log file, prints a message saying that the error can be
    found in that file, and exist, with default code 1 or a given code. *)
val print_error_and_exit :
  ?exit_code:int -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Prints information about a unix error *)
val print_unix_error : string -> exn -> unit

(** [run_jobs_in_parallel jobs_stack gen_cmd cmd_to_string] runs the jobs in the given stack, by
    spawning the jobs in batches of n, where n is [Config.jobs]. It then waits for all those jobs
    and starts a new batch and so on. [gen_cmd] should return a tuple [(dir_opt, command, args,
    env)] where [dir_opt] is an optional directory to chdir to before executing the process, and
    [command], [args], [env] are the same as for [exec_command]. [cmd_to_string] is used for
    printing information about the job's status. *)
val run_jobs_in_parallel : 'a Stack.t ->
  ('a -> (string option * string * string array * string array)) -> ('a -> string) -> unit

(** Pipeline producer program into consumer program *)
val pipeline : producer_prog:string -> producer_args:string list -> consumer_prog:string ->
  consumer_args:string list -> Core.Std.Unix.Exit_or_signal.t * Core.Std.Unix.Exit_or_signal.t
