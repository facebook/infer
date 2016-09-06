(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Prints information about a unix error *)
let print_unix_error cmd e =
  match e with
  | Unix.Unix_error(err, _, _) ->
      Logging.err "Cannot execute %s : %s\n%!"
        cmd (Unix.error_message err)
  | _ -> ()

(** Prints an error message to a log file, prints a message saying that the error can be
    found in that file, and exits, with default code 1 or a given code. *)
let print_error_and_exit ?(exit_code=1) f el =
  Logging.do_err f el;
  let log_file = snd (Config.tmp_log_files_of_current_exe ()) in
  Logging.stderr "\nAn error occured. Please, find details in %s\n\n%!" log_file;
  exit exit_code

(** Executes a command and catches a potential exeption and prints it. *)
let exec_command cmd args env =
  try Unix.execvpe cmd args env
  with (Unix.Unix_error _ as e) ->
    print_unix_error cmd e

(** Given an command to be executed, creates a process to execute this command,
    and waits for it to terminate. The standard out and error are not redirected.
    If the commands fails to execute, prints an error message and exits. *)
let create_process_and_wait cmd =
  let pid = Unix.create_process cmd.(0) cmd Unix.stdin Unix.stdout Unix.stderr in
  let _, status = Unix.waitpid [] pid in
  let exit_code = match status with
    | Unix.WEXITED i -> i
    | _ -> 1 in
  if exit_code <> 0 then
    print_error_and_exit ~exit_code:exit_code
      "Failed to execute: %s" (String.concat " " (Array.to_list cmd))

(** Given a process id and a function that describes the command that the process id
    represents, prints a message explaining the command and its status, if in debug or stats mode.
    It also prints a dot to show progress of jobs being finished.  *)
let print_status f pid (status : Unix. process_status) =
  if Config.debug_mode || Config.stats_mode then
    (let program = f pid in
     match status with
     | WEXITED status ->
         if status = 0 then
           Logging.out "%s OK \n%!" program
         else
           Logging.err "%s exited with code %d\n%!" program status
     | WSIGNALED signal ->
         Logging.err "%s killed by signal %d\n%!" program signal
     | WSTOPPED _ ->
         Logging.err "%s stopped \n%!" program);
  Logging.stdout ".%!"

let start_current_jobs_count () = ref 0

let waited_for_jobs = ref 0

(** [wait_for_son pid_child f jobs_count] wait for pid_child
    and all the other children and update the current jobs count.
    Use f to print the job status *)
let rec wait_for_child pid_child f current_jobs_count =
  let pid, status = Unix.wait () in
  Pervasives.decr current_jobs_count;
  Pervasives.incr waited_for_jobs;
  print_status f pid status;
  if pid <> pid_child then
    wait_for_child pid_child f current_jobs_count

let pid_to_program jobsMap pid =
  try
    IntMap.find pid jobsMap
  with Not_found -> ""

(** [run_jobs_in_parallel jobs_stack run_job cmd_to_string ] runs the jobs in
    the given stack, by spawning the jobs in batches of n, where n is Config.jobs. It
    then waits for all those jobs and starts a new batch and so on. cmd_to_string
    is used for printing information about the job's status.   *)
let run_jobs_in_parallel jobs_stack run_job cmd_to_string =
  let run_job () =
    let jobs_map = ref IntMap.empty in
    let current_jobs_count = start_current_jobs_count () in
    while not (Stack.is_empty jobs_stack) do
      let job_cmd = Stack.pop jobs_stack in
      Pervasives.incr current_jobs_count;
      match Unix.fork () with
      | 0 ->
          run_job job_cmd
      | pid_child ->
          jobs_map := IntMap.add pid_child (cmd_to_string job_cmd) !jobs_map;
          if Stack.length jobs_stack = 0 || !current_jobs_count >= Config.jobs then
            wait_for_child pid_child (pid_to_program !jobs_map) current_jobs_count
    done in
  run_job ();
  Logging.stdout ".\n%!";
  Logging.out "Waited for %d jobs" !waited_for_jobs
