(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module L = Logging
module F = Format

(** Prints an error message to a log file, prints a message saying that the error can be
    found in that file, and exits, with default code 1 or a given code. *)
let print_error_and_exit ?(exit_code=1) fmt =
  F.kfprintf (fun _ ->
      L.do_err "%s" (F.flush_str_formatter ());
      let log_file = snd (L.log_file_names ()) in
      L.stderr "@\nAn error occured. Please find details in %s@\n@\n%!" log_file;
      exit exit_code
    )
    F.str_formatter fmt

(** Given a command to be executed, create a process to execute this command, and wait for it to
    terminate. The standard out and error are not redirected.  If the command fails to execute,
    print an error message and exit. *)
let create_process_and_wait ~prog ~args =
  Unix.fork_exec ~prog ~args:(prog :: args) ()
  |> Unix.waitpid
  |> function
  | Ok () -> ()
  | Error err as status ->
      L.do_err "Executing: %s@\n%s@\n"
        (String.concat ~sep:" " (prog :: args)) (Unix.Exit_or_signal.to_string_hum status) ;
      exit (match err with `Exit_non_zero i -> i | `Signal _ -> 1)

(** Given a process id and a function that describes the command that the process id
    represents, prints a message explaining the command and its status, if in debug or stats mode.
    It also prints a dot to show progress of jobs being finished.  *)
let print_status ~fail_on_failed_job f pid status =
  L.err "%a%s@."
    (fun fmt pid -> F.pp_print_string fmt (f pid)) pid
    (Unix.Exit_or_signal.to_string_hum status) ;
  L.stdout ".%!";
  match status with
  | Error err when fail_on_failed_job ->
      exit (match err with `Exit_non_zero i -> i | `Signal _ -> 1)
  | _ -> ()

let start_current_jobs_count () = ref 0

let waited_for_jobs = ref 0

module PidMap = Caml.Map.Make (Pid)

(** [wait_for_son pid_child f jobs_count] wait for pid_child
    and all the other children and update the current jobs count.
    Use f to print the job status *)
let rec wait_for_child ~fail_on_failed_job f current_jobs_count jobs_map =
  let pid, status = Unix.wait `Any in
  Pervasives.decr current_jobs_count;
  Pervasives.incr waited_for_jobs;
  print_status ~fail_on_failed_job f pid status;
  jobs_map := PidMap.remove pid !jobs_map;
  if not (PidMap.is_empty !jobs_map) then
    wait_for_child ~fail_on_failed_job f current_jobs_count jobs_map

let pid_to_program jobsMap pid =
  try
    PidMap.find pid jobsMap
  with Not_found -> ""

(** [run_jobs_in_parallel jobs_stack gen_prog prog_to_string] runs the jobs in the given stack, by
    spawning the jobs in batches of n, where n is [Config.jobs]. It then waits for all those jobs
    and starts a new batch and so on. [gen_prog] should return a tuple [(dir_opt, command, args,
    env)] where [dir_opt] is an optional directory to chdir to before executing [command] with
    [args] in [env]. [prog_to_string] is used for printing information about the job's status. *)
let run_jobs_in_parallel ?(fail_on_failed_job=false) jobs_stack gen_prog prog_to_string =
  let run_job () =
    let jobs_map = ref PidMap.empty in
    let current_jobs_count = start_current_jobs_count () in
    while not (Stack.is_empty jobs_stack) do
      let job_prog = Stack.pop_exn jobs_stack in
      let (dir_opt, prog, args, env) = gen_prog job_prog in
      Pervasives.incr current_jobs_count;
      match Unix.fork () with
      | `In_the_child ->
          Option.iter dir_opt ~f:Unix.chdir ;
          Unix.exec ~prog ~args:(prog :: args) ~env ~use_path:false
          |> Unix.handle_unix_error
          |> never_returns
      | `In_the_parent pid_child ->
          jobs_map := PidMap.add pid_child (prog_to_string job_prog) !jobs_map;
          if Int.equal (Stack.length jobs_stack) 0 || !current_jobs_count >= Config.jobs then
            wait_for_child ~fail_on_failed_job (pid_to_program !jobs_map) current_jobs_count
              jobs_map
    done in
  run_job ();
  L.stdout ".@.";
  L.out "Waited for %d jobs" !waited_for_jobs

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
      never_returns (Unix.exec ~prog:producer_prog ~args:producer_args ())
  | `In_the_parent producer_pid ->
      match Unix.fork () with
      | `In_the_child ->
          (* redirect consumer's stdin to pipe_in *)
          Unix.dup2 ~src:pipe_in ~dst:Unix.stdin ;
          (* close consumer's copy of pipe ends *)
          Unix.close pipe_out ;
          Unix.close pipe_in ;
          (* exec consumer *)
          never_returns (Unix.exec ~prog:consumer_prog ~args:consumer_args ())
      | `In_the_parent consumer_pid ->
          (* close parent's copy of pipe ends *)
          Unix.close pipe_out ;
          Unix.close pipe_in ;
          (* wait for children *)
          let producer_status = Unix.waitpid producer_pid in
          let consumer_status = Unix.waitpid consumer_pid in
          (producer_status, consumer_status)
