(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module CLOpt = CommandLineOption

let sanitize_infer_args infer_args =
  let rec remove_run_as_child acc = function
    | "--run-as-child" :: _ :: rest ->
        remove_run_as_child acc rest
    | arg :: rest ->
        remove_run_as_child (arg :: acc) rest
    | [] ->
        List.rev acc
  in
  let decode_arg = function
    | arg when String.is_prefix ~prefix:"@" arg ->
        In_channel.read_lines (String.drop_prefix arg 1)
    | arg ->
        [arg]
  in
  let args =
    if String.is_empty infer_args then []
    else String.split ~on:CLOpt.env_var_sep infer_args |> List.concat_map ~f:decode_arg
  in
  match remove_run_as_child [] args with
  | [] ->
      ""
  | sanitized_args ->
      let argfile = IFilename.temp_file "args" "" in
      Out_channel.write_lines argfile sanitized_args ;
      Utils.unlink_file_on_exit argfile ;
      "@" ^ argfile


let raw_env () =
  let infer_args = Sys.getenv CLOpt.args_env_var |> Option.value ~default:"" in
  let sanitized_infer_args = sanitize_infer_args infer_args in
  let other_env =
    Array.to_list (Unix.environment ())
    |> List.filter ~f:(fun entry ->
           not (String.is_prefix ~prefix:(CLOpt.args_env_var ^ "=") entry) )
  in
  (CLOpt.args_env_var ^ "=" ^ sanitized_infer_args) :: other_env


let with_devnull_stdin_stdout ~f =
  let stdin_null = IUnix.openfile ~mode:[O_RDONLY] "/dev/null" in
  let stdout_null = IUnix.openfile ~mode:[O_WRONLY] "/dev/null" in
  protect
    ~finally:(fun () ->
      Unix.close stdin_null ;
      Unix.close stdout_null )
    ~f:(fun () -> f ~stdin:stdin_null ~stdout:stdout_null)


let run ?cwd ~prog ~argv () =
  let cwd =
    Option.value_map cwd ~default:Spawn.Working_dir.Inherit ~f:(fun dir ->
        Spawn.Working_dir.Path dir )
  in
  with_devnull_stdin_stdout ~f:(fun ~stdin ~stdout ->
      match
        Spawn.spawn
          ~env:(Spawn.Env.of_list (raw_env ()))
          ~cwd ~prog ~argv ~stdin ~stdout ~stderr:Unix.stderr ()
      with
      | pid -> (
        match IUnix.waitpid (Pid.of_int pid) with
        | Ok () ->
            Ok ()
        | Error _ as exit_or_signal ->
            Error (IUnix.Exit_or_signal.to_string_hum exit_or_signal) )
      | exception Unix.Unix_error (err, f, arg) ->
          Error (Printf.sprintf "%s(%s): %s" f arg (IUnix.Error.message err)) )
