(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let rename ~src ~dst = Caml_unix.rename src dst

let mkdir_p ?(perm = 0o777) name =
  let mkdir_idempotent ~perm dirname =
    try Caml_unix.mkdir dirname perm
    with
    (* [mkdir] on MacOSX returns [EISDIR] instead of [EEXIST] if the directory already
        exists. *)
    | Caml_unix.Unix_error ((EEXIST | EISDIR), _, _) ->
      ()
  in
  let rec mkdir_p ~perm dir =
    try mkdir_idempotent ~perm dir
    with Caml_unix.Unix_error (ENOENT, _, _) as exn ->
      let parent = Filename.dirname dir in
      if Filename.equal parent dir then raise exn
      else (
        mkdir_p ~perm parent ;
        mkdir_idempotent ~perm dir )
  in
  mkdir_p name ~perm


let nanosleep nanoseconds = Caml_unix.sleepf (nanoseconds *. 1_000_000_000.)

let readdir_opt dir_handle = try Some (Caml_unix.readdir dir_handle) with End_of_file -> None

let mkdtemp prefix = Filename.temp_dir prefix ""

let putenv ~key ~data = Caml_unix.putenv key data

module Pid = Pid
module Process_info = Unix.Process_info
module Env = Unix.Env

let create_process_env ~prog ~args ~env =
  let in_read, in_write = Caml_unix.pipe () in
  let out_read, out_write = Caml_unix.pipe () in
  let err_read, err_write = Caml_unix.pipe () in
  let args_array = Array.of_list (prog :: args) in
  let env_array = Env.expand_array env in
  let pid =
    Caml_unix.create_process_env prog args_array env_array in_read out_write err_write |> Pid.of_int
  in
  Caml_unix.close in_read ;
  Caml_unix.close out_write ;
  Caml_unix.close err_write ;
  Process_info.{pid; stdin= in_write; stdout= out_read; stderr= err_read}


let create_process ~prog ~args = create_process_env ~prog ~args ~env:(`Extend [])

let open_process_in = Caml_unix.open_process_in

module Exit_or_signal = Unix.Exit_or_signal

let close_process_in ic = Exit_or_signal.of_unix (Caml_unix.close_process_in ic)

let getpid () = Pid.of_int (Caml_unix.getpid ())
