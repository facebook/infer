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

let putenv ~key ~data = Caml_unix.putenv key data

module Pid = Pid
module Process_info = Core_unix.Process_info
module Env = Core_unix.Env

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

module Exit_or_signal = Core_unix.Exit_or_signal

let close_process_in ic = Exit_or_signal.of_unix (Caml_unix.close_process_in ic)

let getpid () = Pid.of_int (Caml_unix.getpid ())

let do_maybe_restart ~restart f =
  let rec retry_until_no_eintr f =
    try f () with Caml_unix.Unix_error (EINTR, _, _) -> retry_until_no_eintr f
  in
  if restart then retry_until_no_eintr f else f ()


let waitpid pid =
  do_maybe_restart ~restart:true (fun () ->
      let _, process_status = Caml_unix.waitpid [] (Pid.to_int pid) in
      Exit_or_signal.of_unix process_status )


let wait_nohang_any () =
  do_maybe_restart ~restart:true (fun () ->
      let pid, process_status = Caml_unix.waitpid [WNOHANG] (-1) in
      if Int.( = ) 0 pid then None else Some (Pid.of_int pid, Exit_or_signal.of_unix process_status) )


let fork () =
  let pid = Caml_unix.fork () in
  if Int.( < ) pid 0 then assert false
  else if Int.( = ) pid 0 then `In_the_child
  else `In_the_parent (Pid.of_int pid)


let symlink ~target ~link_name = Caml_unix.symlink target link_name

module File_descr = Core_unix.File_descr

let dup2 ?close_on_exec ~src ~dst () = Caml_unix.dup2 ?cloexec:close_on_exec src dst

let read ?(restart = true) ~pos ~len fd ~buf =
  do_maybe_restart ~restart (fun () -> Caml_unix.read fd buf pos len)


type file_perm = Caml_unix.file_perm

type open_flag = Caml_unix.open_flag

let openfile ?(perm = 0o644) ~mode filename = Caml_unix.openfile filename mode perm

type socket_domain = Caml_unix.socket_domain

type socket_type = Caml_unix.socket_type

let socket ?close_on_exec ~domain ~kind ~protocol () =
  Caml_unix.socket ?cloexec:close_on_exec domain kind protocol


type sockaddr = Caml_unix.sockaddr

let bind fd ~addr = Caml_unix.bind fd addr

let listen fd ~backlog = Caml_unix.listen fd backlog

module Select_fds = Core_unix.Select_fds

type select_timeout = Core_unix.select_timeout

let select ?(restart = false) ~read ~write ~except ~timeout () =
  let timeout =
    match timeout with
    | `Never ->
        -1.
    | `Immediately ->
        0.
    | `After span ->
        if Time_ns.Span.( < ) span Time_ns.Span.zero then 0. else Time_ns.Span.to_sec span
  in
  let read, write, except =
    do_maybe_restart ~restart (fun () -> Caml_unix.select read write except timeout)
  in
  {Select_fds.read; write; except}


let system s = Exit_or_signal.of_unix (Caml_unix.system s)

module Error = Core_unix.Error

type env = Core_unix.env [@@deriving sexp]
