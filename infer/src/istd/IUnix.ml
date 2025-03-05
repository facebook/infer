(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module Pid = Pid

module Process_info = struct
  type t =
    { pid: Pid.t
    ; stdin: Caml_unix.file_descr
    ; stdout: Caml_unix.file_descr
    ; stderr: Caml_unix.file_descr }
end

module Env = struct
  type t =
    [ `Replace of (string * string) list
    | `Extend of (string * string) list
    | `Override of (string * string option) list
    | `Replace_raw of string list ]
  [@@deriving sexp]

  let current () =
    let base = Array.to_list (Caml_unix.environment ()) in
    List.map base ~f:(fun s -> String.lsplit2_exn s ~on:'=')


  let env_map env =
    let map_of_list list = String.Map.of_alist_reduce list ~f:(fun _ x -> x) in
    match env with
    | `Replace env ->
        map_of_list env
    | `Extend extend ->
        map_of_list (current () @ extend)
    | `Override overrides ->
        List.fold_left overrides
          ~init:(map_of_list (current ()))
          ~f:(fun acc (key, v) ->
            match v with None -> Map.remove acc key | Some data -> Map.set acc ~key ~data )


  let expand env =
    match env with
    | `Replace_raw env ->
        env
    | (`Replace _ | `Extend _ | `Override _) as env ->
        Map.fold (env_map env) ~init:[] ~f:(fun ~key ~data acc -> (key ^ "=" ^ data) :: acc)


  let expand_array env = Array.of_list (expand env)
end

module Select_fds = struct
  type t =
    { read: Caml_unix.file_descr list
    ; write: Caml_unix.file_descr list
    ; except: Caml_unix.file_descr list }
end

module Exit_or_signal = struct
  type error = [`Exit_non_zero of int | `Signal of int] [@@deriving compare, sexp]

  type t = (unit, error) Result.t [@@deriving compare, sexp]

  let of_unix (u : Caml_unix.process_status) =
    match u with
    | WEXITED i ->
        if Int.( = ) i 0 then Ok () else Error (`Exit_non_zero i)
    | WSIGNALED i ->
        Error (`Signal i)
    | WSTOPPED _ ->
        assert false


  let to_string_hum = function
    | Ok () ->
        "exited normally"
    | Error (`Exit_non_zero i) ->
        sprintf "exited with code %d" i
    | Error (`Signal s) ->
        sprintf "died after receiving signal number %d" s
end

module Error = struct
  type t = Caml_unix.error

  let message = Caml_unix.error_message
end

type select_timeout = [`Never | `Immediately | `After of Time_ns.Span.t]

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

let dup2 ?close_on_exec ~src ~dst () = Caml_unix.dup2 ?cloexec:close_on_exec src dst

let read ?(restart = true) ~pos ~len fd ~buf =
  do_maybe_restart ~restart (fun () -> Caml_unix.read fd buf pos len)


let openfile ?(perm = 0o644) ~mode filename = Caml_unix.openfile filename mode perm

let socket ?close_on_exec ~domain ~kind ~protocol () =
  Caml_unix.socket ?cloexec:close_on_exec domain kind protocol


let bind fd ~addr = Caml_unix.bind fd addr

let listen fd ~backlog = Caml_unix.listen fd backlog

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
