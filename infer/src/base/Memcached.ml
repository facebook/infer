(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(** unix socket name, always relative to the master [results_dir] *)
let memcached_socket_relative = "memcached.socket"

(** find the results_dir of the top-level infer process *)
let results_dir =
  Sys.getenv Config.infer_top_results_dir_env_var |> Option.value ~default:Config.results_dir


(** log file for memcached *)
let memcached_log = results_dir ^ "/memcached.log"

(** binary name -- assumed to be on path *)
let memcached_bin = "memcached"

(** shell to use for redirecting memcached's output to the log *)
let shell = "sh"

type server = {input: In_channel.t; output: Out_channel.t}

(* Unix socket paths have a historical length limit of ~100 chars (!?*@&*$).  However, this applies 
   to the argument passed in the system call to create the socket.  Thus a workaround is to cd into 
   the parent dir of the socket and then create it, hence this function. *)
let in_results_dir ~f =
  let cwd = Unix.getcwd () in
  let () = Unix.chdir results_dir in
  let res = f () in
  let () = Unix.chdir cwd in
  res


let fail_on response_line = L.die InternalError "Unexpected server response: %s" response_line

let send server str = Out_channel.output_string server.output str

let eol = "\r\n"

let send_eol server = send server eol

let flush_server server = Out_channel.flush server.output

let send_line server str = send server str ; send_eol server ; flush_server server

let recv_line server =
  match In_channel.input_line ~fix_win_eol:true server.input with
  | None ->
      fail_on "No response"
  | Some line ->
      line


let expect_line server lines =
  match recv_line server with
  | x when List.exists lines ~f:(String.equal x) ->
      ()
  | response ->
      fail_on response


(** socket to memcached server *)
let server : server option ref = ref None

let with_server ~f =
  match !server with
  | None ->
      L.die InternalError "Asked to perform socket operation without live connection.@."
  | Some s ->
      f s


let flush_all () = with_server ~f:(fun s -> send_line s "flush_all")

let disconnect () =
  with_server ~f:(fun s ->
      server := None ;
      Unix.shutdown_connection s.input ;
      (* For some reason the below sometimes throws -- wrong docs? *)
      (* In_channel.close s.input ; *)
      Out_channel.close s.output )


let connect () =
  if Option.is_some !server then
    L.die InternalError "Asked to connect memcached while already connected.@." ;
  in_results_dir ~f:(fun () ->
      let input, output = Unix.open_connection (ADDR_UNIX memcached_socket_relative) in
      server := Some {input; output} )


let stats server =
  let rec aux acc = match recv_line server with "END" -> List.rev acc | l -> aux (l :: acc) in
  send_line server "stats" ; aux []


let stop pid () =
  connect () ;
  let stats = with_server ~f:stats in
  disconnect () ;
  Signal.send Signal.term (`Pid pid) |> ignore ;
  Unix.wait (`Pid pid) |> ignore ;
  in_results_dir ~f:(fun () -> Unix.remove memcached_socket_relative) ;
  Out_channel.(with_file memcached_log ~append:true ~f:(fun ch -> output_lines ch stats))


let start () =
  let socket_exists () =
    in_results_dir ~f:(fun () -> Sys.file_exists_exn memcached_socket_relative)
  in
  if Option.is_some !server then
    L.die InternalError "Connection is open but asked to start memcached.@." ;
  (* Unix sockets can be shadowed, so avoid creating a new socket/server if there is one already *)
  if socket_exists () then ()
  else
    let verbosity = match Config.debug_level_analysis with 0 -> "" | 1 -> "-v" | _ -> "-vv" in
    let cmd =
      Printf.sprintf "exec %s %s -Bascii -C -m%d -s%s > %s 2>&1" memcached_bin verbosity
        Config.memcached_size_mb memcached_socket_relative memcached_log
    in
    let pid = in_results_dir ~f:(Unix.fork_exec ~prog:shell ~argv:[shell; "-c"; cmd]) in
    (* Wait until socket is open -- NB this waits for 0.05s not 0.05ns *)
    while not (socket_exists ()) do
      Unix.nanosleep 0.05 |> ignore
    done ;
    Epilogues.register ~description:"Shutdown Memcached daemon" ~f:(stop pid)


module type Value = sig
  type t

  val label : string
end

module type Server = sig
  module Value : Value

  val get : key:string -> Value.t option

  val set : key:string -> Value.t -> unit
end

module Make (V : Value) : Server with module Value = V = struct
  module Value = V

  let set_ =
    let buffer = ref (Bytes.create 1024) in
    let rec try_to_buffer value =
      try Marshal.to_buffer !buffer 0 (Bytes.length !buffer) value [] with Failure _ ->
        (* double buffer length *)
        buffer := Bytes.create (2 * Bytes.length !buffer) ;
        try_to_buffer value
    in
    fun server ~key value ->
      let value_length = try_to_buffer value in
      Printf.fprintf server.output "set %s:%s 0 0 %d%s" Value.label key value_length eol ;
      Out_channel.output server.output ~buf:!buffer ~pos:0 ~len:value_length ;
      send_eol server ;
      flush_server server ;
      expect_line server ["STORED"; "SERVER_ERROR object too large for cache"]


  let get_ server ~key =
    Printf.fprintf server.output "get %s:%s%s" Value.label key eol ;
    flush_server server ;
    let value_line = recv_line server in
    match String.split value_line ~on:' ' with
    | ["END"] ->
        None
    | ["VALUE"; _key'; _flags; _bytes] ->
        let value : Value.t = Marshal.from_channel server.input in
        (* eat up the trailing eol *)
        expect_line server [""] ;
        expect_line server ["END"] ;
        Some value
    | _ ->
        fail_on value_line


  let get ~key = with_server ~f:(get_ ~key)

  (* TODO: do this on background thread to avoid blocking on the response *)
  let set ~key value = with_server ~f:(fun s -> set_ s ~key value)
end
