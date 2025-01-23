(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module ServerSocket = struct
  let socket_name = ResultsDirEntryName.db_writer_socket_name

  let socket_path = Config.toplevel_results_dir ^/ socket_name

  let socket_addr = Unix.ADDR_UNIX socket_name

  let socket_domain = Unix.domain_of_sockaddr socket_addr

  (** Unix socket *paths* have a historical length limit of ~100 chars (!?*@&*$). However, this only
      applies to the argument passed in the system call to create the socket, not to the actual
      path. Thus a workaround is to cd into the parent dir of the socket and then use it, hence this
      function. *)
  let in_results_dir ~f = Utils.do_in_dir ~dir:Config.toplevel_results_dir ~f

  let socket_exists () = Sys.file_exists_exn socket_path

  (* Error recuperation is done by attempting this function at module initialization time, and
     not using DbWriter at all in case it fails. See {!can_use_socket} below. *)
  let setup_socket () =
    if socket_exists () then L.die InternalError "Sqlite write daemon: socket already exists@." ;
    let socket = Unix.socket ~domain:socket_domain ~kind:SOCK_STREAM ~protocol:0 () in
    in_results_dir ~f:(fun () -> Unix.bind socket ~addr:socket_addr) ;
    (* [backlog] is (supposedly) the length of the queue for pending connections ;
       there are no rules about the implied behaviour though.  Here use optimistically
       the number of workers, though even that is a guess. *)
    Unix.listen socket ~backlog:Config.jobs ;
    socket


  let remove_socket_file () = if socket_exists () then Unix.unlink socket_path

  let remove_socket socket =
    Unix.close socket ;
    remove_socket_file ()


  (* Check whether we can create a socket to communicate with the asynchronous DBWriter process. *)
  let can_use_socket () =
    try
      (* This function is called very early, and the infer-out directory may not have been created
         yet. Ensure it exists before attempting to create the socket. *)
      Unix.mkdir_p Config.toplevel_results_dir ;
      let socket = setup_socket () in
      remove_socket socket ;
      true
    with _ -> false
end

let default_use_daemon =
  lazy
    (let is_windows =
       match Version.build_platform with Windows -> true | Linux | Darwin -> false
     in
     Config.((not is_windows) && dbwriter && (not (buck || genrule_mode)) && jobs > 1)
     &&
     (* Only the main process should try detecting whether the socket can be created.
        Otherwise, re-spawned Infer will try to create a socket on top of the existing one. *)
     if Config.is_originator then (
       let socket_ok = ServerSocket.can_use_socket () in
       if not socket_ok then
         L.user_warning
           "Cannot setup the socket to communicate with the database daemon. Performance will be \
            impacted. Do you have enough rights to create a Unix socket in directory '%s'?@."
           Config.toplevel_results_dir ;
       socket_ok )
     else ServerSocket.socket_exists () )


let override_daemon_ref = ref None

let override_use_daemon should_use = override_daemon_ref := Some should_use

let use_daemon () =
  Option.value_or_thunk !override_daemon_ref ~default:(fun () -> Lazy.force default_use_daemon)


type response = Ack | Error of (exn * Stdlib.Printexc.raw_backtrace)

let server_pid : Pid.t option ref = ref None

module Server = struct
  (* General comment about socket/channel destruction: closing the in_channel associated with the socket
     will close the file descriptor too, so closing also the out_channel sometimes throws an exception.
     That's why in all code below only the input channel is ever closed. *)

  let log_store_pct useful_time =
    let label = "dbwriter.store_sql_pct" in
    let total_useful = ExecutionDuration.total_useful_s useful_time in
    let total_store = ExecutionDuration.total_useful_s !DBWriterCommand.store_sql_time in
    let store_pct = 100.0 *. total_store /. total_useful |> Float.round |> int_of_float in
    L.debug Analysis Quiet "%s= %d%%@\n" label store_pct ;
    StatsLogging.log_count ~label ~value:store_pct


  let rec server_loop ?(useful_time = ExecutionDuration.zero) socket =
    let client_sock, _client = Unix.accept socket in
    let in_channel = Unix.in_channel_of_descr client_sock
    and out_channel = Unix.out_channel_of_descr client_sock in
    let now = ExecutionDuration.counter () in
    let command : DBWriterCommand.t = Marshal.from_channel in_channel in
    ( try
        DBWriterCommand.perform command ;
        Marshal.to_channel out_channel Ack []
      with exn ->
        Marshal.to_channel out_channel (Error (exn, Stdlib.Printexc.get_raw_backtrace ())) [] ) ;
    Out_channel.flush out_channel ;
    In_channel.close in_channel ;
    let useful_time = ExecutionDuration.add_duration_since useful_time now in
    match command with
    | Terminate ->
        L.debug Analysis Quiet "Sqlite write daemon: terminating@." ;
        ExecutionDuration.log ~prefix:"dbwriter.useful_time" Analysis useful_time ;
        ExecutionDuration.log ~prefix:"dbwriter.store_sql" Analysis !DBWriterCommand.store_sql_time ;
        log_store_pct useful_time ;
        ()
    | _ ->
        server_loop ~useful_time socket


  let server socket =
    L.debug Analysis Quiet "Sqlite write daemon: process starting, pid= %a@." Pid.pp
      (Unix.getpid ()) ;
    let finally () = ServerSocket.remove_socket socket in
    Exception.try_finally ~finally ~f:(fun () ->
        let ExecutionDuration.{execution_duration} =
          ExecutionDuration.timed_evaluate ~f:(fun () -> server_loop socket)
        in
        ExecutionDuration.log ~prefix:"dbwriter.total_time" Analysis execution_duration )


  let send cmd =
    let in_channel, out_channel =
      ServerSocket.in_results_dir ~f:(fun () -> Unix.open_connection ServerSocket.socket_addr)
    in
    Marshal.to_channel out_channel cmd [Closures] ;
    Out_channel.flush out_channel ;
    let (response : response) = Marshal.from_channel in_channel in
    ( match response with
    | Ack ->
        ()
    | Error (exn, exn_backtrace) ->
        Stdlib.Printexc.raise_with_backtrace exn exn_backtrace ) ;
    In_channel.close in_channel


  let start () =
    L.debug Analysis Quiet "Sqlite write daemon: starting up@." ;
    let socket = ServerSocket.setup_socket () in
    L.debug Analysis Quiet "Sqlite write daemon: set up complete, waiting for connections@." ;
    match Unix.fork () with
    | `In_the_child ->
        ForkUtils.protect ~f:server socket ;
        L.exit 0
    | `In_the_parent child_pid ->
        server_pid := Some child_pid ;
        send DBWriterCommand.Start
end

let remove_socket_file () = ServerSocket.remove_socket_file ()

let terminate () =
  (try Server.send DBWriterCommand.Terminate with Unix.Unix_error _ -> ()) ;
  (* don't terminate the main infer process before the server has finished *)
  Option.iter !server_pid ~f:(fun server_pid ->
      L.debug Analysis Quiet "Sqlite write daemon: waiting for process %a to finish@." Pid.pp
        server_pid ;
      try
        let pid, exit_or_signal = Unix.wait (`Pid server_pid) in
        Result.iter_error exit_or_signal ~f:(function error ->
            ( L.internal_error "ERROR: Sqlite write daemon terminated with an error: %s@\n"
                (Core_unix.Exit_or_signal.to_string_hum (Error error)) ;
              try ServerSocket.remove_socket_file () with Unix.Unix_error _ -> () ) ) ;
        L.debug Analysis Quiet "Sqlite write daemon: process %a terminated@." Pid.pp pid
      with Unix.Unix_error _ -> () )


let start =
  let already_started = ref false in
  fun () ->
    if (not !already_started) && Config.is_originator then (
      remove_socket_file () ;
      if use_daemon () then (
        Server.start () ;
        Epilogues.register ~f:terminate ~description:"Stop Sqlite write daemon" ;
        already_started := true ) )


let perform cmd =
  match (cmd : DBWriterCommand.t) with
  | Start ->
      start ()
  | Terminate ->
      terminate ()
  | _ ->
      Server.send cmd
