(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(* a request equipped with fields necessary to implement client blocking until the request is completed. *)
type request =
  { command: DBWriterCommand.t
  ; mutex: Error_checking_mutex.t
  ; mutable completed_flag: bool
  ; completed: Condition.t }

let make_request command =
  { command
  ; mutex= Error_checking_mutex.create ()
  ; completed_flag= false
  ; completed= Condition.create () }


let wait_for_completion request =
  let rec wait_loop () =
    if not request.completed_flag then (
      Condition.wait request.completed request.mutex ;
      wait_loop () )
  in
  Error_checking_mutex.critical_section request.mutex ~f:wait_loop


let request_queue = SafeQueue.create ()

let rec server_loop () =
  let request = SafeQueue.dequeue request_queue in
  let command = request.command in
  ( try
      DBWriterCommand.perform command ;
      (* signal completion to client *)
      Error_checking_mutex.critical_section request.mutex ~f:(fun () ->
          request.completed_flag <- true ;
          Condition.signal request.completed )
    with exn ->
      L.die InternalError "DBWriter thread crashed@\n%a@\n%s@." Exn.pp exn
        (Printexc.get_backtrace ()) ) ;
  match (command : DBWriterCommand.t) with
  | Terminate ->
      L.debug Analysis Quiet "Sqlite write daemon: terminating@." ;
      ExecutionDuration.log ~prefix:"dbwriter.store_sql" Analysis !DBWriterCommand.store_sql_time
  | _ ->
      server_loop ()


let domain_server () =
  Database.new_database_connections Primary ;
  server_loop ()


let send command =
  let request = make_request command in
  SafeQueue.enqueue request request_queue ;
  wait_for_completion request


let dbwriter_domain = ref None

let start () =
  dbwriter_domain := Some (Domain.spawn domain_server) ;
  send Start


let terminate () =
  send Terminate ;
  Option.iter !dbwriter_domain ~f:Domain.join


let perform cmd =
  match (cmd : DBWriterCommand.t) with
  | Start ->
      start ()
  | Terminate ->
      terminate ()
  | _ ->
      send cmd
