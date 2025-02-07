(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

module Worker = struct
  (** the number matches the index of the worker in the array of final results *)
  type id = int [@@deriving show]
end

(** messages from the parent process down to worker processes *)
type 'work command =
  | Do of 'work
      (** [Do x] is sent only when the worker is [Idle], and moves worker state to [Processing x] *)
  | GoHome  (** all tasks done, prepare for teardown *)

type 'result worker_message =
  | Crash of {worker_id: Worker.id; exn_backtrace: string}
      (** there was an error and the child is no longer receiving messages *)
  | Ready of {worker_id: Worker.id; result: 'result option}
      (** Sent after finishing a given task. When received by the orchestrator, this moves the
          worker state from [Processing _] to [Idle]. *)

type 'work worker_state = Idle | Processing of {work: 'work}

type ('work, 'final) worker =
  { domain: 'final Domain.t
  ; command_queue: 'work command Concurrent.Queue.t
  ; mutable state: 'work worker_state }

type ('work, 'final, 'result) t =
  { jobs: int
  ; workers: ('work, 'final) worker Array.t
  ; message_queue: 'result worker_message Concurrent.Queue.t
  ; tasks: ('work, 'result, WorkerPoolState.worker_id) TaskGenerator.t }

let rec child_loop ~f ~command_queue ~message_queue worker_id =
  match Concurrent.Queue.dequeue command_queue with
  | GoHome ->
      ()
  | Do work ->
      let go_on =
        try
          let result = f work in
          Concurrent.Queue.enqueue (Ready {worker_id; result}) message_queue ;
          true
        with
        | exn when Config.keep_going ->
            L.internal_error "Error in worker %d: %a@." worker_id Exn.pp exn ;
            Concurrent.Queue.enqueue (Ready {worker_id; result= None}) message_queue ;
            true
        | _exn ->
            let crash = Crash {worker_id; exn_backtrace= Printexc.get_backtrace ()} in
            Concurrent.Queue.enqueue crash message_queue ;
            false
      in
      if go_on then child_loop ~f ~command_queue ~message_queue worker_id


let child ~f ~child_prologue ~child_epilogue ~command_queue ~message_queue worker_id =
  Printexc.record_backtrace true ;
  Database.new_database_connections Primary ;
  child_prologue worker_id ;
  child_loop ~f ~command_queue ~message_queue worker_id ;
  child_epilogue worker_id


let create :
       jobs:int
    -> child_prologue:(Worker.id -> unit)
    -> f:('work -> 'result option)
    -> child_epilogue:(Worker.id -> 'final)
    -> tasks:(unit -> ('work, 'result, WorkerPoolState.worker_id) TaskGenerator.t)
    -> ('work, 'final, 'result) t =
 fun ~jobs ~child_prologue ~f ~child_epilogue ~tasks ->
  DBWriter.terminate () ;
  DBWriter.use_multicore := true ;
  DBWriter.start () ;
  let message_queue = Concurrent.Queue.create () in
  let workers =
    Array.init jobs ~f:(fun id ->
        let command_queue = Concurrent.Queue.create () in
        let domain =
          Domain.spawn (fun () ->
              WorkerPoolState.set_in_child (Some id) ;
              child ~child_prologue ~child_epilogue ~f ~command_queue ~message_queue id )
        in
        {domain; command_queue; state= Idle} )
  in
  {jobs; workers; message_queue; tasks= tasks ()}


let handle_worker_message pool = function
  | Crash {worker_id; exn_backtrace} ->
      L.die InternalError "DomainPool: worker %d crashed with backtrace:@\n%s@." worker_id
        exn_backtrace
  | Ready {worker_id; result} -> (
    match pool.workers.(worker_id).state with
    | Idle ->
        L.die InternalError "DomainPool: Received Ready from an idle worker@."
    | Processing {work} ->
        pool.tasks.finished ~result work ;
        pool.workers.(worker_id).state <- Idle )


let rec handle_all_messages pool =
  match Concurrent.Queue.dequeue_opt pool.message_queue with
  | None ->
      ()
  | Some message ->
      handle_worker_message pool message ;
      handle_all_messages pool


let send_work_to_idle_workers pool =
  let exception NoMoreWork in
  let is_first_update_ref = ref true in
  let send_work_to_child pool worker_id =
    let is_first_update = !is_first_update_ref in
    is_first_update_ref := false ;
    match
      pool.tasks.next
        {child_slot= worker_id; child_id= WorkerPoolState.Domain worker_id; is_first_update}
    with
    | None ->
        raise_notrace NoMoreWork
    | Some work ->
        pool.workers.(worker_id).state <- Processing {work} ;
        Concurrent.Queue.enqueue (Do work) pool.workers.(worker_id).command_queue
  in
  try
    Array.iteri pool.workers ~f:(fun worker_id {state} ->
        match state with Idle -> send_work_to_child pool worker_id | Processing _ -> () )
  with NoMoreWork -> ()


let some_workers_processing pool =
  Array.exists pool.workers ~f:(fun {state} ->
      match state with Idle -> false | Processing _ -> true )


let do_compaction_if_needed =
  let last_compaction_time = ref (Mtime_clock.counter ()) in
  let min_compaction_interval = Mtime.Span.(1 * s) in
  let compaction_if_heap_greater_equal_to_words =
    Config.compaction_if_heap_greater_equal_to_GB_multicore * 1024 * 1024
    * (1024 / Sys.word_size_in_bits)
  in
  let min_time_elapsed_between_compactions () =
    let elapsed = Mtime_clock.count !last_compaction_time in
    Mtime.Span.compare elapsed min_compaction_interval > 0
  in
  let do_compaction_if_needed () =
    if min_time_elapsed_between_compactions () then
      let stat = Stdlib.Gc.quick_stat () in
      let heap_words = stat.Stdlib.Gc.heap_words in
      if heap_words >= compaction_if_heap_greater_equal_to_words then (
        L.debug Analysis Quiet "Triggering compaction, heap size= %d GB@\n"
          (heap_words * Sys.word_size_in_bits / 1024 / 1024 / 1024) ;
        Gc.compact () ;
        last_compaction_time := Mtime_clock.counter () )
  in
  do_compaction_if_needed


let rec run pool =
  if pool.tasks.is_empty () then (
    Array.iter pool.workers ~f:(fun {command_queue} ->
        Concurrent.Queue.enqueue GoHome command_queue ) ;
    Array.init (Array.length pool.workers) ~f:(fun worker_id ->
        Domain.join pool.workers.(worker_id).domain |> Option.some ) )
  else (
    handle_all_messages pool ;
    send_work_to_idle_workers pool ;
    if some_workers_processing pool then Concurrent.Queue.wait_until_non_empty pool.message_queue
    else Domain.cpu_relax () ;
    do_compaction_if_needed () ;
    run pool )
