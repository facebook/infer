(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module TaskGenerator = struct
  type 'a t =
    { remaining_tasks: unit -> int
    ; is_empty: unit -> bool
    ; finished: 'a -> unit
    ; next: unit -> 'a option }

  let chain (gen1 : 'a t) (gen2 : 'a t) : 'a t =
    let remaining_tasks () = gen1.remaining_tasks () + gen2.remaining_tasks () in
    let gen1_returned_empty = ref false in
    let gen1_is_empty () =
      gen1_returned_empty := !gen1_returned_empty || gen1.is_empty () ;
      !gen1_returned_empty
    in
    let is_empty () = gen1_is_empty () && gen2.is_empty () in
    let finished x = if gen1_is_empty () then gen2.finished x else gen1.finished x in
    let next x = if gen1_is_empty () then gen2.next x else gen1.next x in
    {remaining_tasks; is_empty; finished; next}


  let of_list (lst : 'a list) : 'a t =
    let content = ref lst in
    let length = ref (List.length lst) in
    let remaining_tasks () = !length in
    let is_empty () = List.is_empty !content in
    let finished _finished_item = decr length in
    let next () =
      match !content with
      | [] ->
          None
      | x :: xs ->
          content := xs ;
          Some x
    in
    {remaining_tasks; is_empty; finished; next}
end

let log_or_die fmt = if Config.keep_going then L.internal_error fmt else L.die InternalError fmt

type child_info = {pid: Pid.t; down_pipe: Out_channel.t}

(** The master's abstraction of state for workers.
    See [worker_message] and [boss_message] below for transitions between states.
    - [Initializing] is the state a newly-forked worker is in.
    - [Idle] is the state a worker goes to after it finishes initializing, or finishes processing a work item.
    - [Processing x] means the worker is currently processing [x].
*)
type 'a child_state = Initializing | Idle | Processing of 'a

(** the state of the pool *)
type ('work, 'final) t =
  { jobs: int
        (** number of jobs running in parallel, i.e. number of children we are responsible for *)
  ; slots: child_info Array.t
        (** array of child processes with their pids and channels we can use to send work down to
            each child *)
  ; children_states: 'work child_state Array.t  (** array tracking the state of each worker *)
  ; children_updates: Unix.File_descr.t
        (** all the children send updates up the same pipe to the pool *)
  ; task_bar: TaskBar.t
  ; tasks: 'work TaskGenerator.t  (** generator for work remaining to be done *)
  ; file_lock: Utils.file_lock  (** file lock for sending worker messages *) }

(** {2 Constants} *)

(** refresh rate of the task bar (worst case: it also refreshes on children updates)
    this is now mandatory to allow checking for new work packets, when none were
    previously available *)
let refresh_timeout =
  let frames_per_second = 12 in
  `After (Time_ns.Span.of_int_ms (1_000 / frames_per_second))


(** size of the buffer for communicating with children --standard pipe buffer size *)
let buffer_size = 65_535

(** {2 parmap} *)

(** Messages from child processes to the parent process. Each message includes the identity of the
    child sending the process as its index (slot) in the array [pool.slots].

    LIMITATION: the messages must not be bigger than [buffer_size] once marshalled, or reading from
    the pipe will crash in the parent process. This is a limitation of the way we read from the pipe
    for now. To lift it, it should be possible to extend the buffer to the required length if we
    notice that we are trying to read more than [buffer_size] for example. *)
type worker_message =
  | UpdateStatus of int * Mtime.t * string
      (** [(i, t, status)]: starting a task from slot [i], at start time [t], with description
          [status]. Watch out that [status] must not be too close in length to [buffer_size]. *)
  | Ready of int
      (** Sent after finishing initializing or after finishing a given task.
          When received by master, this moves the worker state from [Initializing] or [Processing _] to [Idle]. *)
  | Crash of int  (** there was an error and the child is no longer receiving messages *)

(** messages from the parent process down to worker processes *)
type 'a boss_message =
  | Do of 'a
      (** [Do x] is sent only when the worker is [Idle], and moves worker state to [Processing x]  *)
  | GoHome  (** all tasks done, prepare for teardown *)

(** convenience function to send data down pipes without forgetting to flush *)

let marshal_to_pipe ?file_lock fd x =
  PerfEvent.log (fun logger ->
      PerfEvent.log_begin_event logger ~categories:["sys"] ~name:"send to pipe" () ) ;
  Option.iter file_lock ~f:(fun {Utils.lock} -> lock ()) ;
  Marshal.to_channel fd x [] ;
  (* Channel flush should be inside the critical section. *)
  Out_channel.flush fd ;
  Option.iter file_lock ~f:(fun {Utils.unlock} -> unlock ()) ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


(** like [Unix.read] but reads until [len] bytes have been read *)
let rec really_read ?(pos = 0) ~len fd ~buf =
  if len <= 0 then ()
  else
    let read = Unix.read ~pos ~len fd ~buf in
    if Int.equal read 0 then raise End_of_file ;
    really_read ~pos:(pos + read) ~len:(len - read) fd ~buf


(** return a list of all updates coming from workers. The first update is expected for up to the
    timeout [refresh_timeout].  After that, all already received updates are consumed but with zero timeout.
    If there is none left, return the list. *)
let wait_for_updates pool buffer =
  let rec aux acc ~timeout =
    let file_descr = pool.children_updates in
    (* Use select(2) so that we can both wait on the pipe of children updates and wait for a
       timeout. The timeout is for giving a chance to the taskbar of refreshing from time to time,
       as well as for checking for new work where none were previously available. *)
    let {Unix.Select_fds.read= read_fds} =
      Unix.select ~read:[file_descr] ~write:[] ~except:[] ~timeout ()
    in
    match read_fds with
    | _ :: _ :: _ ->
        assert false
    | [] ->
        (* no updates, break loop *) acc
    | [_file_descr] ->
        (* Read one OCaml value at a time. This is done by first reading the header of the marshalled
           value (fixed size), then get the total size of the data from that header, then request a
           read of the full OCaml value.

           This way the buffer is used for only one OCaml value at a time. This is simpler (values do
           not overlap across the end of a read and the beginning of another) and means we do not need
           a large buffer as long as messages are never bigger than the buffer.

           This works somewhat like [Marshal.from_channel] but uses the file descriptor directly
           instead of an [in_channel]. Do *not* read from the pipe via an [in_channel] as they read
           as much as possible eagerly. This can empty the pipe without us having a way to tell that
           there is more to read anymore since the [select] call will return that there is nothing to
           read. *)
        really_read pool.children_updates ~buf:buffer ~len:Marshal.header_size ;
        let data_size = Marshal.data_size buffer 0 in
        really_read pool.children_updates ~buf:buffer ~pos:Marshal.header_size ~len:data_size ;
        aux (Marshal.from_bytes buffer 0 :: acc) ~timeout:`Immediately
  in
  aux [] ~timeout:refresh_timeout |> List.rev


let wait_for_updates pool buffer =
  PerfEvent.log (fun logger ->
      PerfEvent.log_begin_event logger ~categories:["sys"] ~name:"wait for event" () ) ;
  let update = wait_for_updates pool buffer in
  PerfEvent.(log (fun logger -> log_end_event logger ())) ;
  update


let killall pool ~slot status =
  Array.iter pool.slots ~f:(fun {pid} ->
      match Signal.send Signal.term (`Pid pid) with `Ok | `No_such_process -> () ) ;
  Array.iter pool.slots ~f:(fun {pid} ->
      try Unix.wait (`Pid pid) |> ignore
      with Unix.Unix_error (ECHILD, _, _) ->
        (* some children may have died already, it's fine *) () ) ;
  L.die InternalError "Subprocess %d: %s" slot status


let has_dead_child pool =
  Unix.wait_nohang `Any
  |> Option.map ~f:(fun (dead_pid, status) ->
         ( Array.find_mapi_exn pool.slots ~f:(fun slot {pid} ->
               if Pid.equal pid dead_pid then Some slot else None )
         , status ) )


let child_is_idle = function Idle -> true | _ -> false

let all_children_idle pool = Array.for_all pool.children_states ~f:child_is_idle

let send_work_to_child pool slot =
  assert (child_is_idle pool.children_states.(slot)) ;
  pool.tasks.next ()
  |> Option.iter ~f:(fun x ->
         let {down_pipe} = pool.slots.(slot) in
         pool.children_states.(slot) <- Processing x ;
         marshal_to_pipe down_pipe (Do x) )


(* this should not be called in any other arch than Linux *)
let should_throttle =
  let currently_throttled = ref false in
  fun threshold ->
    ( match Utils.get_available_memory_MB () with
    | None ->
        L.die UserError "Can't obtain available memory even though oom detection was requested.@."
    | Some available_memory when available_memory < threshold ->
        if not !currently_throttled then
          L.user_warning
            "Available memory (%d MB) is below configured threshold, throttling back scheduling \
             analysis work.@."
            available_memory ;
        currently_throttled := true
    | Some available_memory ->
        if !currently_throttled then
          L.user_warning
            "Available memory (%d MB) exceeds configured threshold, resuming scheduling analysis \
             work.@."
            available_memory ;
        currently_throttled := false ) ;
    !currently_throttled


let send_work_to_child pool slot =
  let throttled = Option.exists Config.oom_threshold ~f:should_throttle in
  if not throttled then send_work_to_child pool slot


(** main dispatch function that responds to messages from worker processes and updates the taskbar
   periodically *)
let process_updates pool buffer =
  (* abort everything if some child has died unexpectedly *)
  has_dead_child pool
  |> Option.iter ~f:(fun (slot, status) ->
         killall pool ~slot (Unix.Exit_or_signal.to_string_hum status) ) ;
  wait_for_updates pool buffer
  |> List.iter ~f:(function
       | UpdateStatus (slot, t, status) ->
           TaskBar.update_status pool.task_bar ~slot t status
       | Crash slot ->
           (* NOTE: the workers only send this message if {!Config.keep_going} is not [true] so if
              we receive it we know we should fail hard *)
           let {pid} = pool.slots.(slot) in
           (* clean crash, give the child process a chance to cleanup *)
           Unix.wait (`Pid pid) |> ignore ;
           killall pool ~slot "see backtrace above"
       | Ready slot ->
           ( match pool.children_states.(slot) with
           | Processing work ->
               pool.tasks.finished work
           | Initializing ->
               ()
           | Idle ->
               L.die InternalError "Received a Ready message from an idle worker@." ) ;
           TaskBar.set_remaining_tasks pool.task_bar (pool.tasks.remaining_tasks ()) ;
           TaskBar.update_status pool.task_bar ~slot (Mtime_clock.now ()) "idle" ;
           pool.children_states.(slot) <- Idle ) ;
  (* try to schedule more work if there are idle workers *)
  if not (pool.tasks.is_empty ()) then
    Array.iteri pool.children_states ~f:(fun slot state ->
        match state with Idle -> send_work_to_child pool slot | Initializing | Processing _ -> () )


type 'a final_worker_message = Finished of int * 'a option | FinalCrash of int

let collect_results (pool : (_, 'final) t) =
  let failed = ref false in
  let updates_in = Unix.in_channel_of_descr pool.children_updates in
  (* use [Array.init] just to collect n messages, the order in the array will not be the same as the
     slots of the workers but that's ok *)
  Array.init pool.jobs ~f:(fun i ->
      if !failed then None
      else
        match (Marshal.from_channel updates_in : 'final final_worker_message) with
        | exception (End_of_file | Failure _) ->
            failed := true ;
            log_or_die "@[<v>error reading %dth final values from children@]%!" i ;
            None
        | FinalCrash slot ->
            (* NOTE: the workers only send this message if {!Config.keep_going} is not [true] so if
               we receive it we know we should fail hard *)
            killall pool ~slot "see backtrace above"
        | Finished (_slot, data) ->
            data )


(** terminate all worker processes *)
let wait_all pool =
  (* tell each alive worker to go home *)
  Array.iter pool.slots ~f:(fun {down_pipe} ->
      marshal_to_pipe down_pipe GoHome ; Out_channel.close down_pipe ) ;
  let results = collect_results pool in
  (* wait(2) workers one by one; the order doesn't matter since we want to wait for all of them
     eventually anyway. *)
  let errors =
    Array.foldi ~init:[] pool.slots ~f:(fun slot errors {pid} ->
        match Unix.wait (`Pid pid) with
        | _pid, Ok () ->
            errors
        | _pid, (Error _ as status) ->
            (* Collect all children errors and die only at the end to avoid creating zombies. *)
            (slot, status) :: errors )
  in
  ( if not (List.is_empty errors) then
    let pp_error f (slot, status) =
      F.fprintf f "Error in infer subprocess %d: %s@." slot
        (Unix.Exit_or_signal.to_string_hum status)
    in
    log_or_die "@[<v>%a@]%!" (Pp.seq ~print_env:Pp.text_break ~sep:"" pp_error) errors ) ;
  results


(** worker loop: wait for tasks and run [f] on them until we are told to go home *)
let rec child_loop ~slot send_to_parent send_final receive_from_parent ~f ~epilogue =
  send_to_parent (Ready slot) ;
  match receive_from_parent () with
  | GoHome -> (
    match epilogue () with
    | data ->
        send_final (Finished (slot, Some data))
    | exception e ->
        IExn.reraise_if e ~f:(fun () ->
            if Config.keep_going then (
              L.internal_error "Error running epilogue in subprocess %d: %a@." slot Exn.pp e ;
              send_final (Finished (slot, None)) ;
              false )
            else (
              (* crash hard, but first let the master know that we have crashed *)
              send_final (FinalCrash slot) ;
              true ) ) )
  | Do stuff ->
      ( try f stuff
        with e ->
          IExn.reraise_if e ~f:(fun () ->
              if Config.keep_going then (
                L.internal_error "Error in subprocess %d: %a@." slot Exn.pp e ;
                (* do not raise and continue accepting jobs *)
                false )
              else (
                (* crash hard, but first let the master know that we have crashed *)
                send_to_parent (Crash slot) ;
                true ) ) ) ;
      child_loop ~slot send_to_parent send_final receive_from_parent ~f ~epilogue


(** Fork a new child and start it so that it is ready for work.

    The child inherits [updates_w] to send updates up to the parent, and a new pipe is set up for
    the parent to send instructions down to the child. *)
let fork_child ~file_lock ~child_prelude ~slot (updates_r, updates_w) ~f ~epilogue =
  let to_child_r, to_child_w = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
      let[@warning "-26"] updates_r = Unix.close updates_r in
      let[@warning "-26"] to_child_w = Unix.close to_child_w in
      (* Pin to a core. [setcore] does the modulo <number of cores> for us. *)
      Setcore.setcore slot ;
      ProcessPoolState.in_child := true ;
      ProcessPoolState.reset_pid () ;
      child_prelude () ;
      let updates_oc = Unix.out_channel_of_descr updates_w in
      let send_to_parent (message : worker_message) =
        marshal_to_pipe ~file_lock updates_oc message
      in
      let send_final (final_message : 'a final_worker_message) =
        marshal_to_pipe ~file_lock updates_oc final_message
      in
      (* Function to send updates up the pipe to the parent instead of directly to the task
         bar. This is because only the parent knows about all the children, hence it's in charge of
         actually updating the task bar. *)
      let update_status t status =
        let status =
          (* Truncate status if too big: it's pointless to spam the status bar with long status, and
             also difficult to achieve technically over pipes (it's easier if all the messages fit
             into a buffer of reasonable size). *)
          if String.length status > 100 then String.subo ~len:100 status ^ "..." else status
        in
        send_to_parent (UpdateStatus (slot, t, status))
      in
      ProcessPoolState.update_status := update_status ;
      let orders_ic = Unix.in_channel_of_descr to_child_r in
      let receive_from_parent () =
        PerfEvent.log (fun logger ->
            PerfEvent.log_begin_event logger ~categories:["sys"] ~name:"receive from pipe" () ) ;
        let x = Marshal.from_channel orders_ic in
        PerfEvent.(log (fun logger -> log_end_event logger ())) ;
        x
      in
      child_loop ~slot send_to_parent send_final receive_from_parent ~f ~epilogue ;
      Out_channel.close updates_oc ;
      In_channel.close orders_ic ;
      Epilogues.run () ;
      Pervasives.exit 0
  | `In_the_parent pid ->
      let[@warning "-26"] to_child_r = Unix.close to_child_r in
      {pid; down_pipe= Unix.out_channel_of_descr to_child_w}


let create :
       jobs:int
    -> child_prelude:(unit -> unit)
    -> f:('work -> unit)
    -> child_epilogue:(unit -> 'final)
    -> tasks:'work TaskGenerator.t
    -> ('work, 'final) t =
 fun ~jobs ~child_prelude ~f ~child_epilogue ~tasks ->
  let file_lock = Utils.create_file_lock () in
  let task_bar = TaskBar.create ~jobs in
  (* Pipe to communicate from children to parent. Only one pipe is needed: the messages sent by
     children include the identifier of the child sending the message (its [slot]). This way there
     is only one pipe to wait on for updates. *)
  let ((pipe_child_r, pipe_child_w) as status_pipe) = Unix.pipe () in
  let slots =
    Array.init jobs ~f:(fun slot ->
        fork_child ~file_lock ~child_prelude ~slot status_pipe ~f ~epilogue:child_epilogue )
  in
  (* we have forked the child processes and are now in the parent *)
  let[@warning "-26"] pipe_child_w = Unix.close pipe_child_w in
  let children_updates = pipe_child_r in
  let children_states = Array.create ~len:jobs Initializing in
  {slots; children_updates; jobs; task_bar; tasks; children_states; file_lock}


let run pool =
  Utils.with_file_lock ~file_lock:pool.file_lock ~f:(fun () ->
      let total_tasks = pool.tasks.remaining_tasks () in
      TaskBar.set_tasks_total pool.task_bar total_tasks ;
      TaskBar.tasks_done_reset pool.task_bar ;
      (* allocate a buffer for reading children updates once for the whole run *)
      let buffer = Bytes.create buffer_size in
      (* wait for all children to run out of tasks *)
      while not (pool.tasks.is_empty () && all_children_idle pool) do
        process_updates pool buffer ; TaskBar.refresh pool.task_bar
      done ;
      let results = wait_all pool in
      TaskBar.finish pool.task_bar ; results )


let run pool =
  PerfEvent.(log (fun logger -> log_instant_event logger ~name:"start process pool" Global)) ;
  let results = run pool in
  PerfEvent.(log (fun logger -> log_instant_event logger ~name:"end process pool" Global)) ;
  results
