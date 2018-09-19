(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

type child_info = {pid: Pid.t; down_pipe: Out_channel.t}

(** the state of the pool *)
type 'a t =
  { jobs: int
        (** number of jobs running in parallel, i.e. number of children we are responsible for *)
  ; slots: child_info Array.t
        (** array of child processes with their pids and channels we can use to send work down to
            each child *)
  ; children_updates: Unix.File_descr.t
        (** all the children send updates up the same pipe to the pool *)
  ; task_bar: TaskBar.t
  ; mutable tasks: 'a list  (** work remaining to be done *)
  ; mutable idle_children: int
        (** number of children currently ready for more work, but there are no tasks to send to
            them *)
  }

(** {2 Constants} *)

(** refresh rate of the task bar (worst case: it also refreshes on children updates) *)
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
  | Ready of int  (** finished the given task, ready to receive messages *)
  | Crash of int  (** there was an error and the child is no longer receiving messages *)

(** messages from the parent process down to worker processes *)
type 'a boss_message =
  | Do of 'a  (** a task to do *)
  | GoHome  (** all tasks done, prepare for teardown *)

(** convenience function to send data down pipes without forgetting to flush *)
let marshal_to_pipe fd x =
  PerfEvent.log (fun logger ->
      PerfEvent.log_begin_event logger ~categories:["sys"] ~name:"send to pipe" () ) ;
  Marshal.to_channel fd x [] ;
  Out_channel.flush fd ;
  PerfEvent.(log (fun logger -> log_end_event logger ()))


(** like [Unix.read] but reads until [len] bytes have been read *)
let rec really_read ?(pos = 0) ~len fd ~buf =
  if len <= 0 then ()
  else
    let read = Unix.read ~pos ~len fd ~buf in
    if Int.equal read 0 then raise End_of_file ;
    really_read ~pos:(pos + read) ~len:(len - read) fd ~buf


(** return [true] if the [file_descr] is ready for reading after at most [timeout] has
     elapsed *)
let wait_for_updates pool buffer =
  let file_descr = pool.children_updates in
  let timeout = if TaskBar.is_interactive pool.task_bar then refresh_timeout else `Never in
  (* Use select(2) so that we can both wait on the pipe of children updates and wait for a
     timeout. The timeout is for giving a chance to the taskbar of refreshing from time to time. *)
  let {Unix.Select_fds.read= read_fds} =
    Unix.select ~read:[file_descr] ~write:[] ~except:[] ~timeout ()
  in
  match read_fds with
  | _ :: _ :: _ ->
      assert false
  | [] ->
      (* not ready *) None
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
      Some (Marshal.from_bytes buffer 0)


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
      try Unix.wait (`Pid pid) |> ignore with Unix.Unix_error (ECHILD, _, _) ->
        (* some children may have died already, it's fine *) () ) ;
  L.die InternalError "Subprocess %d: %s" slot status


let has_dead_child pool =
  Unix.wait_nohang `Any
  |> Option.map ~f:(fun (dead_pid, status) ->
         ( Array.find_mapi_exn pool.slots ~f:(fun slot {pid} ->
               if Pid.equal pid dead_pid then Some slot else None )
         , status ) )


(** main dispatch function that responds to messages from worker processes and updates the taskbar
   periodically *)
let process_updates pool buffer =
  (* abort everything if some child has died unexpectedly *)
  has_dead_child pool
  |> Option.iter ~f:(fun (slot, status) ->
         killall pool ~slot (Unix.Exit_or_signal.to_string_hum status) ) ;
  match wait_for_updates pool buffer with
  | Some (UpdateStatus (slot, t, status)) ->
      TaskBar.update_status pool.task_bar ~slot t status
  | Some (Crash slot) ->
      let {pid} = pool.slots.(slot) in
      (* clean crash, give the child process a chance to cleanup *)
      Unix.wait (`Pid pid) |> ignore ;
      killall pool ~slot "see backtrace above"
  | Some (Ready slot) -> (
      TaskBar.tasks_done_add pool.task_bar 1 ;
      match pool.tasks with
      | [] ->
          TaskBar.update_status pool.task_bar ~slot (Mtime_clock.now ()) "idle" ;
          pool.idle_children <- pool.idle_children + 1
      | x :: tasks ->
          pool.tasks <- tasks ;
          let {down_pipe} = pool.slots.(slot) in
          marshal_to_pipe down_pipe (Do x) )
  | None ->
      ()


(** terminate all worker processes *)
let wait_all pool =
  (* tell each alive worker to go home and wait(2) them, one by one; the order doesn't matter since
     we want to wait for all of them eventually anyway. *)
  let errors =
    Array.foldi ~init:[] pool.slots ~f:(fun slot errors {pid; down_pipe} ->
        marshal_to_pipe down_pipe GoHome ;
        Out_channel.close down_pipe ;
        match Unix.wait (`Pid pid) with
        | _pid, Ok () ->
            errors
        | _pid, (Error _ as status) ->
            (* Collect all children errors and die only at the end to avoid creating zombies. *)
            (slot, status) :: errors )
  in
  if not (List.is_empty errors) then
    let log_or_die = if Config.keep_going then L.internal_error else L.die InternalError in
    let pp_error f (slot, status) =
      F.fprintf f "Error in infer subprocess %d: %s@." slot
        (Unix.Exit_or_signal.to_string_hum status)
    in
    log_or_die "@[<v>%a@]%!" (Pp.seq ~print_env:Pp.text_break ~sep:"" pp_error) errors


(** worker loop: wait for tasks and run [f] on them until we are told to go home *)
let rec child_loop ~slot send_to_parent receive_from_parent ~f =
  send_to_parent (Ready slot) ;
  match receive_from_parent () with
  | GoHome ->
      ()
  | Do stuff ->
      ( try f stuff with e ->
          IExn.reraise_if e ~f:(fun () ->
              if Config.keep_going then (
                L.internal_error "Error in subprocess %d: %a@." slot Exn.pp e ;
                (* do not raise and continue accepting jobs *)
                false )
              else (
                (* crash hard, but first let the master know that we have crashed *)
                send_to_parent (Crash slot) ;
                true ) ) ) ;
      child_loop ~slot send_to_parent receive_from_parent ~f


(** Fork a new child and start it so that it is ready for work.

    The child inherits [updates_w] to send updates up to the parent, and a new pipe is set up for
    the parent to send instructions down to the child. *)
let fork_child ~child_prelude ~slot (updates_r, updates_w) ~f =
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
      let send_to_parent (message : worker_message) = marshal_to_pipe updates_oc message in
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
      child_loop ~slot send_to_parent receive_from_parent ~f ;
      Out_channel.close updates_oc ;
      In_channel.close orders_ic ;
      Epilogues.run () ;
      Pervasives.exit 0
  | `In_the_parent pid ->
      let[@warning "-26"] to_child_r = Unix.close to_child_r in
      {pid; down_pipe= Unix.out_channel_of_descr to_child_w}


let create : jobs:int -> child_prelude:(unit -> unit) -> f:('a -> unit) -> 'a t =
 fun ~jobs ~child_prelude ~f ->
  let task_bar = TaskBar.create ~jobs in
  (* Pipe to communicate from children to parent. Only one pipe is needed: the messages sent by
      children include the identifier of the child sending the message (its [slot]). This way there
      is only one pipe to wait on for updates. *)
  let ((pipe_child_r, pipe_child_w) as status_pipe) = Unix.pipe () in
  let slots = Array.init jobs ~f:(fun slot -> fork_child ~child_prelude ~slot status_pipe ~f) in
  (* we have forked the child processes and are now in the parent *)
  let[@warning "-26"] pipe_child_w = Unix.close pipe_child_w in
  let children_updates = pipe_child_r in
  {slots; children_updates; jobs; task_bar; tasks= []; idle_children= 0}


let run pool tasks =
  pool.tasks <- tasks ;
  TaskBar.set_tasks_total pool.task_bar (List.length tasks) ;
  TaskBar.tasks_done_reset pool.task_bar ;
  (* Start with a negative number of completed tasks to account for the initial [Ready]
     messages. All the children start by sending [Ready], which is interpreted by the parent process
     as "one task has been completed". Starting with a negative number is a simple if hacky way to
     account for these spurious "done" tasks.  *)
  TaskBar.tasks_done_add pool.task_bar (-pool.jobs) ;
  (* allocate a buffer for reading children updates once for the whole run *)
  let buffer = Bytes.create buffer_size in
  (* wait for all children to run out of tasks *)
  while not (List.is_empty pool.tasks && pool.idle_children >= pool.jobs) do
    process_updates pool buffer ; TaskBar.refresh pool.task_bar
  done ;
  wait_all pool ;
  TaskBar.finish pool.task_bar


let run pool tasks =
  PerfEvent.(log (fun logger -> log_instant_event logger ~name:"start process pool" Global)) ;
  run pool tasks ;
  PerfEvent.(log (fun logger -> log_instant_event logger ~name:"end process pool" Global))
