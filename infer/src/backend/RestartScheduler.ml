(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging
open TaskSchedulerTypes
module Node = SpecializedProcname
module NodeSet = SpecializedProcname.Set

let read_procs_to_analyze () =
  Option.value_map ~default:NodeSet.empty Config.procs_to_analyze_index ~f:(fun index ->
      In_channel.read_all index |> Parsexp.Single.parse_string_exn |> NodeSet.t_of_sexp )


type target_with_dependency = {target: TaskSchedulerTypes.target; dependency_filenames: string list}

module FinalizerMap = Stdlib.Hashtbl.Make (struct
  type t = TaskSchedulerTypes.target [@@deriving equal, hash]
end)

let of_queue ready :
    ( TaskSchedulerTypes.target
    , TaskSchedulerTypes.analysis_result
    , WorkerPoolState.worker_id )
    TaskGenerator.t =
  let remaining = ref (Queue.length ready) in
  let remaining_tasks () = !remaining in
  let is_empty () = Int.equal !remaining 0 in
  (* jobs that had to restart get placed in the [blocked] queue while waiting to be retried *)
  let blocked = Queue.create () in
  (* a ref to avoid checking if the same first job in the [blocked] queue is blocked multiple times
     for different idle workers in the same process pool update cycle *)
  let waiting_for_blocked_target = ref false in
  let finalizers = FinalizerMap.create 1 in
  let restart_count = ref 0 in
  let finished ~result target =
    FinalizerMap.find_opt finalizers target |> Option.iter ~f:ProcLocker.unlock_all ;
    FinalizerMap.remove finalizers target ;
    match result with
    | None | Some Ok ->
        decr remaining ;
        if is_empty () then (
          StatsLogging.log_count ~label:"analysis_restarts" ~value:!restart_count ;
          L.debug Analysis Quiet "restart count: %d@\n" !restart_count )
    | Some (RaceOn {dependency_filenames}) ->
        incr restart_count ;
        Queue.enqueue blocked {target; dependency_filenames}
  in
  let dequeue_from_blocked worker_id =
    match Queue.peek blocked with
    | Some w when not !waiting_for_blocked_target -> (
      (* see if we can acquire the locks needed by this job *)
      match ProcLocker.lock_all worker_id w.dependency_filenames with
      | `LocksAcquired locks ->
          (* success! remove the job from [blocked] since we only [peek]ed before *)
          Queue.dequeue_exn blocked |> ignore ;
          (* the scheduler will need to unlock the locks we acquired on behalf of the child once it
             is done with this work packet *)
          FinalizerMap.add finalizers w.target locks ;
          Some w.target
      | `FailedToLockAll ->
          (* failure; leave the job at the head of the queue and set the flag to avoid checking
             again for a while. This is better (perf wise) than trying to run jobs further down the
             queue, possibly because if we are here there is already too much contention so
             decreasing contention by not scheduling any more tasks for this round is beneficial. *)
          waiting_for_blocked_target := true ;
          None )
    | _ ->
        None
  in
  let next {TaskGenerator.child_id; is_first_update} =
    if is_first_update then
      (* new update cycle, worth checking if the first job in the queue is still blocked again *)
      waiting_for_blocked_target := false ;
    match dequeue_from_blocked child_id with
    | Some _ as some_result ->
        some_result
    | None ->
        (* if there are no blocked jobs available to be run, continue with the original queue or
           wait for the next update if it's empty *)
        Queue.dequeue ready
  in
  {remaining_tasks; is_empty; finished; next}


let make sources =
  let target_count = ref 0 in
  let cons_procname_work acc ~specialization proc_name =
    incr target_count ;
    Procname {proc_name; specialization} :: acc
  in
  let procs_to_analyze_targets =
    NodeSet.fold
      (fun {Node.proc_name; specialization} acc -> cons_procname_work acc ~specialization proc_name)
      (read_procs_to_analyze ()) []
  in
  let pname_targets =
    List.fold sources ~init:procs_to_analyze_targets ~f:(fun init source ->
        SourceFiles.proc_names_of_source source
        |> List.fold ~init ~f:(cons_procname_work ~specialization:None) )
  in
  let make_file_work file =
    incr target_count ;
    TaskSchedulerTypes.File file
  in
  let file_targets = List.rev_map sources ~f:make_file_work in
  let queue = Queue.create ~capacity:!target_count () in
  let permute_and_enqueue targets =
    List.permute targets ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> List.iter ~f:(fun target -> Queue.enqueue queue target)
  in
  permute_and_enqueue pname_targets ;
  permute_and_enqueue file_targets ;
  of_queue queue


let setup () = match Config.scheduler with Restart -> ProcLocker.setup () | _ -> ()

type locked_proc = {start: ExecutionDuration.counter; mutable callees_useful: ExecutionDuration.t}

let locked_procs = DLS.new_key Stack.create

let unlock ~after_exn pname =
  match Stack.pop @@ DLS.get locked_procs with
  | None ->
      L.internal_error "Trying to unlock %a but it does not appear to be locked.@\n" Procname.pp
        pname
  | Some {start; callees_useful} ->
      ( match Stack.top @@ DLS.get locked_procs with
      | Some caller ->
          caller.callees_useful <-
            ( if after_exn then ExecutionDuration.add caller.callees_useful callees_useful
              else ExecutionDuration.add_duration_since caller.callees_useful start )
      | None ->
          Stats.add_to_restart_scheduler_useful_time
            (if after_exn then callees_useful else ExecutionDuration.since start) ;
          Stats.add_to_restart_scheduler_total_time (ExecutionDuration.since start) ) ;
      ProcLocker.unlock pname


let with_lock ~get_actives ~f pname =
  match Config.scheduler with
  | Restart -> (
    match ProcLocker.try_lock pname with
    | `AlreadyLockedByUs ->
        f ()
    | `LockAcquired ->
        Stack.push (DLS.get locked_procs)
          {start= ExecutionDuration.counter (); callees_useful= ExecutionDuration.zero} ;
        let res =
          try f () with exn -> IExn.reraise_after ~f:(fun () -> unlock ~after_exn:true pname) exn
        in
        unlock ~after_exn:false pname ;
        res
    | `LockedByAnotherProcess ->
        let dependency_filenames =
          Procname.to_filename pname
          :: ( get_actives ()
             |> List.map ~f:(fun {SpecializedProcname.proc_name} -> Procname.to_filename proc_name)
             )
        in
        raise (RestartSchedulerException.ProcnameAlreadyLocked {dependency_filenames}) )
  | _ ->
      f ()


let finish result task = match result with None | Some Ok -> None | Some (RaceOn _) -> Some task
