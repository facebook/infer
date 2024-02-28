(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type work_with_dependency = {work: TaskSchedulerTypes.target; dependency_filename_opt: string option}

let of_queue content : ('a, string) ProcessPool.TaskGenerator.t =
  let remaining = ref (Queue.length content) in
  let remaining_tasks () = !remaining in
  let is_empty () = Int.equal !remaining 0 in
  let finished ~result work =
    match result with
    | None ->
        decr remaining
    | Some _ as dependency_filename_opt ->
        Queue.enqueue content {work; dependency_filename_opt}
  in
  let work_if_dependency_allows w =
    match w.dependency_filename_opt with
    | Some dependency_filename when ProcLocker.is_locked ~proc_filename:dependency_filename ->
        Queue.enqueue content w ;
        None
    | None | Some _ ->
        Some w.work
  in
  let next () = Option.bind (Queue.dequeue content) ~f:(fun w -> work_if_dependency_allows w) in
  {remaining_tasks; is_empty; finished; next}


let get_callees =
  let stmt =
    Database.register_statement CaptureDatabase "SELECT callees FROM procedures WHERE proc_uid = :k"
  in
  fun procname ->
    Database.with_registered_statement stmt ~f:(fun db stmt ->
        Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT (Procname.to_unique_id procname))
        |> SqliteUtils.check_result_code db ~log:"get callees bind" ;
        SqliteUtils.result_single_column_option db ~finalize:false ~log:"get static callees" stmt
        |> Option.value_map ~default:[] ~f:Procname.SQLiteList.deserialize )


let is_defined =
  let stmt =
    Database.register_statement CaptureDatabase
      "SELECT 1 FROM procedures WHERE proc_uid = :k AND cfg is not NULL"
  in
  fun proc_uid ->
    Database.with_registered_statement stmt ~f:(fun db stmt ->
        Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT proc_uid)
        |> SqliteUtils.check_result_code db ~log:"is defined bind" ;
        SqliteUtils.result_single_column_option db ~finalize:false ~log:"get static callees" stmt
        |> Option.is_some )


let make sources =
  let target_count = ref 0 in
  let cons_proc_uid_work acc procname =
    let proc_uid = Procname.to_unique_id procname in
    if is_defined proc_uid then (
      incr target_count ;
      {work= TaskSchedulerTypes.ProcUID proc_uid; dependency_filename_opt= None} :: acc )
    else acc
  in
  let add_static_uid_work init procname =
    get_callees procname |> List.fold ~init ~f:cons_proc_uid_work
  in
  let pname_targets, static_targets =
    List.fold sources ~init:([], []) ~f:(fun init source ->
        SourceFiles.proc_names_of_source source
        |> List.fold ~init ~f:(fun (pnames, statics) pname ->
               let pnames' = cons_proc_uid_work pnames pname in
               let statics' = add_static_uid_work statics pname in
               (pnames', statics') ) )
  in
  let make_file_work file =
    incr target_count ;
    {work= TaskSchedulerTypes.File file; dependency_filename_opt= None}
  in
  let file_targets = List.rev_map sources ~f:make_file_work in
  let queue = Queue.create ~capacity:!target_count () in
  let permute_and_enqueue targets =
    List.permute targets ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> List.iter ~f:(Queue.enqueue queue)
  in
  permute_and_enqueue pname_targets ;
  permute_and_enqueue static_targets ;
  permute_and_enqueue file_targets ;
  of_queue queue


let if_restart_scheduler f =
  match Config.scheduler with File | SyntacticCallGraph -> () | Restart -> f ()


let setup () = if_restart_scheduler ProcLocker.setup

type locked_proc = {start: ExecutionDuration.counter; mutable callees_useful: ExecutionDuration.t}

let locked_procs = Stack.create ()

let lock_exn pname =
  if ProcLocker.try_lock pname then
    Stack.push locked_procs
      {start= ExecutionDuration.counter (); callees_useful= ExecutionDuration.zero}
  else
    raise
      (RestartSchedulerException.ProcnameAlreadyLocked
         {dependency_filename= Procname.to_filename pname} )


let unlock ~after_exn pname =
  match Stack.pop locked_procs with
  | None ->
      L.internal_error "Trying to unlock %a but it does not appear to be locked.@\n" Procname.pp
        pname
  | Some {start; callees_useful} ->
      ( match Stack.top locked_procs with
      | Some caller ->
          caller.callees_useful <-
            ( if after_exn then ExecutionDuration.add caller.callees_useful callees_useful
              else ExecutionDuration.add_duration_since caller.callees_useful start )
      | None ->
          Stats.add_to_restart_scheduler_useful_time
            (if after_exn then callees_useful else ExecutionDuration.since start) ;
          Stats.add_to_restart_scheduler_total_time (ExecutionDuration.since start) ) ;
      ProcLocker.unlock pname


let with_lock ~f pname =
  match Config.scheduler with
  | File | SyntacticCallGraph ->
      f ()
  | Restart ->
      lock_exn pname ;
      let res =
        try f () with exn -> IExn.reraise_after ~f:(fun () -> unlock ~after_exn:true pname) exn
      in
      unlock ~after_exn:false pname ;
      res
