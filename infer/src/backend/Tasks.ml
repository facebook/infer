(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type 'a doer = 'a -> unit

type 'a task_generator = 'a ProcessPool.task_generator

let fork_protect ~f x =
  (* this is needed whenever a new process is started *)
  BackendStats.reset () ;
  Epilogues.reset () ;
  EventLogger.prepare () ;
  L.reset_formatters () ;
  ResultsDatabase.new_database_connection () ;
  (* get different streams of random numbers in each fork, in particular to lessen contention in
     `Filename.mk_temp` *)
  Random.self_init () ;
  Epilogues.register
    ~f:(fun () -> L.debug Analysis Quiet "%a@." BackendStats.pp (BackendStats.get ()))
    ~description:"dumping summaries stats" ;
  f x


module Runner = struct
  type 'a t = 'a ProcessPool.t

  let create ~jobs ~f ~tasks =
    PerfEvent.(
      log (fun logger -> log_begin_event logger ~categories:["sys"] ~name:"fork prepare" ())) ;
    ResultsDatabase.db_close () ;
    let pool =
      ProcessPool.create ~jobs ~f ~tasks
        ~child_prelude:
          ((* hack: run post-fork bookkeeping stuff by passing a dummy function to [fork_protect] *)
           fork_protect ~f:(fun () -> ()))
    in
    ResultsDatabase.new_database_connection () ;
    PerfEvent.(log (fun logger -> log_end_event logger ())) ;
    pool


  let run runner =
    (* Flush here all buffers to avoid passing unflushed data to forked processes, leading to duplication *)
    Pervasives.flush_all () ;
    (* Compact heap before forking *)
    Gc.compact () ;
    ProcessPool.run runner
end

let gen_of_list (lst : 'a list) : 'a task_generator =
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


let run_sequentially ~(f : 'a doer) (tasks : 'a list) : unit =
  let task_generator = gen_of_list tasks in
  let task_bar = TaskBar.create ~jobs:1 in
  (ProcessPoolState.update_status :=
     fun t status ->
       TaskBar.update_status task_bar ~slot:0 t status ;
       TaskBar.refresh task_bar) ;
  TaskBar.set_tasks_total task_bar (task_generator.remaining_tasks ()) ;
  TaskBar.tasks_done_reset task_bar ;
  let rec run_tasks () =
    if not (task_generator.is_empty ()) then (
      Option.iter (task_generator.next ()) ~f:(fun t -> f t ; task_generator.finished t) ;
      TaskBar.set_remaining_tasks task_bar (task_generator.remaining_tasks ()) ;
      TaskBar.refresh task_bar ;
      run_tasks () )
  in
  run_tasks () ; TaskBar.finish task_bar
