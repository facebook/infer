(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type ('a, 'b) doer = 'a -> 'b option

let with_new_db_connection ~f () =
  ResultsDatabase.new_database_connection () ;
  f ()


module Runner = struct
  type ('work, 'final, 'result) t = ('work, 'final, 'result) ProcessPool.t

  let create ~jobs ~child_prologue ~f ~child_epilogue ~tasks =
    PerfEvent.(
      log (fun logger -> log_begin_event logger ~categories:["sys"] ~name:"fork prepare" ())) ;
    ResultsDatabase.db_close () ;
    let pool =
      ProcessPool.create ~jobs ~f ~child_epilogue ~tasks:(with_new_db_connection ~f:tasks)
        ~child_prologue:
          ((* hack: we'll continue executing after the function passed to [protect], despite what he name might suggest *)
           ForkUtils.protect ~f:child_prologue)
    in
    PerfEvent.(log (fun logger -> log_end_event logger ())) ;
    pool


  let run runner =
    (* Flush here all buffers to avoid passing unflushed data to forked processes, leading to duplication *)
    Stdlib.flush_all () ;
    (* Compact heap before forking *)
    Gc.compact () ;
    ProcessPool.run runner
end

let run_sequentially ~(f : ('a, 'b) doer) (tasks : 'a list) : unit =
  let task_generator = ProcessPool.TaskGenerator.of_list tasks in
  let task_bar = TaskBar.create ~jobs:1 in
  (ProcessPoolState.update_status :=
     fun t status ->
       TaskBar.update_status task_bar ~slot:0 t status ;
       TaskBar.refresh task_bar) ;
  TaskBar.set_tasks_total task_bar (task_generator.remaining_tasks ()) ;
  TaskBar.tasks_done_reset task_bar ;
  let rec run_tasks () =
    if not (task_generator.is_empty ()) then (
      Option.iter (task_generator.next ()) ~f:(fun t ->
          let result = f t in
          task_generator.finished ~result t ) ;
      TaskBar.set_remaining_tasks task_bar (task_generator.remaining_tasks ()) ;
      TaskBar.refresh task_bar ;
      run_tasks () )
  in
  run_tasks () ;
  TaskBar.finish task_bar
