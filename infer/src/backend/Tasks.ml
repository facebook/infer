(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type 'a doer = 'a -> unit

let run_sequentially ~(f: 'a doer) (tasks: 'a list) : unit =
  let task_bar =
    if Config.show_progress_bar then TaskBar.create_multiline ~jobs:1 else TaskBar.create_dummy ()
  in
  (ProcessPoolState.update_status :=
     fun t status ->
       TaskBar.update_status task_bar ~slot:0 t status ;
       TaskBar.refresh task_bar) ;
  TaskBar.set_tasks_total task_bar (List.length tasks) ;
  TaskBar.tasks_done_reset task_bar ;
  List.iter
    ~f:(fun task -> f task ; TaskBar.tasks_done_add task_bar 1 ; TaskBar.refresh task_bar)
    tasks ;
  TaskBar.finish task_bar


let fork_protect ~f x =
  EventLogger.prepare () ;
  L.reset_formatters () ;
  ResultsDatabase.new_database_connection () ;
  f x


module Runner = struct
  type 'a t = {pool: 'a ProcessPool.t; task_bar: TaskBar.t}

  let create ~jobs ~f =
    let task_bar =
      if Config.show_progress_bar then TaskBar.create_multiline ~jobs else TaskBar.create_dummy ()
    in
    { pool=
        ProcessPool.create ~jobs
          ~child_prelude:
            ((* hack: run post-fork bookkeeping stuff by passing a dummy function to [fork_protect] *)
             fork_protect ~f:(fun () -> () ))
          task_bar ~f
    ; task_bar }


  let run runner ~tasks =
    (* Flush here all buffers to avoid passing unflushed data to forked processes, leading to duplication *)
    Pervasives.flush_all () ;
    (* Compact heap before forking *)
    Gc.compact () ;
    ProcessPool.run runner.pool tasks ;
    TaskBar.finish runner.task_bar
end
