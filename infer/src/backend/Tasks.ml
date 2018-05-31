(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type closure = unit -> unit

type t = {closures: closure list; continuations: closure Queue.t}

let create ?(continuation= None) closures =
  let continuations =
    match continuation with None -> Queue.create () | Some closure -> Queue.singleton closure
  in
  {closures; continuations}


(* Aggregate closures into groups of the given size *)
let aggregate ~size t =
  let group_to_closure group () = List.iter ~f:(fun closure -> closure ()) group in
  let group_size = if size > 0 then size else List.length t.closures / Config.jobs in
  if group_size > 1 then
    let groups = List.groupi ~break:(fun n _ _ -> Int.equal (n mod group_size) 0) t.closures in
    let closures = List.map ~f:group_to_closure groups in
    {t with closures}
  else t


let run t =
  List.iter ~f:(fun f -> f ()) t.closures ;
  Queue.iter ~f:(fun closure -> closure ()) t.continuations


let fork_protect ~f x =
  EventLogger.prepare () ;
  L.reset_formatters () ;
  ResultsDatabase.new_database_connection () ;
  f x


module Runner = struct
  type runner = {pool: ProcessPool.t; all_continuations: closure Queue.t}

  let create ~jobs = {pool= ProcessPool.create ~jobs; all_continuations= Queue.create ()}

  let start runner ~tasks =
    let pool = runner.pool in
    Queue.enqueue_all runner.all_continuations (Queue.to_list tasks.continuations) ;
    (* Flush here all buffers to avoid passing unflushed data to forked processes, leading to duplication *)
    Pervasives.flush_all () ;
    List.iter
      ~f:(fun x -> ProcessPool.start_child ~f:(fun f -> fork_protect ~f ()) ~pool x)
      tasks.closures


  let complete runner =
    ProcessPool.wait_all runner.pool ;
    Queue.iter ~f:(fun f -> f ()) runner.all_continuations
end
