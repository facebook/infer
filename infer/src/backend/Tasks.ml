(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format

type closure = unit -> unit

type t = {
  closures: closure list;
  continuations: closure Queue.t;
}

type tasks = t

let create ?(continuation = None) closures =
  let continuations = match continuation with
    | None ->
        Queue.create ()
    | Some closure ->
        Queue.singleton closure in
  { closures; continuations }

let empty = { closures = []; continuations = Queue.create () }

(* Aggregate closures into groups of the given size *)
let aggregate ~size t =
  let group_to_closure group =
    fun () -> List.iter ~f:(fun closure -> closure ()) group in
  if size > 1
  then
    let groups = List.groupi ~break:(fun n _ _ -> Int.equal (n mod size) 0) t.closures in
    let closures = List.map ~f:group_to_closure groups in
    { t with closures }
  else
    t

let run t =
  List.iter ~f:(fun f  -> f ()) t.closures;
  Queue.iter ~f:(fun closure -> closure ()) t.continuations

module Runner = struct
  type runner =
    { pool : Utils.ProcessPool.t;
      all_continuations : closure Queue.t }

  let create ~jobs =
    { pool = Utils.ProcessPool.create ~jobs;
      all_continuations = Queue.create () }

  let start runner ~tasks =
    let pool = runner.pool in
    Queue.enqueue_all runner.all_continuations (Queue.to_list tasks.continuations);
    List.iter
      ~f:(fun x -> Utils.ProcessPool.start_child ~f:(fun f -> f ()) ~pool x)
      tasks.closures

  let complete runner =
    Utils.ProcessPool.wait_all runner.pool;
    Queue.iter ~f:(fun f -> f ()) runner.all_continuations
end
