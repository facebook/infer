(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
open! IStd

(** Keep track of whether the current execution is in a child process *)
let in_child = ref false

type t = {mutable num_processes: int; jobs: int}

let create ~jobs = {num_processes= 0; jobs}

let incr counter = counter.num_processes <- counter.num_processes + 1

let decr counter = counter.num_processes <- counter.num_processes - 1

let wait counter =
  let _ = Unix.wait `Any in
  decr counter

let wait_all counter = for _ = 1 to counter.num_processes do wait counter done

let should_wait counter = counter.num_processes >= counter.jobs

let start_child ~f ~pool x =
  match Unix.fork () with
  | `In_the_child
   -> in_child := true ;
      f x ;
      exit 0
  | `In_the_parent _pid
   -> incr pool ;
      if should_wait pool then wait pool
