(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type t = {mutable num_processes: int; jobs: int}

let create ~jobs = {num_processes= 0; jobs}

let incr counter = counter.num_processes <- counter.num_processes + 1

let decr counter = counter.num_processes <- counter.num_processes - 1

let wait counter =
  match Unix.wait `Any with
  | _, Ok _ ->
      decr counter
  | _, (Error _ as status) ->
      let log_or_die = if Config.keep_going then L.internal_error else L.die InternalError in
      log_or_die "Error in infer subprocess: %s@." (Unix.Exit_or_signal.to_string_hum status) ;
      decr counter


let wait_all counter = for _ = 1 to counter.num_processes do wait counter done

let should_wait counter = counter.num_processes >= counter.jobs

let start_child ~f ~pool x =
  match Unix.fork () with
  | `In_the_child ->
      ProcessPoolState.in_child := true ;
      f x ;
      Pervasives.exit 0
  | `In_the_parent _pid ->
      incr pool ;
      if should_wait pool then wait pool
