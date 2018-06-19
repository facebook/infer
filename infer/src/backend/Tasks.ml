(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type task = unit -> unit

type t = task list

(** Aggregate closures into groups of the given size *)
let aggregate ~size t =
  let group_to_closure group () = List.iter ~f:(fun closure -> closure ()) group in
  let group_size = if size > 0 then size else List.length t / Config.jobs in
  if group_size > 1 then List.chunks_of t ~length:group_size |> List.map ~f:group_to_closure else t


let run t = List.iter ~f:(fun f -> f ()) t

let fork_protect ~f x =
  EventLogger.prepare () ;
  L.reset_formatters () ;
  ResultsDatabase.new_database_connection () ;
  f x


module Runner = struct
  type tasks = t

  type t = {pool: ProcessPool.t}

  let create ~jobs = {pool= ProcessPool.create ~jobs}

  let start runner ~tasks =
    let pool = runner.pool in
    (* Flush here all buffers to avoid passing unflushed data to forked processes, leading to duplication *)
    Pervasives.flush_all () ;
    List.iter ~f:(fun x -> ProcessPool.start_child ~f:(fun f -> fork_protect ~f ()) ~pool x) tasks


  let complete runner = ProcessPool.wait_all runner.pool
end
