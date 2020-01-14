(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let of_list (lst : 'a list) : 'a ProcessPool.TaskGenerator.t =
  let content = Queue.of_list lst in
  let length = ref (Queue.length content) in
  let remaining_tasks () = !length in
  let is_empty () = Queue.is_empty content in
  let finished _finished_item = decr length in
  let next () = Queue.dequeue content in
  {remaining_tasks; is_empty; finished; next}


(** This behaves exactly the same as the FileScheduler so far. The goal is that it will use a work
    queue and proc locking to avoid repeating work and hopefully get some in process cache hits. *)
let make sources =
  let gen =
    List.rev_map sources ~f:(fun sf -> SchedulerTypes.File sf)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> of_list
  in
  let next x =
    let res = gen.next x in
    (* see defn of gen above to see why res should never match Some (Procname _) *)
    match res with None -> None | Some (Procname _) -> assert false | Some (File _) as v -> v
  in
  {gen with next}
