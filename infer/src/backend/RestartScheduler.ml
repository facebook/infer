(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

(** This behaves exactly the same as the FileScheduler so far. The goal is that it will use a work
    queue and proc locking to avoid repeating work and hopefully get some in process cache hits. *)
let make sources =
  let gen =
    List.rev_map sources ~f:(fun sf -> SchedulerTypes.File sf)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> ProcessPool.TaskGenerator.of_list
  in
  let next x =
    let res = gen.next x in
    (* see defn of gen above to see why res should never match Some (Procname _) *)
    match res with None -> None | Some (Procname _) -> assert false | Some (File _) as v -> v
  in
  {gen with next}
