(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let make sources =
  let open TaskSchedulerTypes in
  let gen =
    List.rev_map sources ~f:(fun sf -> File sf)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> ProcessPool.TaskGenerator.of_list
  in
  let next x =
    gen.next x
    (* see defn of gen above to see why res should never match Some (Procname _) *)
    |> Option.map ~f:(function File _ as v -> v | Procname _ | ProcUID _ -> assert false)
  in
  {gen with next}
