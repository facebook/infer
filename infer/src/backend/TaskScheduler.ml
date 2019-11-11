(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

let of_sources sources =
  let open SchedulerTypes in
  let gen =
    List.rev_map sources ~f:(fun sf -> File sf)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> ProcessPool.TaskGenerator.of_list
  in
  let next x =
    let res = gen.next x in
    match res with None -> None | Some (Procname _) -> assert false | Some (File _) as v -> v
  in
  {gen with next}


let schedule sources =
  if Config.call_graph_schedule then
    ProcessPool.TaskGenerator.chain (SyntacticCallGraph.bottom_up sources) (of_sources sources)
  else of_sources sources
