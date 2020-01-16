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
let make_with_procs_from sources =
  let gen =
    List.map sources ~f:SourceFiles.proc_names_of_source
    |> List.concat
    |> List.rev_map ~f:(fun procname -> SchedulerTypes.Procname procname)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> of_list
  in
  let next x =
    let res = gen.next x in
    (* see defn of gen above to see why res should never match Some (File _) *)
    match res with None -> None | Some (File _) -> assert false | Some (Procname _) as v -> v
  in
  {gen with next}


let make sources =
  ProcessPool.TaskGenerator.chain (make_with_procs_from sources) (FileScheduler.make sources)
