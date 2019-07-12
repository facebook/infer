(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

type target = Procname of Typ.Procname.t | File of SourceFile.t

type 'a task_generator = 'a Tasks.task_generator

let chain (gen1 : 'a task_generator) (gen2 : 'a task_generator) : 'a task_generator =
  let remaining_tasks () = gen1.remaining_tasks () + gen2.remaining_tasks () in
  let gen1_returned_empty = ref false in
  let gen1_is_empty () =
    gen1_returned_empty := !gen1_returned_empty || gen1.is_empty () ;
    !gen1_returned_empty
  in
  let is_empty () = gen1_is_empty () && gen2.is_empty () in
  let finished x = if gen1_is_empty () then gen2.finished x else gen1.finished x in
  let next x = if gen1_is_empty () then gen2.next x else gen1.next x in
  {remaining_tasks; is_empty; finished; next}


let count_procedures () =
  let db = ResultsDatabase.get_database () in
  let stmt = Sqlite3.prepare db "SELECT COUNT(rowid) FROM procedures" in
  let count =
    match SqliteUtils.result_single_column_option db ~log:"counting procedures" stmt with
    | Some (Sqlite3.Data.INT i64) ->
        Int64.to_int i64 |> Option.value ~default:Int.max_value
    | _ ->
        L.die InternalError "Got no result trying to count procedures"
  in
  L.debug Analysis Quiet "Found %d procedures in procedures table.@." count ;
  count


(** choose some reasonable minimum capacity that also is a prime number *)
let initial_call_graph_capacity = 1009

let bottom_up sources : target task_generator =
  (* this will potentially grossly overapproximate the tasks *)
  let remaining = ref (count_procedures ()) in
  let remaining_tasks () = !remaining in
  let g = SyntacticCallGraph.create initial_call_graph_capacity in
  let initialized = ref false in
  let pending : SyntacticCallGraph.Node.t list ref = ref [] in
  let scheduled = ref Typ.Procname.Set.empty in
  let is_empty () =
    let empty = !initialized && List.is_empty !pending && Typ.Procname.Set.is_empty !scheduled in
    if empty then (
      remaining := 0 ;
      L.progress "Finished call graph scheduling, %d procs remaining (in cycles).@."
        (SyntacticCallGraph.n_procs g) ;
      if Config.debug_level_analysis > 0 then SyntacticCallGraph.to_dotty g "cycles.dot" ;
      (* save some memory *)
      SyntacticCallGraph.reset g ) ;
    empty
  in
  let rec next_aux () =
    match !pending with
    | [] ->
        pending := SyntacticCallGraph.get_unflagged_leaves g ;
        if List.is_empty !pending then None else next_aux ()
    | n :: ns when n.flag || not (SyntacticCallGraph.mem g n.id) ->
        pending := ns ;
        next_aux ()
    | n :: ns ->
        pending := ns ;
        scheduled := Typ.Procname.Set.add n.pname !scheduled ;
        SyntacticCallGraph.flag_reachable g n.pname ;
        Some (Procname n.pname)
  in
  let finished = function
    | File _ ->
        assert false
    | Procname pname ->
        decr remaining ;
        scheduled := Typ.Procname.Set.remove pname !scheduled ;
        SyntacticCallGraph.remove_reachable g pname
  in
  let next () =
    (* do construction here, to avoid having the call graph into forked workers *)
    if not !initialized then (
      SyntacticCallGraph.build_from_sources g sources ;
      initialized := true ) ;
    next_aux ()
  in
  {remaining_tasks; is_empty; finished; next}


let of_sources sources =
  let gen =
    List.rev_map sources ~f:(fun sf -> File sf)
    |> List.permute ~random_state:(Random.State.make (Array.create ~len:1 0))
    |> Tasks.gen_of_list
  in
  let next x =
    let res = gen.next x in
    match res with None -> None | Some (Procname _) -> assert false | Some (File _) as v -> v
  in
  {gen with next}


let schedule sources =
  if Config.call_graph_schedule then chain (bottom_up sources) (of_sources sources)
  else of_sources sources
