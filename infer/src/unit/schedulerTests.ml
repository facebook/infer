(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format


(* mock for creating CFG's from adjacency lists *)
module MockProcCfg = struct
  type node = int
  type node_id = int
  type t = (node * node list) list

  let node_id_compare = int_compare

  let node_id n = n

  let succs t n =
    try
      IList.find (fun (node, _) -> node_id_compare node n = 0) t
      |> snd
    with Not_found -> []

  let preds t n =
    try
      IList.filter
        (fun (_, succs) -> IList.exists (fun node -> node_id_compare node n = 0) succs) t
      |> IList.map fst
    with Not_found -> []

  let from_adjacency_list t = t

end

module S = Scheduler.ReversePostorder (MockProcCfg)

let create_test test_graph expected_result _ =
  (* keep popping and scheduling until the queue is empty, record the results *)
  let rec pop_schedule_record q visited_acc =
    match S.pop q with
    | Some (n, _, q') ->
        pop_schedule_record (S.schedule_succs q' n) (n :: visited_acc)
    | None -> IList.rev visited_acc in
  let pp_diff fmt (exp, actual) =
    let pp_sched fmt l =
      F.pp_print_list ~pp_sep:F.pp_print_space (fun fmt i -> F.fprintf fmt "%d" i) fmt l in
    F.fprintf fmt "Expected schedule %a but got schedule %a" pp_sched exp pp_sched actual in
  let cfg = MockProcCfg.from_adjacency_list test_graph in
  let q = S.schedule_succs (S.empty cfg) 1 in
  let result = pop_schedule_record q [1] in
  OUnit2.assert_equal ~pp_diff result expected_result

let tests =
  let open OUnit2 in
  let test_list = [
    ("straightline",
     [(1, [2]);
      (2, [3]);
      (3, [4])],
     [1; 2; 3; 4]);
    ("if_then_else",
     [(1, [2; 3]);
      (2, [4]);
      (3, [4]);
      (4, [5])],
     [1; 2; 3; 4; 5]);
    ("if_then",
     [(1, [2; 4]);
      (2, [3]);
      (3, [4]);
      (4, [5])],
     [1; 2; 3; 4; 5]);
    ("diamond",
     [(1, [2; 3]);
      (2, [4]);
      (3, [4]);
      (4, [5; 6]);
      (5, [7]);
      (6, [7]);
      (7, [8])],
     [1; 2; 3; 4; 5; 6; 7; 8]);
    ("switch",
     [(1, [2; 3; 4; 5;]);
      (2, [6]);
      (3, [6]);
      (4, [6]);
      (5, [6]);
      (6, [7])],
     [1; 2; 3; 4; 5; 6; 7;]);
    ("nums_order_irrelevant",
     [(11, [10];);
      (1, [7; 2]);
      (2, [3; 11]);
      (7, [11]);
      (3, [7]);],
     [1; 2; 3; 7; 11; 10]);
  ]
    |> IList.map
      (fun (name, test, expected) -> name>::create_test test expected) in
  "scheduler_suite">:::test_list
