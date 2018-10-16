(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module MockProcCfg = SchedulerTests.MockProcCfg
module WTO = WeakTopologicalOrder.Bourdoncle_SCC (MockProcCfg)

let inputs_from_scheduler_tests =
  SchedulerTests.inputs |> List.map ~f:(fun (name, cfg, _, wto) -> (name, cfg, wto))


let inputs =
  inputs_from_scheduler_tests
  @ [ ( "bourdoncle_fig1"
      , [(1, [2]); (2, [3; 8]); (3, [4]); (4, [5; 7]); (5, [6]); (6, [5; 7]); (7, [3; 8])]
      , "1 2 (3 4 (5 6) 7) 8" )
    ; ( "bourdoncle_fig2left"
      , [(1, [2; 4]); (2, [3]); (3, []); (4, [5; 3]); (5, [4])]
      , "1 2 (4 5) 3" )
    ; ("bourdoncle_fig2right", [(1, [2; 4]); (2, [3]); (3, [1]); (4, [3])], "(1 2 4 3)")
    ; ( "bourdoncle_fig5"
      , [ (1, [4; 2])
        ; (2, [3])
        ; (3, [6])
        ; (4, [10])
        ; (10, [20; 40])
        ; (40, [1])
        ; (20, [30])
        ; (30, [60])
        ; (60, [5])
        ; (50, [60])
        ; (6, [50])
        ; (5, [6]) ]
      , "(1 4 10 40) 20 30 2 3 (6 50 60 5)" )
    ; ( "elder_fig1"
      , [(1, [2]); (2, [3]); (7, [2; 8]); (3, [4]); (6, [7; 3]); (4, [5]); (5, [6; 2])]
      , "1 (2 (3 4 5 6) 7) 8" )
    ; ( "jjb1" (* corresponds to tests/codetoanalyze/c/frontend/gotostmt/jjb1.c *)
      , [ (1, [19])
        ; (19, [6])
        ; (6, [7; 8])
        ; (7, [16])
        ; (8, [9])
        ; (16, [13])
        ; (13, [14; 15])
        ; (5 (* no preds, dead node *), [4])
        ; (14, [4])
        ; (15, [12])
        ; (4, [3])
        ; (12, [11])
        ; (3, [2])
        ; (11, [10])
        ; (10, [9])
        ; (9, [18])
        ; (18, [17])
        ; (17, [16]) ]
      , "1 19 6 7 8 (9 18 17 16 13 15 12 11 10) 14 4 3 2" )
    ; ("self_loop", [(1, [1])], "(1)")
    ; ("smallest_nested", [(1, [2]); (2, [1; 2])], "(1 (2))")
    ; ("smallest_nested_reversed", [(1, [1; 2]); (2, [1])], "(1 2)")
    ; ( "nested_loops_two_entries"
      , [(1, [60; 6]); (60, [5; 50]); (5, [6]); (6, [50]); (50, [60])]
      , "1 (6 (50 60) 5)" )
    ; ( "nested_loops2"
      , [ (1, [2])
        ; (2, [9])
        ; (6, [2; 7])
        ; (7, [8; 5])
        ; (5, [7; 6])
        ; (9, [3; 10])
        ; (3, [4])
        ; (4, [5])
        ; (8, [4; 3]) ]
      , "1 (2 9 (3 (4 (5 6 7) 8))) 10" ) ]


let create_test cfg expected_result _ =
  let result =
    let partition = WTO.make cfg in
    Format.asprintf "%a"
      (WeakTopologicalOrder.Partition.pp ~pp_node:MockProcCfg.Node.pp_id)
      partition
  in
  OUnit2.assert_equal ~printer:Fn.id expected_result result


let tests =
  let open OUnit2 in
  let test_list =
    inputs |> List.map ~f:(fun (name, test, expected) -> name >:: create_test test expected)
  in
  "wto_suite" >::: test_list
