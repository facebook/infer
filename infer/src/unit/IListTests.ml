(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let inputs =
  [ ("empty", [])
  ; ("0", [0])
  ; ("0s", [0; 0; 0])
  ; ("1", [1])
  ; ("1s", [1; 1])
  ; ("1_2", [1; 2])
  ; ("1_2_3", [1; 2; 3])
  ; ("1_3_2s", [1; 3; 2; 2; 2; 2])
  ; ("3_1_2", [3; 1; 2])
  ; ("0_3_2", [0; 3; 2])
  ; ("3s_1s", [3; 1; 3; 1; 3])
  ; ("4", [4]) ]


let tests =
  let inter_test input1 input2 _ =
    let using_list = IList.inter ~cmp:Int.compare input1 input2 in
    let using_set =
      IntSet.inter (IntSet.of_list input1) (IntSet.of_list input2) |> IntSet.elements
    in
    assert_equal using_list using_set
  in
  let tests_ =
    List.concat_map inputs ~f:(fun (name1, input1) ->
        List.map inputs ~f:(fun (name2, input2) ->
            "inter_" ^ name1 ^ "_with_" ^ name2 >:: inter_test input1 input2 ) )
  in
  "IList_tests" >::: tests_
