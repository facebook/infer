(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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


let inter_tests =
  let inter_test input1 input2 _ =
    let using_list = IList.inter ~cmp:Int.compare input1 input2 in
    let using_set =
      IntSet.inter (IntSet.of_list input1) (IntSet.of_list input2) |> IntSet.elements
    in
    assert_equal using_list using_set
  in
  List.concat_map inputs ~f:(fun (name1, input1) ->
      List.map inputs ~f:(fun (name2, input2) ->
          "inter_" ^ name1 ^ "_with_" ^ name2 >:: inter_test input1 input2 ) )


let traverse_test =
  let test_empty _ = assert_equal (Some []) (IList.traverse_opt [] ~f:(fun _ -> None)) in
  let test_none _ = assert_equal None (IList.traverse_opt [42] ~f:(fun _ -> None)) in
  let test_none_first _ =
    assert_equal None (IList.traverse_opt [42; 43] ~f:(fun n -> Option.some_if (Int.equal n 43) n))
  in
  let test_none_last _ =
    assert_equal None (IList.traverse_opt [42; 43] ~f:(fun n -> Option.some_if (Int.equal n 42) n))
  in
  let test_some _ = assert_equal (Some [42; 43]) (IList.traverse_opt [42; 43] ~f:Option.some) in
  [ "traverse_opt_empty" >:: test_empty
  ; "traverse_opt_none" >:: test_none
  ; "traverse_opt_none_first" >:: test_none_first
  ; "traverse_opt_none_last" >:: test_none_last
  ; "traverse_opt_some" >:: test_some ]


let tests = "IList_tests" >::: inter_tests @ traverse_test
