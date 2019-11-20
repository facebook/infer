(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let pp_diff_of_hashed_value fmt (expected, actual) =
  Format.fprintf fmt "Expected: [%s] Found: [%s]" expected actual


let test_fnv64_hash_function =
  let create_test (input : string) (hashed_input : string) _ =
    let actual_hashed_value = Fnv64Hash.fnv64_hash input in
    let cmp s1 s2 = String.equal s1 s2 in
    assert_equal ~pp_diff:pp_diff_of_hashed_value ~cmp hashed_input actual_hashed_value
  in
  [("test_correct_hash", "_Z4testv", "18241244337164948030")]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let tests = "fnv64_hash_function_suite" >::: test_fnv64_hash_function
