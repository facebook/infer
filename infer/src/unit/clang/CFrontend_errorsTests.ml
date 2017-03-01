(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open OUnit2

let test_correct_removing_new_lines =
  let pp_diff_of_desc fmt (expected, actual) =
    Format.fprintf fmt "Expected: [%s] Found: [%s]" expected actual in
  let create_test (desc : string) (expected_desc : string) _ =
    let output = CFrontend_errors.remove_new_lines desc in
    let cmp = fun s1 s2 -> String.equal s1 s2 in
    assert_equal ~pp_diff:pp_diff_of_desc ~cmp expected_desc output in
  [
    (
      "test_correct_removing_new_lines",
      "The selector m is not available in the required iOS SDK version\n8.0",
      "The selector m is not available in the required iOS SDK version 8.0"
    );
  ]
  |> List.map
    ~f:(fun (name, test_input, expected_output) ->
        name >:: create_test test_input expected_output)

let tests = "cfrontend_errors_suite" >::: test_correct_removing_new_lines
