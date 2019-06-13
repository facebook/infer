(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let test_correct_ios_version =
  let create_test (version : string) (expected_version : string option) _ =
    let output = CiOSVersionNumbers.version_of version in
    let cmp s1 s2 = Option.equal String.equal s1 s2 in
    assert_equal ~pp_diff:CiOSVersionNumbers.pp_diff_of_version_opt ~cmp expected_version output
  in
  [ ("test_correct_ios_version_some_version", "847.20", Some "7.0")
  ; ("test_correct_ios_version_edge_version", "1348.22", Some "10.2")
  ; ("test_correct_ios_version_ck", "1223.1", Some "9.0")
  ; ("test_correct_ios_version_9", "1240.0999", Some "9.0")
  ; ("test_correct_ios_version_2", "478.230001", Some "2.0")
  ; ("test_correct_ios_version_smaller", "1.49", None) ]
  |> List.map ~f:(fun (name, test_input, expected_output) ->
         name >:: create_test test_input expected_output )


let tests = "cios_version_numbers_suite" >::: test_correct_ios_version
