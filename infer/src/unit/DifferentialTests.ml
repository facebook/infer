(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2
open DifferentialTestsUtils

let current_report =
  [ create_fake_jsonbug ~hash:"3" ()
  ; create_fake_jsonbug ~hash:"1" ()
  ; create_fake_jsonbug ~hash:"2" ()
  ; create_fake_jsonbug ~hash:"2" ()
  ; create_fake_jsonbug ~hash:"2" () ]


let previous_report =
  [ create_fake_jsonbug ~hash:"1" ()
  ; create_fake_jsonbug ~hash:"4" ()
  ; create_fake_jsonbug ~hash:"1" () ]


let current_costs = []

let previous_costs = []

let diff = Differential.of_reports ~current_report ~previous_report ~current_costs ~previous_costs

(* Sets operations should keep duplicated issues with identical hashes *)
let test_diff_keeps_duplicated_hashes =
  let hashes_expected = 3 in
  let hashes_found =
    List.fold ~init:0
      ~f:(fun acc (i : Jsonbug_t.jsonbug) ->
        if String.equal i.Jsonbug_t.hash "2" then acc + 1 else acc )
      diff.introduced
  in
  let pp_diff fmt (expected, actual) =
    Format.fprintf fmt "Expected %d issues with hash=2 among the introduced, but got %d instead"
      expected actual
  in
  let do_assert _ = assert_equal ~pp_diff hashes_expected hashes_found in
  "test_diff_keeps_duplicated_hashes" >:: do_assert


(* Sets operations to compute introduced, fixed and preexisting issues are correct *)
let test_set_operations =
  let do_assert _ =
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of introduced")
      ["2"; "2"; "2"; "3"]
      (sorted_hashes_of_issues diff.introduced) ;
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of fixed")
      ["4"]
      (sorted_hashes_of_issues diff.fixed) ;
    assert_equal
      ~pp_diff:(pp_diff_of_string_list "Hashes of preexisting")
      ["1"]
      (sorted_hashes_of_issues diff.preexisting)
  in
  "test_set_operations" >:: do_assert


let tests = "differential_suite" >::: [test_diff_keeps_duplicated_hashes; test_set_operations]
