(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format

module TestInterpreter = AnalyzerTester.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (CopyPropagation.Domain)
    (CopyPropagation.TransferFunctions)

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "{  }" in
  let test_list = [
    "var_tautology",
    [
      var_assign_var "a" "a";
      assert_empty
    ];
    "id_tautology",
    [
      id_assign_id "a" "a";
      assert_empty
    ];
    "id_assign_id_gen",
    [
      id_assign_id "b" "a";
      invariant "{ b$0 -> a$0 }"
    ];
    "id_assign_var_gen",
    [
      id_assign_var "b" "a";
      invariant "{ b$0 -> &a }"
    ];
    "var_assign_var_gen",
    [
      var_assign_var "b" "a";
      invariant "{ &b -> &a }"
    ];
    "var_assign_id_gen",
    [
      var_assign_id "b" "a";
      invariant "{ &b -> a$0 }"
    ];
    "multi_command_gen",
    [
      var_assign_var "b" "a";
      var_assign_var "c" "b";
      var_assign_var "d" "c";
      invariant "{ &b -> &a, &c -> &b, &d -> &c }"
    ];
    "simple_kill",
    [
      var_assign_var "b" "a";
      invariant "{ &b -> &a }";
      var_assign_int "a" 1;
      assert_empty
    ];
    "kill_then_gen",
    [
      var_assign_var "b" "a";
      invariant "{ &b -> &a }";
      var_assign_var "a" "b";
      invariant "{ &a -> &b }"
    ];
    "harder_kill",
    [
      var_assign_var "b" "a";
      var_assign_var "c" "b";
      var_assign_var "d" "c";
      invariant "{ &b -> &a, &c -> &b, &d -> &c }";
      var_assign_int "b" 1;
      invariant "{ &c -> &a, &d -> &c }";
      var_assign_int "c" 1;
      invariant "{ &d -> &a }"
    ];
    "same_copy",
    [
      var_assign_var "b" "a";
      var_assign_var "c" "b";
      invariant "{ &b -> &a, &c -> &b }";
      var_assign_var "c" "b";
      invariant "{ &b -> &a, &c -> &b }"
    ];
    "no_cycles",
    [
      var_assign_var "a" "b";
      invariant "{ &a -> &b }";
      var_assign_var "b" "a";
      invariant "{ &b -> &a }";
      var_assign_var "a" "a";
      invariant "{ &b -> &a }"
    ];
    "conservative_if",
    [
      var_assign_var "b" "a";
      If (unknown_exp,
          [invariant "{ &b -> &a }";
           var_assign_var "a" "b";
           invariant "{ &a -> &b }"],
          []
         );
      assert_empty
    ];
    "if1",
    [
      var_assign_var "b" "a";
      var_assign_var "c" "b";
      If (unknown_exp,
          [invariant "{ &b -> &a, &c -> &b }";
           var_assign_var "c" "d"],
          [invariant "{ &b -> &a, &c -> &b }"]
         );
      invariant "{ &b -> &a }"
    ];
    "if2",
    [
      If (unknown_exp,
          [var_assign_var "a" "b"],
          [var_assign_var "a" "b"]
         );
      invariant "{ &a -> &b }"
    ];
    "if3",
    [
      If (unknown_exp,
          [var_assign_var "a" "b"],
          [var_assign_var "a" "c"]
         );
      assert_empty
    ];
    "nested_if",
    [
      var_assign_var "b" "a";
      var_assign_var "c" "b";
      If (unknown_exp,
          [If (var_of_str "unknown2",
               [ invariant "{ &b -> &a, &c -> &b }";
                 var_assign_var "a" "b";
                 invariant "{ &a -> &b, &c -> &b }"],
               []
              )
          ],
          []
         );
      invariant "{ &c -> &b }"
    ];
    "loop_as_if",
    [
      var_assign_var "b" "a";
      While (unknown_exp,
             [var_assign_var "a" "b"]
            );
      assert_empty
    ];
    "easy_loop_invariant",
    [
      var_assign_var "b" "a";
      While (unknown_exp,
             [var_assign_var "c" "b";
              invariant "{ &b -> &a, &c -> &b }"]
            );
      invariant "{ &b -> &a }"
    ];
    "empty_loop",
    [
      var_assign_var "b" "a";
      While (unknown_exp, []);
      var_assign_var "c" "b";
      invariant "{ &b -> &a, &c -> &b }"
    ];
  ] |> TestInterpreter.create_tests in
  "copy_propagation_test_suite">:::test_list
