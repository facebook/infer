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

module TestInterpreter = AnalyzerTester.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (BoundedCallTree.TransferFunctions)

let mock_get_proc_desc _ = None

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let f_proc_name = Procname.from_string_c_fun "f" in
  let g_proc_name = Procname.from_string_c_fun "g" in
  let g_args = [((Exp.Const (Const.Cint (IntLit.one))), (Typ.Tint IInt))] in
  let g_ret_ids = [(ident_of_str "r")] in
  let class_name = "com.example.SomeClass" in
  let file_name = "SomeClass.java" in
  let trace = Stacktrace.make "java.lang.NullPointerException"
      [Stacktrace.make_frame class_name "foo" file_name 16;
       Stacktrace.make_frame class_name "bar" file_name 20] in
  let extras = { BoundedCallTree.get_proc_desc = mock_get_proc_desc;
                 stacktrace = trace; } in
  let caller_foo_name = Procname.from_string_c_fun "foo" in
  let caller_bar_name = Procname.from_string_c_fun "bar" in
  let caller_baz_name = Procname.from_string_c_fun "baz" in
  let test_list_from_foo = [
    "on_call_add_proc_name",
    [
      make_call ~procname:f_proc_name [] []; (* means f() *)
      invariant "{ f }"
    ];
    "on_call_add_proc_name_w_args",
    [
      make_call ~procname:g_proc_name g_ret_ids g_args; (* means r = a.g(1) *)
      invariant "{ g }"
    ];
    "handle_two_proc_calls",
    [
      make_call ~procname:f_proc_name [] [];
      invariant "{ f }";
      make_call ~procname:g_proc_name g_ret_ids g_args;
      invariant "{ f, g }"
    ];
    "dont_record_procs_twice",
    [
      make_call ~procname:f_proc_name [] [];
      invariant "{ f }";
      make_call ~procname:f_proc_name [] [];
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_foo_name extras in
  let test_list_from_bar = [
    "on_call_anywhere_on_stack_add_proc_name",
    [
      make_call ~procname:f_proc_name [] []; (* means f() *)
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_bar_name extras in
  let test_list_from_baz = [
    "ignore_procs_unrelated_to_trace",
    [
      make_call ~procname:f_proc_name [] []; (* means f() *)
      invariant "{  }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_baz_name extras in
  let test_list = test_list_from_foo @
                  test_list_from_bar @
                  test_list_from_baz in
  "bounded_calltree_test_suite">:::test_list
