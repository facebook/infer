(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

module TestInterpreter =
  AnalyzerTester.Make
    (ProcCfg.Exceptional)
    (BoundedCallTree.TransferFunctions)

let mock_get_proc_desc _ = None

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let initial = BoundedCallTree.Domain.empty in
  let f_proc_name = Typ.Procname.from_string_c_fun "f" in
  let g_proc_name = Typ.Procname.from_string_c_fun "g" in
  let g_args = [((Exp.Const (Const.Cint (IntLit.one))), (Typ.Tint IInt))] in
  let g_ret_id = Some (ident_of_str "r", Typ.Tint IInt) in
  let class_name = "com.example.SomeClass" in
  let file_name = "SomeClass.java" in
  let trace = Stacktrace.make "java.lang.NullPointerException"
      [Stacktrace.make_frame class_name "foo" file_name (Some 16);
       Stacktrace.make_frame class_name "bar" file_name (Some 20)] in
  let extras = { BoundedCallTree.get_proc_desc = mock_get_proc_desc;
                 stacktraces = [trace]; } in
  let multi_trace_1 = Stacktrace.make "java.lang.NullPointerException"
      [Stacktrace.make_frame class_name "foo" file_name (Some 16)] in
  let multi_trace_2 = Stacktrace.make "java.lang.NullPointerException"
      [Stacktrace.make_frame class_name "bar" file_name (Some 20)] in
  let multi_trace_extras = { BoundedCallTree.get_proc_desc = mock_get_proc_desc;
                             stacktraces = [multi_trace_1; multi_trace_2]; } in
  let caller_foo_name = Typ.Procname.from_string_c_fun "foo" in
  let caller_bar_name = Typ.Procname.from_string_c_fun "bar" in
  let caller_baz_name = Typ.Procname.from_string_c_fun "baz" in
  let test_list_from_foo = [
    "on_call_add_proc_name",
    [
      make_call ~procname:f_proc_name None []; (* means f() *)
      invariant "{ f }"
    ];
    "on_call_add_proc_name_w_args",
    [
      make_call ~procname:g_proc_name g_ret_id g_args; (* means r = a.g(1) *)
      invariant "{ g }"
    ];
    "handle_two_proc_calls",
    [
      make_call ~procname:f_proc_name None [];
      invariant "{ f }";
      make_call ~procname:g_proc_name g_ret_id g_args;
      invariant "{ f, g }"
    ];
    "dont_record_procs_twice",
    [
      make_call ~procname:f_proc_name None [];
      invariant "{ f }";
      make_call ~procname:f_proc_name None [];
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests
      ~test_pname:caller_foo_name
      ~initial:BoundedCallTree.Domain.empty
      extras in
  let test_list_from_bar = [
    "on_call_anywhere_on_stack_add_proc_name",
    [
      make_call ~procname:f_proc_name None []; (* means f() *)
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_bar_name extras ~initial in
  let test_list_from_baz = [
    "ignore_procs_unrelated_to_trace",
    [
      make_call ~procname:f_proc_name None []; (* means f() *)
      invariant "{  }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_baz_name extras ~initial in
  let test_list_multiple_traces_from_foo = [
    "on_call_add_proc_name_in_any_stack_1",
    [
      make_call ~procname:f_proc_name None []; (* means f() *)
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_foo_name multi_trace_extras ~initial in
  let test_list_multiple_traces_from_bar = [
    "on_call_add_proc_name_in_any_stack_2",
    [
      make_call ~procname:f_proc_name None []; (* means f() *)
      invariant "{ f }"
    ];
  ] |> TestInterpreter.create_tests ~test_pname:caller_bar_name multi_trace_extras ~initial in
  let test_list = test_list_from_foo @
                  test_list_from_bar @
                  test_list_from_baz @
                  test_list_multiple_traces_from_foo @
                  test_list_multiple_traces_from_bar in
  "bounded_calltree_test_suite">:::test_list
