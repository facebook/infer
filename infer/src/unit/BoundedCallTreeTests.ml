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

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let f_proc_name = Procname.from_string_c_fun "f" in
  let g_proc_name = Procname.from_string_c_fun "g" in
  let g_args = [((Sil.Const (Const.Cint (IntLit.one))), (Typ.Tint IInt))] in
  let g_ret_ids = [(ident_of_str "r")] in
  let test_list = [
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
  ] |> TestInterpreter.create_tests ProcData.empty_extras in
  "bounded_calltree_test_suite">:::test_list
