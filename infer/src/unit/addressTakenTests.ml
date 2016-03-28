(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

module TestInterpreter = AnalyzerTester.Make
    (ProcCfg.Forward)
    (Scheduler.ReversePostorder)
    (AddressTaken.Domain)
    (AddressTaken.TransferFunctions)

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "{  }" in
  let int_typ = Sil.Tint IInt in
  let int_ptr_typ = Sil.Tptr (int_typ, Pk_pointer) in
  let fun_ptr_typ = Sil.Tptr (Tfun false, Pk_pointer) in
  let closure_exp captureds =
    let mk_captured_var str = (Sil.Var (ident_of_str str), pvar_of_str str, int_ptr_typ) in
    let captured_vars = IList.map mk_captured_var captureds in
    let closure = { Sil.name=dummy_procname; captured_vars; } in
    Sil.Const (Cclosure closure) in
  let test_list = [
    "address_taken_set_instr",
    [
      var_assign_var ~rhs_typ:int_ptr_typ "a" "b";
      invariant "{ &b }"
    ];
    "address_not_taken_set_instr",
    [
      var_assign_var ~rhs_typ:int_typ "a" "b";
      assert_empty
    ];
    "address_not_taken_letderef_instr1",
    [
      id_assign_var ~rhs_typ:int_ptr_typ "a" "b";
      assert_empty
    ];
    "address_not_taken_letderef_instr2",
    [
      id_assign_var ~rhs_typ:int_typ "a" "b";
      assert_empty
    ];
    "take_multiple_addresses",
    [
      var_assign_var ~rhs_typ:int_ptr_typ "a" "b";
      invariant "{ &b }";
      var_assign_var ~rhs_typ:int_ptr_typ "c" "d";
      invariant "{ &b, &d }";
      var_assign_var ~rhs_typ:int_typ "e" "f";
      invariant "{ &b, &d }"
    ];
    "address_not_taken_closure",
    [
      var_assign_var ~rhs_typ:int_ptr_typ "a" "b";
      var_assign_exp ~rhs_typ:fun_ptr_typ "c" (closure_exp ["d"; "e"]);
      invariant "{ &b }"
    ];
    "if_conservative1",
    [
      If (unknown_exp,
          [var_assign_var ~rhs_typ:int_ptr_typ "a" "b"],
          []
         );
      invariant "{ &b }"
    ];
    "if_conservative2",
    [
      If (unknown_exp,
          [var_assign_var ~rhs_typ:int_ptr_typ "a" "b"],
          [var_assign_var ~rhs_typ:int_ptr_typ "c" "d"]
         );
      invariant "{ &b, &d }"
    ];
    "loop_as_if",
    [
      While (unknown_exp,
             [var_assign_var ~rhs_typ:int_ptr_typ "a" "b"]
            );
      invariant "{ &b }"
    ];
    "loop_before_after",
    [
      var_assign_var ~rhs_typ:int_ptr_typ "a" "b";
      invariant "{ &b }";
      While (unknown_exp,
             [var_assign_var ~rhs_typ:int_ptr_typ "c" "d"]
            );
      invariant "{ &b, &d }";
      var_assign_var ~rhs_typ:int_ptr_typ "e" "f";
      invariant "{ &b, &d, &f }"
    ];
  ] |> TestInterpreter.create_tests in
  "address_taken_suite">:::test_list
