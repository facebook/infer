(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module TestInterpreter =
  AnalyzerTester.Make (Liveness.PreAnalysisTransferFunctions (ProcCfg.Backward (ProcCfg.Normal)))

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "normal:{ }, exn:{ }" in
  let fun_ptr_typ = Typ.mk (Tptr (Typ.mk Tfun, Pk_pointer)) in
  let closure_exp captured_pvars =
    let mk_captured_var str =
      (Exp.Var (ident_of_str str), pvar_of_str str, dummy_typ, CapturedVar.ByReference)
    in
    let captured_vars = List.map ~f:mk_captured_var captured_pvars in
    let closure = {Exp.name= dummy_procname; captured_vars} in
    Exp.Closure closure
  in
  let unknown_cond =
    (* don't want to use AnalyzerTest.unknown_exp because we'll treat it as a live var! *)
    Exp.zero
  in
  let test_list =
    [ ("basic_live", [invariant "normal:{ b }, exn:{ }"; id_assign_var "a" "b"])
    ; ( "basic_live_then_dead"
      , [ assert_empty
        ; var_assign_int "b" 1
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "iterative_live"
      , [ invariant "normal:{ b, f, d }, exn:{ }"
        ; id_assign_var "e" "f"
        ; invariant "normal:{ b, d }, exn:{ }"
        ; id_assign_var "c" "d"
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "live_kill_live"
      , [ invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "c" "b"
        ; assert_empty
        ; var_assign_int "b" 1
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ("basic_live_load", [invariant "normal:{ y$0 }, exn:{ }"; id_assign_id "x" "y"])
    ; ( "basic_live_then_kill_load"
      , [ invariant "normal:{ z$0 }, exn:{ }"
        ; id_assign_id "y" "z"
        ; invariant "normal:{ y$0 }, exn:{ }"
        ; id_assign_id "x" "y" ] )
    ; ( "set_id"
      , (* this is *x = y, which is a read of both x and y *)
        [invariant "normal:{ x$0, y$0 }, exn:{ }"; id_set_id "x" "y"] )
    ; ( "if_exp_live"
      , [ assert_empty
        ; var_assign_int "x" 1
        ; invariant "normal:{ x }, exn:{ }"
        ; If (var_of_str "x", [], []) ] )
    ; ( "while_exp_live"
      , [ assert_empty
        ; var_assign_int "x" 1
        ; invariant "normal:{ x }, exn:{ }"
        ; While (var_of_str "x", []) ] )
    ; ("call_params_live", [invariant "normal:{ b, a, c }, exn:{ }"; call_unknown ["a"; "b"; "c"]])
    ; ( "dead_after_call_with_retval"
      , [ assert_empty
        ; call_unknown ~return:("y", Typ.mk (Tint IInt)) []
        ; invariant "normal:{ y$0 }, exn:{ }"
        ; id_assign_id "x" "y" ] )
    ; ( "closure_captured_live"
      , [ invariant "normal:{ b$0, c$0 }, exn:{ }"
        ; var_assign_exp ~rhs_typ:fun_ptr_typ "a" (closure_exp ["b"; "c"]) ] )
    ; ( "if_conservative_live1"
      , [invariant "normal:{ b }, exn:{ }"; If (unknown_cond, [id_assign_var "a" "b"], [])] )
    ; ( "if_conservative_live2"
      , [ invariant "normal:{ b, d }, exn:{ }"
        ; If (unknown_cond, [id_assign_var "a" "b"], [id_assign_var "c" "d"]) ] )
    ; ( "if_conservative_kill"
      , [ invariant "normal:{ b }, exn:{ }"
        ; If (unknown_cond, [var_assign_int "b" 1], [])
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "if_conservative_kill_live"
      , [ invariant "normal:{ b, d }, exn:{ }"
        ; If (unknown_cond, [var_assign_int "b" 1], [id_assign_var "c" "d"])
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "if_precise1"
      , [ assert_empty
        ; If
            ( unknown_cond
            , [var_assign_int "b" 1; invariant "normal:{ b }, exn:{ }"; id_assign_var "a" "b"]
            , [var_assign_int "d" 1; invariant "normal:{ d }, exn:{ }"; id_assign_var "c" "d"] ) ]
      )
    ; ( "if_precise2"
      , [ assert_empty
        ; If (unknown_cond, [var_assign_int "b" 2], [var_assign_int "b" 1])
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "loop_as_if1"
      , [invariant "normal:{ b }, exn:{ }"; While (unknown_cond, [id_assign_var "a" "b"])] )
    ; ( "loop_as_if2"
      , [ invariant "normal:{ b }, exn:{ }"
        ; While (unknown_cond, [var_assign_int "b" 1])
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] )
    ; ( "loop_before_after"
      , [ invariant "normal:{ b, d }, exn:{ }"
        ; While (unknown_cond, [id_assign_var "b" "d"])
        ; invariant "normal:{ b }, exn:{ }"
        ; id_assign_var "a" "b" ] ) ]
    |> TestInterpreter.create_tests
         (fun summary -> Summary.get_proc_desc summary)
         ~initial:Liveness.Domain.bottom
  in
  "liveness_test_suite" >::: test_list
