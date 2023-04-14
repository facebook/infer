(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module TestInterpreter =
  AnalyzerTester.MakeBackwardExceptional
    (Liveness.PreAnalysisTransferFunctions (ProcCfg.Backward (ProcCfg.Exceptional)))

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "normal:{ }" in
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
    [ ("basic_live", [invariant "normal:{ b }"; id_assign_var "a" "b"])
    ; ( "basic_live_then_dead"
      , [assert_empty; var_assign_int "b" 1; invariant "normal:{ b }"; id_assign_var "a" "b"] )
    ; ( "iterative_live"
      , [ invariant "normal:{ d, b, f }"
        ; id_assign_var "e" "f"
        ; invariant "normal:{ d, b }"
        ; id_assign_var "c" "d"
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ( "live_kill_live"
      , [ invariant "normal:{ b }"
        ; id_assign_var "c" "b"
        ; assert_empty
        ; var_assign_int "b" 1
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ("basic_live_load", [invariant "normal:{ y$0 }"; id_assign_id "x" "y"])
    ; ( "basic_live_then_kill_load"
      , [ invariant "normal:{ z$0 }"
        ; id_assign_id "y" "z"
        ; invariant "normal:{ y$0 }"
        ; id_assign_id "x" "y" ] )
    ; ( "set_id"
      , (* this is *x = y, which is a read of both x and y *)
        [invariant "normal:{ x$0, y$0 }"; id_set_id "x" "y"] )
    ; ( "if_exp_live"
      , [assert_empty; var_assign_int "x" 1; invariant "normal:{ x }"; If (var_of_str "x", [], [])]
      )
    ; ( "while_exp_live"
      , [assert_empty; var_assign_int "x" 1; invariant "normal:{ x }"; While (var_of_str "x", [])]
      )
    ; ("call_params_live", [invariant "normal:{ c, b, a }"; call_unknown ["a"; "b"; "c"]])
    ; ( "dead_after_call_with_retval"
      , [ assert_empty
        ; call_unknown ~return:("y", Typ.mk (Tint IInt)) []
        ; invariant "normal:{ y$0 }"
        ; id_assign_id "x" "y" ] )
    ; ( "closure_captured_live"
      , [ invariant "normal:{ b$0, c$0 }"
        ; var_assign_exp ~rhs_typ:fun_ptr_typ "a" (closure_exp ["b"; "c"]) ] )
    ; ( "if_conservative_live1"
      , [invariant "normal:{ b }"; If (unknown_cond, [id_assign_var "a" "b"], [])] )
    ; ( "if_conservative_live2"
      , [ invariant "normal:{ d, b }"
        ; If (unknown_cond, [id_assign_var "a" "b"], [id_assign_var "c" "d"]) ] )
    ; ( "if_conservative_kill"
      , [ invariant "normal:{ b }"
        ; If (unknown_cond, [var_assign_int "b" 1], [])
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ( "if_conservative_kill_live"
      , [ invariant "normal:{ d, b }"
        ; If (unknown_cond, [var_assign_int "b" 1], [id_assign_var "c" "d"])
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ( "if_precise1"
      , [ assert_empty
        ; If
            ( unknown_cond
            , [var_assign_int "b" 1; invariant "normal:{ b }"; id_assign_var "a" "b"]
            , [var_assign_int "d" 1; invariant "normal:{ d }"; id_assign_var "c" "d"] ) ] )
    ; ( "if_precise2"
      , [ assert_empty
        ; If (unknown_cond, [var_assign_int "b" 2], [var_assign_int "b" 1])
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ("loop_as_if1", [invariant "normal:{ b }"; While (unknown_cond, [id_assign_var "a" "b"])])
    ; ( "loop_as_if2"
      , [ invariant "normal:{ b }"
        ; While (unknown_cond, [var_assign_int "b" 1])
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ( "loop_before_after"
      , [ invariant "normal:{ d, b }"
        ; While (unknown_cond, [id_assign_var "b" "d"])
        ; invariant "normal:{ b }"
        ; id_assign_var "a" "b" ] )
    ; ( "java_exceptions"
      , [ invariant "normal:{ c, b, a }"
        ; Try
            ( Java
            , [ id_assign_var "x" "c"
              ; invariant "normal:{ b, a }"
              ; id_assign_var "a" "b"
              ; invariant "normal:{ }" ]
            , [invariant "normal:{ a }"; id_assign_var "x" "a"]
            , [] ) ] )
    ; ( "java_exceptions_empty_try"
      , [ invariant "normal:{ a }"
        ; Try
            (Java, [], [invariant "normal:{ b, a }"; id_assign_var "x" "b"], [id_assign_var "x" "a"])
        ] )
    ; ( "c_exceptions"
      , [ invariant "normal:{ c, b }"
        ; Try
            ( Cpp {try_id= 0}
            , [ id_assign_var "x" "c"
                (* a should be live here but the C++ exception system is not in synch yet with the
                   new abstract interpreter framework for exceptional edges *)
              ; invariant "normal:{ b }"
              ; id_assign_var "a" "b"
              ; invariant "normal:{ }" ]
            , [invariant "normal:{ a }"; id_assign_var "x" "a"]
            , [] ) ] )
    ; ( "c_exceptions_empty_try"
      , [ invariant "normal:{ a }"
        ; Try
            ( Cpp {try_id= 0}
            , []
            , [invariant "normal:{ b, a }"; id_assign_var "x" "b"]
            , [id_assign_var "x" "a"] ) ] ) ]
    |> TestInterpreter.create_tests
         (fun {proc_name} -> Procdesc.load_exn proc_name)
         ~initial:Liveness.ExtendedDomain.bottom
  in
  "liveness_test_suite" >::: test_list
