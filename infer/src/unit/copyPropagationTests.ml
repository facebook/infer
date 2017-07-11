(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module TestInterpreter =
  AnalyzerTester.Make (ProcCfg.Exceptional) (CopyPropagation.TransferFunctions)

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let assert_empty = invariant "{  }" in
  let test_list =
    [ ("id_load_id_no_gen", [id_assign_id "b" "a"; (* means b = *a *)
                            assert_empty])
    ; ("id_set_id_no_gen", [id_set_id "b" "a"; (* means *b = a *)
                           assert_empty])
    ; ( "id_set_id_no_kill"
      , [ id_assign_var "b" "a"
        ; invariant "{ b$0 -> &a }"
        ; id_set_id "b" "x"
        ; invariant "{ b$0 -> &a }" ] )
    ; ("id_assign_var_gen", [id_assign_var "b" "a"; invariant "{ b$0 -> &a }"])
    ; ( "var_assign_addrof_var_no_gen"
      , [var_assign_addrof_var "b" "a"; (* means b = &a *)
        assert_empty] )
    ; ("var_assign_id_gen", [var_assign_id "b" "a"; invariant "{ &b -> a$0 }"])
    ; ( "multi_command_gen"
      , [ id_assign_var "b" "a"
        ; var_assign_id "c" "b"
        ; id_assign_var "d" "c"
        ; invariant "{ b$0 -> &a, d$0 -> &c, &c -> b$0 }" ] )
    ; ( "simple_kill"
      , [id_assign_var "b" "a"; invariant "{ b$0 -> &a }"; var_assign_int "a" 1; assert_empty] )
    ; ( "kill_then_gen"
      , [ id_assign_var "b" "a"
        ; invariant "{ b$0 -> &a }"
        ; var_assign_id "a" "c"
        ; invariant "{ &a -> c$0 }" ] )
    ; ( "same_copy"
      , [ var_assign_id "b" "a"
        ; var_assign_id "c" "d"
        ; invariant "{ &b -> a$0, &c -> d$0 }"
        ; var_assign_id "c" "d"
        ; invariant "{ &b -> a$0, &c -> d$0 }" ] )
    ; ( "conservative_if"
      , [ var_assign_id "b" "a"
        ; If
            ( unknown_exp
            , [invariant "{ &b -> a$0 }"; var_assign_id "b" "c"; invariant "{ &b -> c$0 }"]
            , [] )
        ; assert_empty ] )
    ; ( "if1"
      , [ var_assign_id "b" "a"
        ; var_assign_id "c" "d"
        ; If
            ( unknown_exp
            , [ invariant "{ &b -> a$0, &c -> d$0 }"
              ; var_assign_id "c" "e"
              ; invariant "{ &b -> a$0, &c -> e$0 }" ]
            , [invariant "{ &b -> a$0, &c -> d$0 }"] )
        ; invariant "{ &b -> a$0 }" ] )
    ; ( "if2"
      , [ If (unknown_exp, [var_assign_id "a" "b"], [var_assign_id "a" "b"])
        ; invariant "{ &a -> b$0 }" ] )
    ; ("if3", [If (unknown_exp, [var_assign_id "a" "b"], [var_assign_id "a" "c"]); assert_empty])
    ; ( "nested_if"
      , [ var_assign_id "b" "a"
        ; var_assign_id "c" "b"
        ; If
            ( unknown_exp
            , [ If
                  ( var_of_str "unknown2"
                  , [ invariant "{ &b -> a$0, &c -> b$0 }"
                    ; var_assign_id "b" "d"
                    ; invariant "{ &b -> d$0, &c -> b$0 }" ]
                  , [] ) ]
            , [] )
        ; invariant "{ &c -> b$0 }" ] )
    ; ( "loop_as_if"
      , [var_assign_id "b" "a"; While (unknown_exp, [var_assign_id "b" "c"]); assert_empty] )
    ; ( "easy_loop_invariant"
      , [ var_assign_id "b" "a"
        ; While (unknown_exp, [var_assign_id "c" "d"; invariant "{ &b -> a$0, &c -> d$0 }"])
        ; invariant "{ &b -> a$0 }" ] )
    ; ( "empty_loop"
      , [ var_assign_id "b" "a"
        ; While (unknown_exp, [])
        ; var_assign_id "c" "b"
        ; invariant "{ &b -> a$0, &c -> b$0 }" ] ) ]
    |> TestInterpreter.create_tests ProcData.empty_extras ~initial:CopyPropagation.Domain.empty
  in
  "copy_propagation_test_suite" >::: test_list
