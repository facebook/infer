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

(** Test the generic abstract interpreter by using a simple path counting domain. Path counting is
    actually a decent stress test--if you join too much/too little, you'll over/under-count, and
    you'll diverge at loops if you don't widen *)

module PathCountDomain = struct
  type astate = PathCount of int | Top

  let make_path_count c =
    (* guarding against overflow *)
    if c < 0 then Top else PathCount c

  let initial = make_path_count 1

  let ( <= ) ~lhs ~rhs =
    match (lhs, rhs) with
    | PathCount c1, PathCount c2
     -> c1 <= c2
    | _, Top
     -> true
    | Top, PathCount _
     -> false

  let join a1 a2 =
    match (a1, a2) with
    | PathCount c1, PathCount c2
     -> make_path_count (c1 + c2)
    | Top, _ | PathCount _, Top
     -> Top

  let widen ~prev:_ ~next:_ ~num_iters:_ = Top

  let pp fmt = function PathCount c -> F.fprintf fmt "%d" c | Top -> F.fprintf fmt "T"
end

module PathCountTransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = PathCountDomain

  type extras = ProcData.no_extras

  (* just propagate the current path count *)
  let exec_instr astate _ _ _ = astate
end

module NormalTestInterpreter = AnalyzerTester.Make (ProcCfg.Normal) (PathCountTransferFunctions)
module ExceptionalTestInterpreter =
  AnalyzerTester.Make (ProcCfg.Exceptional) (PathCountTransferFunctions)

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  let initial = PathCountDomain.initial in
  let normal_test_list =
    [ ("straightline", [invariant "1"; invariant "1"])
    ; ("if", [invariant "1"; If (unknown_exp, [], []); invariant "2"])
    ; ("if_then", [If (unknown_exp, [invariant "1"], []); invariant "2"])
    ; ("if_else", [If (unknown_exp, [], [invariant "1"]); invariant "2"])
    ; ("if_then_else", [If (unknown_exp, [invariant "1"], [invariant "1"]); invariant "2"])
    ; ( "nested_if_then"
      , [If (unknown_exp, [If (unknown_exp, [], []); invariant "2"], []); invariant "3"] )
    ; ( "nested_if_else"
      , [If (unknown_exp, [], [If (unknown_exp, [], []); invariant "2"]); invariant "3"] )
    ; ( "nested_if_then_else"
      , [ If
            ( unknown_exp
            , [If (unknown_exp, [], []); invariant "2"]
            , [If (unknown_exp, [], []); invariant "2"] )
        ; invariant "4" ] )
    ; ( "if_diamond"
      , [ invariant "1"
        ; If (unknown_exp, [], [])
        ; invariant "2"
        ; If (unknown_exp, [], [])
        ; invariant "4" ] )
    ; ("loop", [invariant "1"; While (unknown_exp, [invariant "T"]); invariant "T"])
    ; ( "if_in_loop"
      , [While (unknown_exp, [If (unknown_exp, [], []); invariant "T"]); invariant "T"] )
    ; ( "nested_loop_visit"
      , [ invariant "1"
        ; While (unknown_exp, [invariant "T"; While (unknown_exp, [invariant "T"]); invariant "T"])
        ; invariant "T" ] )
    ; ( "try"
      , [ Try
            ( (* we expect the try block to be visited *)
              [invariant "1"]
            , (* but not the catch block *)
              [invariant "_|_"]
            , (* we expect the finally block to be visited *)
              [invariant "1"] )
        ; invariant "1" ] ) ]
    |> NormalTestInterpreter.create_tests ProcData.empty_extras ~initial
  in
  let exceptional_test_list =
    [ ( "try1"
      , [ Try
            ( [invariant "1"]
            , (* catch block should be visited *)
              [invariant "1"]
            , (* could come from try or catch block *)
              [invariant "2"] )
        ; invariant "2" ] )
    ; ( "try1"
      , [ Try
            ( (* point 3 *)
              [ (* note: each instruction in try block is treated as potentially-excepting... *)
                (* point 1 *)
              invariant "1"
              ; (* point 2 *)
              invariant "1" ]
            , (* point 4 *)
              [ (* ... so |paths through catch block| shoud be |number of instructions in try block| *)
              invariant "2" ]
            , (* could arrive here via (1, 2, 3), (1, 4), or (2, 4) *)
              [invariant "3"] )
        ; invariant "3" ] ) ]
    |> ExceptionalTestInterpreter.create_tests ProcData.empty_extras ~initial
  in
  "analyzer_tests_suite" >::: normal_test_list @ exceptional_test_list
