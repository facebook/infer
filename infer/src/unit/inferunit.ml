(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** module for running OCaml unit tests *)

let () =
  let open OUnit2 in
  let tests =
    [ AbstractInterpreterTests.tests
    ; AccessPathTests.tests
    ; AccessTreeTests.tests
    ; AddressTakenTests.tests
    ; BoundedCallTreeTests.tests
    ; CopyPropagationTests.tests
    ; DifferentialTests.tests
    ; DifferentialFiltersTests.tests
    ; ProcCfgTests.tests
    ; LivenessTests.tests
    ; SchedulerTests.tests
    ; StacktraceTests.tests
    ; TaintTests.tests
    ; TraceTests.tests ]
    @ ClangTests.tests
  in
  let test_suite = "all" >::: tests in
  OUnit2.run_test_tt_main test_suite
