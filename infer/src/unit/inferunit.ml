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

let rec mk_test_fork_proof test =
  let open OUnitTest in
  match test with
  | TestCase (length, f) ->
      TestCase (length, Tasks.fork_protect ~f)
  | TestList l ->
      TestList (List.map ~f:mk_test_fork_proof l)
  | TestLabel (label, test) ->
      TestLabel (label, mk_test_fork_proof test)


let () =
  ResultsDir.create_results_dir () ;
  let open OUnit2 in
  let tests =
    (* OUnit runs tests in parallel using fork(2) *)
    List.map ~f:mk_test_fork_proof
      ( [ AbstractInterpreterTests.tests
        ; AccessPathTests.tests
        ; AccessTreeTests.tests
        ; AddressTakenTests.tests
        ; BoundedCallTreeTests.tests
        ; DifferentialTests.tests
        ; DifferentialFiltersTests.tests
        ; FileDiffTests.tests
        ; JavaProfilerSamplesTest.tests
        ; ProcCfgTests.tests
        ; LivenessTests.tests
        ; SchedulerTests.tests
        ; StacktraceTests.tests
        ; TaintTests.tests
        ; TraceTests.tests ]
      @ ClangTests.tests )
  in
  let test_suite = "all" >::: tests in
  OUnit2.run_test_tt_main test_suite
