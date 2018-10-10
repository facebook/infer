(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
        ; DifferentialFiltersTests.tests
        ; DifferentialTests.tests
        ; FileDiffTests.tests
        ; IListTests.tests
        ; JavaProfilerSamplesTest.tests
        ; LivenessTests.tests
        ; PerfProfilerATDParserTest.tests
        ; ProcCfgTests.tests
        ; SchedulerTests.tests
        ; SeverityTests.tests
        ; StacktraceTests.tests
        ; TaintTests.tests
        ; TraceTests.tests
        ; WeakTopologicalOrderTests.tests ]
      @ ClangTests.tests )
  in
  let test_suite = "all" >::: tests in
  OUnit2.run_test_tt_main test_suite
