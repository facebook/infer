(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
      TestCase (length, ForkUtils.protect ~f)
  | TestList l ->
      TestList (List.map ~f:mk_test_fork_proof l)
  | TestLabel (label, test) ->
      TestLabel (label, mk_test_fork_proof test)


let () =
  let open OUnit2 in
  let tests =
    (* OUnit runs tests in parallel using fork(2) *)
    List.map ~f:mk_test_fork_proof
      ( [ AbstractInterpreterTests.tests
        ; AccessTreeTests.tests
        ; AddressTakenTests.tests
        ; DifferentialFiltersTests.tests
        ; DifferentialTests.tests
        ; FileDiffTests.tests
        ; GradleTests.tests
        ; HilExpTests.tests
        ; IListTests.tests
        ; JavaClassNameTests.tests
        ; LivenessTests.tests
        ; LRUHashtblTests.tests
        ; ProcCfgTests.tests
        ; RestartSchedulerTests.tests
        ; SchedulerTests.tests
        ; SeverityTests.tests
        ; WeakTopologicalOrderTests.tests ]
      @ ClangTests.tests )
  in
  let test_suite = "all" >::: tests in
  run_test_tt_main test_suite ;
  Epilogues.run ()
