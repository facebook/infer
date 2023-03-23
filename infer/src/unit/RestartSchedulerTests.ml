(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let a_pname = Procname.from_string_c_fun "a_c_fun_name"

(* Tests are organized like this instead of using one function per test because
   OUnit run tests in parallel and since all tests use the same output directory
   (inter-out-unit) the file locks would collide because they all live in a
   directory called procnames_locks inside the output dir. *)
let tests_wrapper _test_ctxt =
  ProcLocker.(
    setup () ;
    (* When tries to lock a Procname that was already locked it fails *)
    try_lock a_pname |> ignore ;
    assert_bool "Should not be able to lock a Procname that's already locked."
      (not (try_lock a_pname)) ;
    unlock a_pname ;
    (* When successives locks/unlocks are performed in the right order they succeed *)
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    (* When an unlock is performed over a non-locked Procname it fails *)
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    try
      unlock a_pname ;
      assert_failure "Should have raised an exception."
    with Die.InferInternalError _ -> () )


let tests = "restart_scheduler_suite" >:: tests_wrapper
