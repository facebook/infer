(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open OUnit2

let a_pname = Procname.from_string_c_fun "a_c_fun_name"

let test_try_lock_already_locked _test_ctxt =
  ProcLocker.(
    setup () ;
    try_lock a_pname |> ignore ;
    assert_bool "Should not be able to lock a Procname that's already locked."
      (not (try_lock a_pname)))


let test_lock_after_unlock _test_ctxt =
  ProcLocker.(
    setup () ;
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    try_lock a_pname |> ignore ;
    unlock a_pname)


let test_unlocking_unlocked_fails _text_ctxt =
  ProcLocker.(
    setup () ;
    try_lock a_pname |> ignore ;
    unlock a_pname ;
    assert_raises (UnlockNotLocked a_pname) (fun () -> unlock a_pname))


let tests =
  "restart_scheduler_suite"
  >::: [ "test_try_lock_already_locked" >:: test_try_lock_already_locked
       ; "test_lock_after_unlock" >:: test_lock_after_unlock
       ; "test_unlocking_unlocked_fails" >:: test_unlocking_unlocked_fails ]
