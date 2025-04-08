(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let exn_not_failure = function
  | RestartSchedulerException.ProcnameAlreadyLocked _
  | MissingDependencyException.MissingDependencyException ->
      false
  | _ ->
      true


let try_finally ~f ~finally =
  match f () with
  | r ->
      finally () ;
      r
  | exception f_exn when RestartSchedulerException.is_not_restart_exception f_exn ->
      IExn.reraise_after f_exn ~f:(fun () ->
          try finally ()
          with
          | finally_exn
          when (* do not swallow restart exception thrown from finally *)
               exn_not_failure finally_exn
          ->
            () )
