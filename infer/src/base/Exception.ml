(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type failure_kind =
  | FKtimeout  (** max time exceeded *)
  | FKsymops_timeout of int  (** max symop's exceeded *)
  | FKcrash of string  (** uncaught exception or failed assertion *)

(** failure that prevented biabduction analysis from finishing *)
exception Analysis_failure_exe of failure_kind

let exn_not_failure = function
  | Analysis_failure_exe _
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
  | exception (Analysis_failure_exe _ as f_exn) ->
      IExn.reraise_after f_exn ~f:(fun () ->
          try finally () with
          | MissingDependencyException.MissingDependencyException ->
              ()
          | finally_exn when RestartSchedulerException.is_not_restart_exception finally_exn ->
              (* swallow in favor of the original exception unless it's the restart scheduler exception *)
              () )
  | exception f_exn when RestartSchedulerException.is_not_restart_exception f_exn ->
      IExn.reraise_after f_exn ~f:(fun () ->
          try finally ()
          with
          | finally_exn
          when (* do not swallow Analysis_failure_exe or restart exception thrown from finally *)
               exn_not_failure finally_exn
          ->
            () )


let pp_failure_kind fmt = function
  | FKtimeout ->
      F.pp_print_string fmt "TIMEOUT"
  | FKsymops_timeout symops ->
      F.fprintf fmt "SYMOPS TIMEOUT (%d)" symops
  | FKcrash msg ->
      F.fprintf fmt "CRASH (%s)" msg
