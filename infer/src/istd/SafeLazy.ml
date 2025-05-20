(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a content = Eager of 'a | Lazy of {mutex: IMutex.t; lazy_v: 'a Lazy.t}

(** Thread safe lazy evaluation

    An atomic reference is used to avoid taking a lock for already-forced values, yet still
    guaranteeing thread safety. An unforced value is paired with mutex. When forcing, the lock is
    taken and if that fails (because another thread is already forcing the value) then we retry. If
    the lock taking succeeds, we recheck that the atomic reference hasn't changed, in case another
    thread just released the lock, having changed the atomic reference in the meantime. If the
    atomic hasn't changed, forcing happens and the atomic is set to the now evaluated version of the
    value. *)
type 'a t = 'a content Atomic.t

let make lazy_v = Atomic.make (Lazy {mutex= IMutex.create (); lazy_v})

let rec force t =
  match Atomic.get t with
  | Eager v ->
      v
  | Lazy {mutex} when Stdlib.Mutex.try_lock mutex ->
      Exn.protect
        ~finally:(fun () -> Stdlib.Mutex.unlock mutex)
        ~f:(fun () ->
          match Atomic.get t with
          | Eager v ->
              v
          | Lazy {lazy_v} ->
              let result = Lazy.force lazy_v in
              Atomic.set t (Eager result) ;
              result )
  | Lazy _ ->
      (* the lock was taken by another thread, retry *)
      Domain.cpu_relax () ;
      force t


let force_option v_opt = Option.map ~f:force v_opt

let from_val v = Atomic.make (Eager v)

let from_val_option v = Option.map ~f:from_val v
