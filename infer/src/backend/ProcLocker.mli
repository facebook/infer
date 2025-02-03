(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val setup : unit -> unit
(** This should be called once before trying to lock Anything. *)

val try_lock : Procname.t -> [`AlreadyLockedByUs | `LockedByAnotherProcess | `LockAcquired]

val unlock : Procname.t -> unit
(** This will work as a cleanup function because after calling unlock all the workers that need an
    unlocked Proc should find it's summary already Cached. Throws if the lock had not been taken. *)

val lock_all :
  WorkerPoolState.worker_id -> string list -> [> `FailedToLockAll | `LocksAcquired of string list]

val unlock_all : string list -> unit
