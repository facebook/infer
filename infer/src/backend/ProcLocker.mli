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

val unlock_all_owned : WorkerPoolState.worker_id -> string list -> unit
(** release the locks in the list that the given worker holds and leave the others alone *)

val is_locked_by_us : Procname.t -> bool
(** whether the current worker holds the lock of the procedure *)

val unlock_all_locked_by_us : except:string list -> unit
(** release all the locks that the current worker holds except the ones in [except] *)

(** the locks used with [--multicore], exposed for testing *)
module DomainLocks : sig
  val setup : unit -> unit

  val try_lock : Procname.t -> [`AlreadyLockedByUs | `LockedByAnotherProcess | `LockAcquired]

  val lock_all : int -> string list -> [> `FailedToLockAll | `LocksAcquired of string list]

  val unlock_all_owned : int -> string list -> unit

  val is_locked_by_us : Procname.t -> bool

  val unlock_all_locked_by_us : except:string list -> unit
end
