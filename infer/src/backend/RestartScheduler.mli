(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val setup : unit -> unit

val make :
     SourceFile.t list
  -> ( TaskSchedulerTypes.target
     , TaskSchedulerTypes.analysis_result
     , WorkerPoolState.worker_id )
     TaskGenerator.t

val with_lock :
  get_actives:(unit -> SpecializedProcname.t list) -> f:(unit -> 'a) -> Procname.t -> 'a
(** Run [f] after having taken a lock on the given [Procname.t] and unlock after. If the lock is
    already held by another worker, throw [RestartSchedulerException.ProcnameAlreadyLocked] so that
    the dependency can be sent to the scheduler process. Finally, account for time spent analysing
    each procedure as useful (finished analysis) or not (an exception was thrown, terminating
    analysis early). *)

val of_queue :
     TaskSchedulerTypes.target Queue.t
  -> ( TaskSchedulerTypes.target
     , TaskSchedulerTypes.analysis_result
     , WorkerPoolState.worker_id )
     TaskGenerator.t
(** exposed for testing *)

val release_reservations : Procname.t -> unit
(** To be called when the summary of the given procedure is found to be already computed. If the
    scheduler had reserved that procedure for the current job then the job most likely needs neither
    it nor the procedures it calls: release the reservations of the job, except for the procedures
    it is analyzing. *)

val forget_reservations : unit -> unit
(** to be called by workers before each job *)

val finish : TaskSchedulerTypes.analysis_result option -> 'a -> 'a option
