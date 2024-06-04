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
  -> (TaskSchedulerTypes.target, TaskSchedulerTypes.analysis_result) ProcessPool.TaskGenerator.t

val with_lock : f:(unit -> 'a) -> Procname.t -> 'a
(** Run [f] after having taken a lock on the given [Procname.t] and unlock after. If the lock is
    already held by another worker, throw [RestartSchedulerException.ProcnameAlreadyLocked] so that
    the dependency can be sent to the scheduler process. Finally, account for time spent analysing
    each procedure as useful (finished analysis) or not (an exception was thrown, terminating
    analysis early). *)
