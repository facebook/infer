(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

exception ProcnameAlreadyLocked of Procname.t

val setup : unit -> unit

val clean : unit -> unit

val lock_exn : Procname.t -> unit

val unlock : Procname.t -> unit

val make : SourceFile.t list -> (SchedulerTypes.target, Procname.t) ProcessPool.TaskGenerator.t
