(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val bottom_up : SourceFile.t list -> SchedulerTypes.target ProcessPool.TaskGenerator.t
(** task generator that works by 
    - loading the syntactic call graph from the capture DB 
    - restricting it to the reachable procs from the modified files
    - scheduling leaves only and removing them from the graph when analysed.
*)
