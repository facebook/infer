(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val iter_captured_procs_and_callees : (Procname.t -> Procname.t list -> unit) -> unit
(** run a function on every captured procedure, passing its list of syntactic callees *)

val make : SourceFile.t list -> (TaskSchedulerTypes.target, 'a) ProcessPool.TaskGenerator.t
(** task generator that works by

    - loading the syntactic call graph from the capture DB
    - restricting it to the reachable procs from the modified files
    - scheduling leaves only and removing them from the graph when analysed. *)

val build_from_sources : SourceFile.t list -> CallGraph.t
(** construct the syntactic call graph from the capture DB *)

val to_dotty : CallGraph.t -> unit
(** write out the given syntactic call graph into [<results-dir>/syntactic-call-graph.dot] *)
