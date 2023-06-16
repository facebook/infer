(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val invalidate : changed_files:SourceFile.Set.t option -> unit
(** Build a dependency graph from the summaries in the specs table, flag any transitive dependencies
    of the [changed_files], then invalidate any flagged transitive dependencies. *)

val from_summaries : unit -> CallGraph.t
(** the callgraph obtained by adding edges from the [summary_loads] dependencies in the
    {!Summary.dependencies} field of every procedure; used to replay the previous analysis
    inter-procedural schedule *)
