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

val store_previous_schedule : unit -> unit
(** stores to disk the callgraph obtained by adding edges from the [summary_loads] dependencies in
    the {!Summary.dependencies} field of every procedure; used to replay the previous analysis
    inter-procedural schedule *)

val store_previous_schedule_if_needed : unit -> unit
(** do [store_previous_schedule ()] if we have been asked to replay the analysis and there isn't a
    schedule file already; use if the results DB is about to be deleted, eg before the capture
    phase, otherwise [load_previous_schedule] takes care of selecting between loading from an
    existing schedule file or directly from summaries *)

val load_previous_schedule : unit -> CallGraph.t option
(** load from a file created with [store_previous_schedule] if present or from summaries otherwise *)
