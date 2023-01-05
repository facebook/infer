(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val build : changed_files:SourceFile.Set.t -> CallGraph.t
(** Build the graph from the summaries in the .specs files, and flag any transitive dependencies of
    the [changed_files] *)
