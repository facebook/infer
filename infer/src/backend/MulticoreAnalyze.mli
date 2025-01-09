(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run_analysis :
     CallGraph.t option
  -> SourceFile.t list lazy_t
  -> Stats.t list * GCStats.t list * MissingDependencies.t list
