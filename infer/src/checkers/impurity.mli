(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker : IntraproceduralAnalysis.t -> PulseSummary.t option -> unit
(** An impurity analysis that relies on pulse summaries to determine how the state changes *)
