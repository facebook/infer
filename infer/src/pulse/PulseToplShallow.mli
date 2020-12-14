(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val analyze : PulseSummary.t Topl.analysis_transformer
(** Run pulse with Topl instrumentation if active. Inserts calls to the Topl automaton. Mutates the
    arguments: it is the caller's responsibility to instrument procedures at most once. *)
