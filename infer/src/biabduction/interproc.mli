(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Interprocedural Analysis *)

val analyze_procedure :
  BiabductionSummary.t InterproceduralAnalysis.t -> BiabductionSummary.t option
(** Run the biabduction analysis on the given procedure *)
