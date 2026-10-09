(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val checker :
     ?specialization:PulseSummary.t * Specialization.t
  -> PulseSummary.t InterproceduralAnalysis.t
  -> PulseSummary.t option

val is_already_specialized : Specialization.t -> PulseSummary.t -> bool

val mark_specialization_failed : Specialization.t -> PulseSummary.t -> PulseSummary.t
(** record that the analysis of a specialization timed out: for the rest of the run, callers use the
    main summary instead of analyzing the specialization again *)
