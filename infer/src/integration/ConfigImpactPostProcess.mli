(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val instantiate_unchecked_callees_cond :
  ConfigImpactAnalysis.Summary.t -> ConfigImpactAnalysis.Summary.t
(** Instantiate conditional unchecked callees with collected config fields. WARN: This function must
    be called after the summaries for all procedures completed, because it evaluates a lazy value
    internally. *)

val is_in_gated_classes : Procname.t -> bool
(** Check if a procedure is in a gated class. WARN: This function must be called after the summaries
    for all procedures completed, because it evaluates a lazy value internally. *)
