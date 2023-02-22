(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* This type has to be in sync with Payloads.t *)
type t =
  | AnnotMap
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ConfigImpactAnalysis
  | Cost
  | DisjunctiveDemo
  | LabResourceLeaks
  | LithoRequiredProps
  | Pulse
  | Purity
  | Quandary
  | RacerD
  | ScopeLeakage
  | SIOF
  | SimpleLineage
  | SimpleShape
  | Starvation
  | Nullsafe
  | Uninit
[@@deriving variants]

val database_fields : string list
