(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | AnnotMap
  | Biabduction
  | BufferOverrunAnalysis
  | BufferOverrunChecker
  | ConfigImpactAnalysis
  | Cost
  | DisjunctiveDemo
  | DotnetResourceLeaks
  | LabResourceLeaks
  | LithoRequiredProps
  | Pulse
  | Purity
  | Quandary
  | RacerD
  | SIOF
  | SimpleLineage
  | SimpleShape
  | Starvation
  | Nullsafe
  | Uninit
[@@deriving variants]

let database_fields = List.map ~f:fst Variants.descriptions
