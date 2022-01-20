(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

val count_references : Tenv.t -> AbductiveDomain.t -> int AbstractValue.Map.t

val removable_vars : Tenv.t -> AbductiveDomain.t -> Var.t list -> Var.t list
