(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type access_type = Strong | Weak | Unknown [@@deriving compare, equal]

val count_references : Tenv.t -> AbductiveDomain.t -> int AbstractValue.Map.t

val removable_vars : Tenv.t -> AbductiveDomain.t -> Var.t list -> Var.t list

val get_access_type : Tenv.t -> Access.t -> access_type

val pp_access_type : Format.formatter -> access_type -> unit
