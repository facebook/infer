(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = OperationCost | AllocationCost | IOCost [@@deriving compare]

type kind_spec = {kind: t; (* for non-diff analysis *) top_and_bottom: bool}

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_complexity_string : t -> string

val to_issue_string : t -> string

val to_json_cost_info : Jsonbug_t.cost_item -> t -> Jsonbug_t.cost_info

val enabled_cost_kinds : kind_spec list

val to_threshold : t -> int option
