(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = OperationCost | AllocationCost | IOCost [@@deriving compare]

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_issue_string : t -> string

val enabled_cost_kinds : ((Jsonbug_t.cost_item -> Jsonbug_t.cost_info) * t) list
