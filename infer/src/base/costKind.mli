(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = OperationCost | AllocationCost [@@deriving compare]

type kind_spec = {kind: t; (* for non-diff analysis *) top_and_unreachable: bool; expensive: bool}

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val to_complexity_string : t -> string

val to_issue_string : t -> string

val to_json_cost_info : Jsoncost_t.item -> t -> Jsoncost_t.info

val enabled_cost_kinds : kind_spec list
