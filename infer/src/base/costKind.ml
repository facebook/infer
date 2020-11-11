(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = OperationCost | AllocationCost | AutoreleasepoolSize [@@deriving compare]

let to_issue_string = function
  | OperationCost ->
      "EXECUTION_TIME"
  | AllocationCost ->
      "ALLOCATION"
  | AutoreleasepoolSize ->
      "AUTORELEASEPOOL_SIZE"


let to_complexity_string = function
  | AllocationCost ->
      "Allocation complexity"
  | AutoreleasepoolSize ->
      "Autoreleasepool size"
  | OperationCost ->
      "Time complexity"


let pp f k =
  let k_str =
    match k with
    | OperationCost ->
        "Execution Cost"
    | AllocationCost ->
        "Allocation Cost"
    | AutoreleasepoolSize ->
        "Autoreleasepool Size"
  in
  F.pp_print_string f k_str


let to_json_cost_info c = function
  | OperationCost ->
      c.Jsonbug_t.exec_cost
  | AllocationCost ->
      assert false
  | AutoreleasepoolSize ->
      c.Jsonbug_t.autoreleasepool_size


type kind_spec = {kind: t; (* for non-diff analysis *) top_and_unreachable: bool; expensive: bool}

let enabled_cost_kinds =
  [ {kind= OperationCost; top_and_unreachable= true; expensive= false}
  ; {kind= AutoreleasepoolSize; top_and_unreachable= true; expensive= true} ]
