(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module AbductiveDomain = PulseAbductiveDomain
module AbstractValue = PulseAbstractValue
module CallEvent = PulseCallEvent
module Decompiler = PulseDecompiler

let add_call_source v call astate =
  AbductiveDomain.map_decompiler astate ~f:(fun decompiler ->
      Decompiler.add_call_source v call decompiler )


type expr = Decompiler.expr [@@deriving compare, equal, yojson_of]

let pp_expr = Decompiler.pp_expr

let find v {AbductiveDomain.decompiler} = Decompiler.find v decompiler

let abstract_value_of_expr = Decompiler.abstract_value_of_expr
