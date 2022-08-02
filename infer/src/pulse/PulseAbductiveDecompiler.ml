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
module DecompilerExpr = PulseDecompilerExpr
module ValueHistory = PulseValueHistory

let add_call_source v call actuals astate =
  AbductiveDomain.map_decompiler astate ~f:(fun decompiler ->
      Decompiler.add_call_source v call actuals decompiler )


let find v {AbductiveDomain.decompiler} = Decompiler.find v decompiler
