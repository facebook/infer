(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
module PathContext = PulsePathContext

type t = Trace.t

let mk {PathContext.timestamp} location call =
  Trace.Immediate
    { location
    ; history=
        ValueHistory.singleton
          (Call {f= Call call; location; timestamp; in_call= ValueHistory.epoch}) }
