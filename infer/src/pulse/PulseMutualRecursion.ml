(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
module PathContext = PulsePathContext

type t = Trace.t

let mk {PathContext.timestamp} location call =
  Trace.Immediate
    { location
    ; history=
        ValueHistory.singleton
          (Call {f= Call call; location; timestamp; in_call= ValueHistory.epoch}) }


let get_inner_call trace =
  match Trace.get_immediate trace with
  | _loc, Sequence (Call {f= Call orig_proc_name}, Epoch) ->
      orig_proc_name
  | _ ->
      L.die InternalError "unexpected trace in recursive call detection: %a"
        (Trace.pp ~pp_immediate:(fun _ -> ()))
        trace


let rec pp fmt (trace : Trace.t) =
  match trace with
  | Immediate _ ->
      let proc_name = get_inner_call trace in
      CallEvent.pp fmt (Call proc_name)
  | ViaCall {f; in_call} ->
      F.fprintf fmt "%a -> " CallEvent.pp f ;
      pp fmt in_call
