(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module ValueHistory = PulseValueHistory

type 'a t =
  | Immediate of {imm: 'a; location: Location.t; history: ValueHistory.t}
  | ViaCall of {f: CallEvent.t; location: Location.t; history: ValueHistory.t; in_call: 'a t}
[@@deriving compare]

let rec get_immediate = function
  | Immediate {imm; _} ->
      imm
  | ViaCall {in_call; _} ->
      get_immediate in_call


let get_outer_location = function Immediate {location; _} | ViaCall {location; _} -> location

let get_history = function Immediate {history; _} | ViaCall {history; _} -> history

let get_start_location trace =
  match get_history trace |> List.last with
  | Some event ->
      ValueHistory.location_of_event event
  | None ->
      get_outer_location trace


let rec pp pp_immediate fmt = function
  | Immediate {imm; location= _; history} ->
      if Config.debug_level_analysis < 3 then pp_immediate fmt imm
      else F.fprintf fmt "%a::%a" ValueHistory.pp history pp_immediate imm
  | ViaCall {f; location= _; history; in_call} ->
      if Config.debug_level_analysis < 3 then pp pp_immediate fmt in_call
      else
        F.fprintf fmt "%a::%a[%a]" ValueHistory.pp history CallEvent.pp f (pp pp_immediate) in_call


let rec add_to_errlog ~nesting pp_immediate trace errlog =
  match trace with
  | Immediate {imm; location; history} ->
      ValueHistory.add_to_errlog ~nesting history
      @@ Errlog.make_trace_element nesting location (F.asprintf "%a" pp_immediate imm) []
         :: errlog
  | ViaCall {f; location; in_call; history} ->
      ValueHistory.add_to_errlog ~nesting history
      @@ (fun errlog ->
           Errlog.make_trace_element nesting location
             (F.asprintf "when calling %a here" CallEvent.pp f)
             []
           :: errlog )
      @@ add_to_errlog ~nesting:(nesting + 1) pp_immediate in_call
      @@ errlog
