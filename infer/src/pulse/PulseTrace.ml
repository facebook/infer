(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module CallEvent = PulseCallEvent
module Invalidation = PulseInvalidation
module ValueHistory = PulseValueHistory

type t =
  | Immediate of {location: Location.t; history: ValueHistory.t}
  | ViaCall of {f: CallEvent.t; location: Location.t; history: ValueHistory.t; in_call: t}
[@@deriving compare, equal]

let rec trace_up_to_key_event ~is_key_event history =
  match (history : ValueHistory.t) with
  | [] ->
      None
  | event :: past when is_key_event event ->
      Some (Immediate {location= ValueHistory.location_of_event event; history= past})
  | Call {f; location; in_call} :: past ->
      Option.map (trace_up_to_key_event ~is_key_event in_call) ~f:(fun in_call_trace ->
          ViaCall {f; location; in_call= in_call_trace; history= past} )
  | _event :: past ->
      (* [_event ≠ key_event]: we are past the key event if it exists *)
      trace_up_to_key_event ~is_key_event past


let get_outer_location = function Immediate {location; _} | ViaCall {location; _} -> location

let get_outer_history = function Immediate {history; _} | ViaCall {history; _} -> history

let get_start_location trace =
  match get_outer_history trace |> List.last with
  | Some event ->
      ValueHistory.location_of_event event
  | None ->
      get_outer_location trace


let rec pp ~pp_immediate fmt trace =
  if Config.debug_level_analysis < 3 then pp_immediate fmt
  else
    match trace with
    | Immediate {location= _; history} ->
        F.fprintf fmt "%a::%t" ValueHistory.pp history pp_immediate
    | ViaCall {f; location= _; history; in_call} ->
        F.fprintf fmt "%a::%a[%a]" ValueHistory.pp history CallEvent.pp f (pp ~pp_immediate) in_call


let rec add_to_errlog ?(include_value_history = true) ~nesting ~pp_immediate trace errlog =
  match trace with
  | Immediate {location; history} ->
      let acc =
        Errlog.make_trace_element nesting location (F.asprintf "%t" pp_immediate) [] :: errlog
      in
      if include_value_history then ValueHistory.add_to_errlog ~nesting history @@ acc else acc
  | ViaCall {f; location; in_call; history} ->
      let acc =
        (fun errlog ->
          Errlog.make_trace_element nesting location
            (F.asprintf "when calling %a here" CallEvent.pp f)
            []
          :: errlog )
        @@ add_to_errlog ~include_value_history ~nesting:(nesting + 1) ~pp_immediate in_call
        @@ errlog
      in
      if include_value_history then ValueHistory.add_to_errlog ~nesting history @@ acc else acc


let rec iter trace ~f =
  match trace with
  | Immediate {history} ->
      ValueHistory.iter history ~f
  | ViaCall {history; in_call} ->
      ValueHistory.iter history ~f ;
      iter in_call ~f


let find_map trace ~f = Container.find_map ~iter trace ~f

let exists trace ~f = Container.exists ~iter trace ~f

let get_invalidation trace =
  find_map trace ~f:(function
    | ValueHistory.Invalidated (invalidation, _) ->
        Some invalidation
    | _ ->
        None )


let has_invalidation trace =
  exists trace ~f:(function ValueHistory.Invalidated _ -> true | _ -> false)
