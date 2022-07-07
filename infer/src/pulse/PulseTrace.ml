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

type t =
  | Immediate of {location: Location.t; history: ValueHistory.t}
  | ViaCall of {f: CallEvent.t; location: Location.t; history: ValueHistory.t; in_call: t}
[@@deriving compare, equal]

let get_outer_location = function Immediate {location; _} | ViaCall {location; _} -> location

let get_outer_history = function Immediate {history; _} | ViaCall {history; _} -> history

let get_start_location trace =
  match ValueHistory.get_first_main_event (get_outer_history trace) with
  | Some event ->
      ValueHistory.location_of_event event
  | None ->
      get_outer_location trace


let rec pp ~pp_immediate fmt trace =
  if Config.debug_level_analysis < 3 then pp_immediate fmt
  else
    match trace with
    | Immediate {location; history} ->
        F.fprintf fmt "%a::(%a)%t" ValueHistory.pp history Location.pp location pp_immediate
    | ViaCall {f; location; history; in_call} ->
        F.fprintf fmt "%a::(%a)%a[%a]" ValueHistory.pp history CallEvent.pp f Location.pp location
          (pp ~pp_immediate) in_call


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


let rec synchronous_add_to_errlog ~nesting ~pp_immediate traces errlog =
  match traces with
  | [] ->
      errlog
  | _ ->
      let in_sync, out_sync =
        List.fold traces ~init:([], []) ~f:(fun (in_sync, out_sync) trace ->
            match in_sync with
            | [] ->
                (trace :: in_sync, out_sync)
            | trace2 :: _ ->
                let correspond t1 t2 =
                  match (t1, t2) with
                  | Immediate {location= loc1}, Immediate {location= loc2}
                  | ViaCall {location= loc1}, ViaCall {location= loc2} ->
                      Location.equal loc1 loc2
                  | _ ->
                      false
                in
                if correspond trace trace2 then (trace :: in_sync, out_sync)
                else (in_sync, trace :: out_sync) )
      in
      let add_in_sync_to_errlog errlog = function
        | [] ->
            errlog
        | Immediate {location} :: _ ->
            Errlog.make_trace_element nesting location (F.asprintf "%t" pp_immediate) [] :: errlog
        | ViaCall {f; location} :: _ as in_sync ->
            let traces =
              List.map in_sync ~f:(function
                | Immediate _ ->
                    Logging.die InternalError "Mismatching synchronous traces"
                | ViaCall {in_call} ->
                    in_call )
            in
            let errlog =
              synchronous_add_to_errlog ~nesting:(nesting + 1) ~pp_immediate traces errlog
            in
            Errlog.make_trace_element nesting location
              (F.asprintf "when calling %a here" CallEvent.pp f)
              []
            :: errlog
      in
      let errlog = add_in_sync_to_errlog errlog (List.rev in_sync) in
      synchronous_add_to_errlog ~nesting ~pp_immediate (List.rev out_sync) errlog


let rec iter trace ~f =
  let f_event = function ValueHistory.Event event -> f event | _ -> () in
  match trace with
  | Immediate {history} ->
      ValueHistory.iter_main history ~f:f_event
  | ViaCall {history; in_call} ->
      ValueHistory.iter_main history ~f:f_event ;
      iter in_call ~f


let find_map trace ~f = Container.find_map ~iter trace ~f

let exists trace ~f = Container.exists ~iter trace ~f

let has_invalidation trace =
  exists trace ~f:(function ValueHistory.Invalidated _ -> true | _ -> false)
