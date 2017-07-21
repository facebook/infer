(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

module type S = sig
  include Trace.S

  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  val get_reportable_sink_paths : t -> trace_of_pname:(Typ.Procname.t -> t) -> sink_path list

  val get_reportable_sink_path : Sink.t -> trace_of_pname:(Typ.Procname.t -> t) -> sink_path option

  val with_callsite : t -> CallSite.t -> t

  val of_sink : Sink.t -> t

  val to_sink_loc_trace :
    ?desc_of_sink:(Sink.t -> string) -> ?sink_should_nest:(Sink.t -> bool) -> sink_path
    -> Errlog.loc_trace_elem list
end

module MakeSink (TraceElem : TraceElem.S) = struct
  include TraceElem

  let get _ _ _ = None

  let indexes _ = IntSet.empty
end

module Make (TraceElem : TraceElem.S) = struct
  include Trace.Make (struct
    module Source = Source.Dummy
    module Sink = MakeSink (TraceElem)

    let should_report _ _ = true
  end)

  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  let empty =
    let dummy_source = () in
    of_source dummy_source

  let get_reportable_sink_paths t ~trace_of_pname =
    List.map
      ~f:(fun (passthroughs, _, sinks) -> (passthroughs, sinks))
      (get_reportable_paths t ~trace_of_pname)

  let to_sink_loc_trace ?desc_of_sink ?sink_should_nest (passthroughs, sinks) =
    to_loc_trace ?desc_of_sink ?sink_should_nest (passthroughs, [], sinks)

  let with_callsite t call_site =
    List.fold
      ~f:(fun t_acc sink ->
        let callee_sink = Sink.with_callsite sink call_site in
        add_sink callee_sink t_acc)
      ~init:empty
      (Sinks.elements (sinks t))

  let of_sink sink =
    let sinks = Sinks.add sink Sinks.empty in
    update_sinks empty sinks

  let get_reportable_sink_path sink ~trace_of_pname =
    match get_reportable_sink_paths (of_sink sink) ~trace_of_pname with
    | []
     -> None
    | [report]
     -> Some report
    | _
     -> failwithf "Should not get >1 report for 1 sink"

  let pp fmt t =
    let pp_passthroughs_if_not_empty fmt p =
      if not (Passthroughs.is_empty p) then F.fprintf fmt " via %a" Passthroughs.pp p
    in
    F.fprintf fmt "%a%a" Sinks.pp (sinks t) pp_passthroughs_if_not_empty (passthroughs t)
end
