(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module type S = sig
  include TaintTrace.S

  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  val get_reportable_sink_paths : t -> trace_of_pname:(Procname.t -> t) -> sink_path list

  val get_reportable_sink_path : Sink.t -> trace_of_pname:(Procname.t -> t) -> sink_path option

  val with_callsite : t -> CallSite.t -> t

  val of_sink : Sink.t -> t

  val to_sink_loc_trace :
       ?desc_of_sink:(Sink.t -> string)
    -> ?sink_should_nest:(Sink.t -> bool)
    -> sink_path
    -> Errlog.loc_trace_elem list
end

module MakeSink (TraceElem : TaintTraceElem.S) = struct
  include TraceElem

  let get _ _ _ _ = []

  let indexes _ = IntSet.empty

  let with_indexes _ _ = assert false
end

module Make (TraceElem : TaintTraceElem.S) = struct
  include TaintTrace.Make (struct
    module Source = Source.Dummy
    module Sanitizer = Sanitizer.Dummy
    module Sink = MakeSink (TraceElem)

    let get_report _ _ _ = Some IssueType.do_not_report
  end)

  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  let bottom =
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
        add_sink callee_sink t_acc )
      ~init:bottom
      (Sinks.elements (sinks t))


  let of_sink sink =
    let sinks = Sinks.add sink Sinks.empty in
    update_sinks bottom sinks


  let get_reportable_sink_path sink ~trace_of_pname =
    match get_reportable_sink_paths (of_sink sink) ~trace_of_pname with
    | [] ->
        None
    | [report] ->
        Some report
    | _ ->
        L.(die InternalError) "Should not get >1 report for 1 sink"


  let pp fmt t =
    let pp_passthroughs_if_not_empty fmt p =
      if not (Passthroughs.is_empty p) then F.fprintf fmt " via %a" Passthroughs.pp p
    in
    F.fprintf fmt "%a%a" Sinks.pp (sinks t) pp_passthroughs_if_not_empty (passthroughs t)
end
