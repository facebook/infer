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

  (** a path from some procedure via the given passthroughs to the given call stack, with
      passthroughs for each callee *)
  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  (** get a path for each of the reportable flows to a sink in this trace *)
  val get_reportable_sink_paths : t -> trace_of_pname:(Procname.t -> t) -> sink_path list

  (** update sink with the given call site *)
  val with_callsite : t -> CallSite.t -> t

  val to_sink_loc_trace :
    ?desc_of_sink:(Sink.t -> string) -> ?sink_should_nest:(Sink.t -> bool) ->
    sink_path -> Errlog.loc_trace_elem list
end

module MakeSink(TraceElem : TraceElem.S) = struct
  include TraceElem
  let get _ _ = []
end

module Make (TraceElem : TraceElem.S) = struct
  include Trace.Make(struct
      module Source = Source.Dummy
      module Sink = MakeSink(TraceElem)
      let should_report _ _ = true
    end)

  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  let initial =
    let dummy_source = () in
    of_source dummy_source

  let get_reportable_sink_paths t ~trace_of_pname =
    IList.map
      (fun (passthroughs, _, sinks) -> passthroughs, sinks)
      (get_reportable_paths t ~trace_of_pname)

  let to_sink_loc_trace ?desc_of_sink ?sink_should_nest (passthroughs, sinks) =
    to_loc_trace ?desc_of_sink ?sink_should_nest (passthroughs, [], sinks)

  let with_callsite t call_site =
    IList.fold_left
      (fun t_acc sink ->
         let callee_sink = Sink.with_callsite sink call_site in
         add_sink callee_sink t_acc)
      initial
      (Sinks.elements (sinks t))
end
