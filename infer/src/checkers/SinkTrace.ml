(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module type S = sig
  include Trace.S

  (** a path from some procedure via the given passthroughs to the given call stack, with
      passthroughs for each callee *)
  type sink_path = Passthroughs.t * (Sink.t * Passthroughs.t) list

  (** get a path for each of the reportable flows to a sink in this trace *)
  val get_reportable_sink_paths : t -> trace_of_pname:(Procname.t -> t) -> sink_path list

  (** convert of the sinks to callee sinks for the given call site *)
  val to_callee : t -> CallSite.t -> t
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

  let to_callee t call_site =
    IList.fold_left
      (fun t_acc sink ->
         let callee_sink = Sink.to_callee sink call_site in
         add_sink callee_sink t_acc)
      initial
      (Sinks.elements (sinks t))
end
