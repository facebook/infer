(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Suffix of a normal trace: just sinks and passthroughs, but no sources *)
module type S = sig
  include Trace.S

  (** A path from some procedure via the given passthroughs to the given call stack, with
      passthroughs for each callee *)
  type sink_path = Passthrough.Set.t * (Sink.t * Passthrough.Set.t) list

  (** get a path for each of the reportable flows to a sink in this trace *)
  val get_reportable_sink_paths : t -> trace_of_pname:(Typ.Procname.t -> t) -> sink_path list

  (** update sink with the given call site *)
  val with_callsite : t -> CallSite.t -> t

  val to_sink_loc_trace :
    ?desc_of_sink:(Sink.t -> string) -> ?sink_should_nest:(Sink.t -> bool) ->
    sink_path -> Errlog.loc_trace_elem list
end

module MakeSink (TraceElem : TraceElem.S) :
  Sink.S with module Kind = TraceElem.Kind and type t = TraceElem.t

module Make (TraceElem : TraceElem.S) :
  S with module Source = Source.Dummy and module Sink = MakeSink(TraceElem)
