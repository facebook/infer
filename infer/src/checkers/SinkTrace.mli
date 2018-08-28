(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Suffix of a normal trace: just sinks and passthroughs, but no sources *)
module type S = sig
  include Trace.S

  (** A path from some procedure via the given passthroughs to the given call stack, with
      passthroughs for each callee *)
  type sink_path = Passthrough.Set.t * (Sink.t * Passthrough.Set.t) list

  val get_reportable_sink_paths : t -> trace_of_pname:(Typ.Procname.t -> t) -> sink_path list
  (** get a path for each of the reportable flows to a sink in this trace *)

  val get_reportable_sink_path : Sink.t -> trace_of_pname:(Typ.Procname.t -> t) -> sink_path option
  (** get a report for a single sink *)

  val with_callsite : t -> CallSite.t -> t
  (** update sink with the given call site *)

  val of_sink : Sink.t -> t

  val to_sink_loc_trace :
       ?desc_of_sink:(Sink.t -> string)
    -> ?sink_should_nest:(Sink.t -> bool)
    -> sink_path
    -> Errlog.loc_trace_elem list
end

module MakeSink (TraceElem : TraceElem.S) :
  Sink.S with module Kind = TraceElem.Kind and type t = TraceElem.t

module Make (TraceElem : TraceElem.S) :
  S with module Source = Source.Dummy and module Sink = MakeSink(TraceElem)
