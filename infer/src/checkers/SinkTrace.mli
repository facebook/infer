(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Suffix of a normal trace: just sinks and passthroughs, but no sources *)
module type S = sig
  include Trace.S

  (** A path from some procedure via the given passthroughs to the given call stack, with
      passthroughs for each callee *)
  type sink_path = Passthrough.Set.t * (Sink.t * Passthrough.Set.t) list

  (** get a path for each of the reportable flows to a sink in this trace *)
  val get_reportable_sink_paths : t -> trace_of_pname:(Procname.t -> t) -> sink_path list

  (** convert of the sinks to callee sinks for the given call site *)
  val to_callee : t -> CallSite.t -> t
end

module MakeSink (TraceElem : TraceElem.S) :
  Sink.S with module Kind = TraceElem.Kind and type t = TraceElem.t

module Make (TraceElem : TraceElem.S) :
  S with module Source = Source.Dummy and module Sink = MakeSink(TraceElem)
