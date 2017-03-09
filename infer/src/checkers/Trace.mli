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

module type Spec = sig
  module Source : Source.S
  module Sink : Sink.S

  (** should a flow originating at source and entering sink be reported? *)
  val should_report : Source.t -> Sink.t -> bool
end

module type S = sig
  include Spec
  type t
  type astate = t
  include AbstractDomain.WithBottom with type astate := astate

  module Sources = Source.Set
  module Sinks = Sink.Set
  module Passthroughs = Passthrough.Set

  (** path from a source to a sink with passthroughs at each step in the call stack. the first set
      of passthroughs are the ones in the "reporting" procedure that calls the first function in
      both the source and sink stack *)
  type path = Passthroughs.t * (Source.t * Passthroughs.t) list * (Sink.t * Passthroughs.t) list

  (** the empty trace *)
  val empty : t

  (** get the sources of the trace. *)
  val sources : t -> Sources.t

  (** get the sinks of the trace *)
  val sinks : t -> Sinks.t

  (** get the passthroughs of the trace *)
  val passthroughs : t -> Passthroughs.t

  (** get the reportable source-sink flows in this trace. specifying [cur_site] restricts the
      reported paths to ones introduced by the call at [cur_site]  *)
  val get_reports : ?cur_site:CallSite.t -> t -> (Source.t * Sink.t * Passthroughs.t) list

  (** get a path for each of the reportable source -> sink flows in this trace. specifying
      [cur_site] restricts the reported paths to ones introduced by the call at [cur_site] *)
  val get_reportable_paths :
    ?cur_site:CallSite.t -> t -> trace_of_pname:(Typ.Procname.t -> t) -> path list

  (** create a loc_trace from a path; [source_should_nest s] should be true when we are going one
      deeper into a call-chain, ie when lt_level should be bumper in the next loc_trace_elem, and
      similarly for [sink_should_nest] *)
  val to_loc_trace :
    ?desc_of_source:(Source.t -> string) -> ?source_should_nest:(Source.t -> bool) ->
    ?desc_of_sink:(Sink.t -> string) -> ?sink_should_nest:(Sink.t -> bool) ->
    path -> Errlog.loc_trace

  (** create a trace from a source *)
  val of_source : Source.t -> t

  (** ad a source to the current trace *)
  val add_source : Source.t -> t -> t

  (** add a sink to the current trace. *)
  val add_sink : Sink.t -> t -> t

  (** replace sinks with new ones *)
  val update_sinks : t -> Sinks.t -> t

  (** append the trace for given call site to the current caller trace *)
  val append : t -> t -> CallSite.t -> t

  (** return true if this trace has no source or sink data *)
  val is_empty : t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit

  (** pretty-print a path in the context of the given procname *)
  val pp_path : Typ.Procname.t -> F.formatter -> path -> unit
end

module Make (Spec : Spec) : S with module Source = Spec.Source and module Sink = Spec.Sink
