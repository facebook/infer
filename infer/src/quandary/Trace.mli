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

module type Spec = sig
  module Source : Source.S
  module Sink : Sink.S

  (** should a flow originating at source and entering sink be reported? *)
  val should_report : Source.t -> Sink.t -> bool

  (** get a loggable exception reporting a flow from source -> sink *)
  val get_reportable_exn : Source.t -> Sink.t -> Passthrough.Set.t -> exn
end

module type S = sig
  include Spec
  type t
  type astate = t
  include AbstractDomain.S with type astate := astate

  module Sources = Source.Set
  module Sinks = Sink.Set
  module Passthroughs = Passthrough.Set

  (** get the sources of the trace. *)
  val sources : t -> Sources.t

  (** get the sinks of the trace *)
  val sinks : t -> Sinks.t

  (** get the passthroughs of the trace *)
  val passthroughs : t -> Passthroughs.t

  (** get the reportable source-sink flows in this trace *)
  val get_reports : t -> (Source.t * Sink.t * Passthroughs.t) list

  (** get logging-ready exceptions for the reportable source-sink flows in this trace *)
  val get_reportable_exns : t -> exn list

  (** create a trace from a source *)
  val of_source : Source.t -> t

  (** ad a source to the current trace *)
  val add_source : Source.t -> t -> t

  (** add a sink to the current trace. *)
  val add_sink : Sink.t -> t -> t

  (** append the trace for given call site to the current caller trace *)
  val append : t -> t -> CallSite.t -> t

  (** return true if this trace has no source or sink data *)
  val is_empty : t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val pp : F.formatter -> t -> unit
end

module Make (Spec : Spec) : S with module Source = Spec.Source and module Sink = Spec.Sink
