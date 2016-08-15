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
end

module type S = sig
  include Spec
  type t

  (** get the sources of the trace. this should never be empty *)
  val sources : t -> Source.Set.t

  (** get the sinks of the trace *)
  val sinks : t -> Sink.Set.t

  (** get the reportable source-sink flows in this trace *)
  val get_reports : t -> (Source.t * Sink.t * Passthrough.Set.t) list

  (** create a trace from a source *)
  val of_source : Source.t -> t

  (** ad a source to the current trace *)
  val add_source : Source.t -> t -> t

  (** add a sink to the current trace. *)
  val add_sink : Sink.t -> t -> t

  val compare : t -> t -> int

  val pp : F.formatter -> t -> unit
end

module Make (Spec : Spec) = struct
  include Spec

  module Sources = Source.Set
  module Sinks = Sink.Set

  type t =
    {
      sources : Sources.t; (** last functions in the trace that returned tainted data *)
      sinks : Sinks.t;
      (** last callees in the trace that transitively called a tainted function (if any) *)
      passthroughs : Passthrough.Set.t; (** calls that occurred between source and sink *)
    }

  let compare t1 t2 =
    Sources.compare t1.sources t2.sources
    |> next Sinks.compare t1.sinks t2.sinks
    |> next Passthrough.Set.compare t1.passthroughs t2.passthroughs

  let pp fmt t =
    F.fprintf
      fmt
      "%a -> %a via %a"
      Sources.pp t.sources Sinks.pp t.sinks Passthrough.Set.pp t.passthroughs

  let sources t =
    t.sources

  let sinks t =
    t.sinks

  let get_reports t =
    if Sinks.is_empty t.sinks
    then []
    else
      let report_one source sink acc =
        if Spec.should_report source sink
        then (source, sink, t.passthroughs) :: acc
        else acc in
      Sources.fold (fun source acc -> Sinks.fold (report_one source) t.sinks acc) t.sources []

  let of_source source =
    let sources = Sources.singleton source in
    let passthroughs = Passthrough.Set.empty in
    let sinks = Sinks.empty in
    { sources; passthroughs; sinks; }

  let add_source source t =
    let sources = Sources.add source t.sources in
    { t with sources; }

  let add_sink sink t =
    let sinks = Sinks.add sink t.sinks in
    { t with sinks; }
end
