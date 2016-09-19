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

module Make (Spec : Spec) = struct
  include Spec

  module Sources = Source.Set
  module Sinks = Sink.Set
  module Passthroughs = Passthrough.Set

  type t =
    {
      sources : Sources.t; (** last functions in the trace that returned tainted data *)
      sinks : Sinks.t;
      (** last callees in the trace that transitively called a tainted function (if any) *)
      passthroughs : Passthrough.Set.t; (** calls that occurred between source and sink *)
    }

  type astate = t

  let compare t1 t2 =
    Sources.compare t1.sources t2.sources
    |> next Sinks.compare t1.sinks t2.sinks
    |> next Passthroughs.compare t1.passthroughs t2.passthroughs

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp fmt t =
    F.fprintf
      fmt
      "%a -> %a via %a"
      Sources.pp t.sources Sinks.pp t.sinks Passthroughs.pp t.passthroughs

  let sources t =
    t.sources

  let sinks t =
    t.sinks

  let passthroughs t =
    t.passthroughs

  let is_empty t =
    (* sources empty => sinks empty and passthroughs empty *)
    Sources.is_empty t.sources

  let get_reports t =
    if Sinks.is_empty t.sinks
    then []
    else
      let report_one source sink acc =
        if Spec.should_report source sink
        then (source, sink, t.passthroughs) :: acc
        else acc in
      Sources.fold (fun source acc -> Sinks.fold (report_one source) t.sinks acc) t.sources []

  let get_reportable_exns t =
    IList.map
      (fun (source, sink, passthroughs) -> Spec.get_reportable_exn source sink passthroughs)
      (get_reports t)

  let of_source source =
    let sources = Sources.singleton source in
    let passthroughs = Passthroughs.empty in
    let sinks = Sinks.empty in
    { sources; passthroughs; sinks; }

  let add_source source t =
    let sources = Sources.add source t.sources in
    { t with sources; }

  let add_sink sink t =
    let sinks = Sinks.add sink t.sinks in
    { t with sinks; }

  (** compute caller_trace + callee_trace *)
  let append caller_trace callee_trace callee_site =
    if is_empty callee_trace
    then caller_trace
    else
      let sources =
        Sources.filter (fun source -> not (Source.is_footprint source)) callee_trace.sources
        |> Sources.union caller_trace.sources in
      let sinks = Sinks.union caller_trace.sinks callee_trace.sinks in
      let passthroughs =
        let joined_passthroughs =
          Passthroughs.union caller_trace.passthroughs callee_trace.passthroughs in
        if Sinks.is_empty callee_trace.sinks
        then Passthroughs.add (Passthrough.make callee_site) joined_passthroughs
        else joined_passthroughs in
      { sources; sinks; passthroughs; }

  let initial =
    let sources = Sources.empty in
    let sinks = Sinks.empty in
    let passthroughs = Passthroughs.empty in
    { sources; sinks; passthroughs; }

  let (<=) ~lhs ~rhs =
    lhs == rhs ||
    (Sources.subset lhs.sources rhs.sources &&
     Sinks.subset lhs.sinks rhs.sinks &&
     Passthroughs.subset lhs.passthroughs rhs.passthroughs)

  let join t1 t2 =
    if t1 == t2
    then t1
    else
      let sources = Sources.union t1.sources t2.sources in
      let sinks = Sinks.union t1.sinks t2.sinks in
      let passthroughs = Passthroughs.union t1.passthroughs t2.passthroughs in
      { sources; sinks; passthroughs; }

  let widen ~prev ~next ~num_iters:_ =
    join prev next
end
