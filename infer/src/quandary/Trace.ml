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

  (** get the source of the trace *)
  val source : t -> Source.t

  (** get the sink of the trace, if any *)
  val sink : t -> Sink.t option

  (** return true if this trace represent a dangerous flow *)
  val should_report : t -> bool

  (** create a trace from a source *)
  val of_source : Source.t -> t

  (** add a sink to the current trace. *)
  val add_sink : t -> Sink.t -> t

  val compare : t -> t -> int
  val pp : F.formatter -> t -> unit
end

module Make (Spec : Spec) = struct
  include Spec

  type t =
    {
      source : Source.t; (** last function in the trace that returned tainted data *)
      sink : Sink.t option;
      (** last callee in the trace that transitively called a tainted function (if any) *)
      passthroughs : Passthrough.Set.t; (** calls that occurred between source and sink *)
    }

  let compare t1 t2 =
    let compare_sink_opts sink1_opt sink2_opt = match sink1_opt, sink2_opt with
      | Some sink1, Some sink2 -> Sink.compare sink1 sink2
      | None, None -> 0
      | Some _, None -> (-1)
      | None, Some _ -> 1 in
    Source.compare t1.source t2.source
    |> next compare_sink_opts t1.sink t2.sink
    |> next Passthrough.Set.compare t1.passthroughs t2.passthroughs

  let pp fmt t =
    let pp_sink_opt fmt = function
      | None -> F.fprintf fmt "?"
      | Some sink -> Sink.pp fmt sink in
    F.fprintf
      fmt
      "%a -> %a via %a"
      Source.pp t.source pp_sink_opt t.sink Passthrough.Set.pp t.passthroughs

  let source t =
    t.source

  let sink t =
    t.sink

  let should_report t = match t.sink with
    | Some sink -> Spec.should_report t.source sink
    | None -> false

  let of_source source =
    let passthroughs = Passthrough.Set.empty in
    let sink = None in
    { source; passthroughs; sink; }

  let add_sink t sink =
    { t with sink = Some sink; }
end
