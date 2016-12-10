(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging
module F = Format

module MockTraceElem = struct
  type t =
    | Kind1
    | Kind2
    | Footprint

  let call_site _ = CallSite.dummy

  let kind t = t

  let make kind _ = kind

  let compare t1 t2 =
    match t1, t2 with
    | Kind1, Kind1 -> 0
    | Kind1, _ -> (-1)
    | _, Kind1 -> 1
    | Kind2, Kind2 -> 0
    | Kind2, _ -> (-1)
    | _, Kind2 -> 1
    | Footprint, Footprint -> 0

  let equal t1 t2 =
    compare t1 t2 = 0

  let pp fmt = function
    | Kind1 -> F.fprintf fmt "Kind1"
    | Kind2 -> F.fprintf fmt "Kind2"
    | Footprint -> F.fprintf fmt "Footprint"

  module Kind = struct
    type nonrec t = t
    let compare = compare
    let pp = pp
  end

  module Set = PrettyPrintable.MakePPSet(struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)

  let with_callsite t _ = t
end

module MockSource = struct
  include MockTraceElem

  let make = MockTraceElem.make

  let is_footprint kind =
    kind = Footprint

  let make_footprint _ _ = Footprint

  let get _ = assert false
  let get_footprint_access_path _ = assert false
end

module MockSink = struct
  include MockTraceElem


  let get _ = assert false
end


module MockTrace = Trace.Make(struct
    module Source = MockSource
    module Sink = MockSink

    let should_report source sink =
      Source.kind source = Sink.kind sink
  end)

let tests =
  let source1 = MockSource.make MockTraceElem.Kind1 CallSite.dummy in
  let source2 = MockSource.make MockTraceElem.Kind2 CallSite.dummy in
  let sink1 = MockSink.make MockTraceElem.Kind1 CallSite.dummy in
  let sink2 = MockSink.make MockTraceElem.Kind2 CallSite.dummy in

  let open OUnit2 in
  let get_reports =
    let get_reports_ _ =
      let trace =
        MockTrace.of_source source1
        |> MockTrace.add_source source2
        |> MockTrace.add_sink sink1
        |> MockTrace.add_sink sink2 in
      let reports = MockTrace.get_reports trace in

      assert_equal (IList.length reports) 2;
      assert_bool
        "Reports should contain source1 -> sink1"
        (IList.exists
           (fun (source, sink, _) -> MockSource.equal source source1 && MockSink.equal sink sink1)
           reports);
      assert_bool
        "Reports should contain source2 -> sink2"
        (IList.exists
           (fun (source, sink, _) -> MockSource.equal source source2 && MockSink.equal sink sink2)
           reports) in
    "get_reports">::get_reports_ in

  let append =
    let append_ _ =
      let call_site = CallSite.dummy in
      let footprint_source = MockSource.make_footprint MockTraceElem.Kind1 call_site in
      let source_trace =
        MockTrace.of_source source1 in
      let footprint_trace =
        MockTrace.of_source footprint_source
        |> MockTrace.add_sink sink1 in

      let expected_trace =
        MockTrace.of_source source1
        |> MockTrace.add_sink sink1 in
      assert_bool
        "Appended trace should contain source and sink"
        (MockTrace.equal (MockTrace.append source_trace footprint_trace call_site) expected_trace);

      let appended_trace = MockTrace.append source_trace source_trace call_site in
      assert_bool
        "Appending a trace that doesn't add a new source/sink should add a passthrough"
        (MockTrace.Passthroughs.mem
           (Passthrough.make call_site) (MockTrace.passthroughs appended_trace)) in
    "append">::append_ in

  "trace_domain_suite">:::[get_reports; append]
