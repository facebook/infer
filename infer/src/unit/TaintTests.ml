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

module MockTrace = Trace.Make(struct
    module MockTraceElem = struct
      type kind = unit
      type t = CallSite.t
      let call_site t = t
      let kind _ = ()
      let make _ site = site
      let compare = CallSite.compare
      let equal = CallSite.equal
      let pp = CallSite.pp

      let to_callee t _ = t

      module Set = PrettyPrintable.MakePPSet(struct
          type nonrec t = t
          let compare = compare
          let pp_element = pp
        end)

    end

    module Source = struct
      include MockTraceElem

      let get site =
        if string_is_prefix "SOURCE" (Procname.to_string (CallSite.pname site))
        then [(0, site)]
        else []

      let is_footprint _ = assert false
      let make_footprint _ = assert false
      let get_footprint_access_path _ = assert false
      let to_return _ _ = assert false
    end

    module Sink = struct
      include MockTraceElem

      let get site =
        if string_is_prefix "SINK" (Procname.to_string (CallSite.pname site))
        then [(0, site)]
        else []
    end

    let should_report _ _ = true
  end)

module MockTaintAnalysis = TaintAnalysis.Make(MockTrace)

module TestInterpreter = AnalyzerTester.Make
    (ProcCfg.Normal)
    (Scheduler.ReversePostorder)
    (MockTaintAnalysis.TransferFunctions)

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  (* less verbose form of pretty-printing to make writing tests easy *)
  let pp_sparse fmt astate =
    let pp_call_site fmt call_site =
      F.fprintf fmt "%a" Procname.pp (CallSite.pname call_site) in
    let pp_sources fmt sources =
      if MockTrace.Sources.is_empty sources
      then F.fprintf fmt "?"
      else
        MockTrace.Sources.iter
          (fun source -> pp_call_site fmt (MockTrace.Source.call_site source))
          sources in
    let pp_sinks fmt sinks =
      if MockTrace.Sinks.is_empty sinks
      then F.fprintf fmt "?"
      else
        MockTrace.Sinks.iter
          (fun sink ->
             pp_call_site fmt (MockTrace.Sink.call_site sink))
          sinks in
    (* just print source -> sink, no line nums or passthroughs *)
    let pp_trace fmt trace =
      F.fprintf
        fmt
        "(%a -> %a)"
        pp_sources (MockTrace.sources trace)
        pp_sinks (MockTrace.sinks trace) in
    let pp_item fmt (ap, trace) =
      F.fprintf fmt "%a => %a" AccessPath.pp ap pp_trace trace in
    (* flatten access tree into list of access paths with associated traces *)
    let trace_assocs =
      MockTaintAnalysis.TaintDomain.fold
        (fun acc ap t ->
           if not (MockTrace.is_empty t)
           then (ap, t) :: acc
           else acc)
        astate.MockTaintAnalysis.Domain.access_tree
        [] in
    PrettyPrintable.pp_collection ~pp_item fmt (IList.rev trace_assocs) in
  let assign_to_source ret_str =
    let procname = Procname.from_string_c_fun "SOURCE" in
    make_call ~procname [ident_of_str ret_str] [] in
  let assign_to_non_source ret_str =
    let procname = Procname.from_string_c_fun "NON-SOURCE" in
    make_call ~procname [ident_of_str ret_str] [] in
  let assert_empty = invariant "{  }" in
  let test_list = [
    "source recorded",
    [
      assign_to_source "ret_id";
      invariant "{ ret_id$0 => (SOURCE -> ?) }";
    ];
    "non-source not recorded",
    [
      assign_to_non_source "ret_id";
      assert_empty;
    ];
  ] |> TestInterpreter.create_tests ~pp_opt:pp_sparse [] in
  "taint_test_suite">:::test_list
