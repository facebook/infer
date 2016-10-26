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
      let pp_kind _ _ = assert false

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
        then Some site
        else None

      let is_footprint _ = assert false
      let make_footprint _ = assert false
      let get_footprint_access_path _ = assert false
    end

    module Sink = struct
      include MockTraceElem

      let get site _ =
        if string_is_prefix "SINK" (Procname.to_string (CallSite.pname site))
        then [Sink.make_sink_param site 0 ~report_reachable:false]
        else []
    end

    let should_report _ _ = false

    let get_reportable_exn _ _ _ = assert false
  end)

module MockTaintAnalysis = TaintAnalysis.Make(struct
    module Trace = MockTrace

    let of_summary_trace _ = assert false
    let to_summary_trace _ = assert false
    let handle_unknown_call _ _ _ = []
  end)

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
    make_call ~procname (Some (ident_of_str ret_str, dummy_typ)) [] in
  let assign_to_non_source ret_str =
    let procname = Procname.from_string_c_fun "NON-SOURCE" in
    make_call ~procname (Some (ident_of_str ret_str, dummy_typ)) [] in
  let call_sink_with_exp exp =
    let procname = Procname.from_string_c_fun "SINK" in
    make_call ~procname None [(exp, dummy_typ)] in
  let call_sink actual_str =
    call_sink_with_exp (Exp.Var (ident_of_str actual_str)) in
  let assign_id_to_field root_str fld_str rhs_id_str =
    let rhs_exp = Exp.Var (ident_of_str rhs_id_str) in
    make_store ~rhs_typ:Typ.Tvoid (Exp.Var (ident_of_str root_str)) fld_str ~rhs_exp in
  let read_field_to_id lhs_id_str root_str fld_str =
    make_load_fld ~rhs_typ:Typ.Tvoid lhs_id_str fld_str (Exp.Var (ident_of_str root_str)) in
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
    "source flows to var",
    [
      assign_to_source "ret_id";
      var_assign_id "var" "ret_id";
      invariant "{ ret_id$0 => (SOURCE -> ?), &var => (SOURCE -> ?) }";
    ];
    "source flows to field",
    [
      assign_to_source "ret_id";
      assign_id_to_field "base_id" "f" "ret_id";
      invariant "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?) }";
    ];
    "source flows to field then var",
    [
      assign_to_source "ret_id";
      assign_id_to_field "base_id" "f" "ret_id";
      read_field_to_id "read_id" "base_id" "f";
      var_assign_id "var" "read_id";
      invariant
        "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?), &var => (SOURCE -> ?) }";
    ];
    "source flows to var then cleared",
    [
      assign_to_source "ret_id";
      var_assign_id "var" "ret_id";
      invariant "{ ret_id$0 => (SOURCE -> ?), &var => (SOURCE -> ?) }";
      assign_to_non_source "non_source_id";
      var_assign_id "var" "non_source_id";
      invariant "{ ret_id$0 => (SOURCE -> ?) }";
    ];
    "source flows to field then cleared",
    [
      assign_to_source "ret_id";
      assign_id_to_field "base_id" "f" "ret_id";
      invariant "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?) }";
      assign_to_non_source "non_source_id";
      assign_id_to_field "base_id" "f" "non_source_id";
      invariant "{ ret_id$0 => (SOURCE -> ?) }";
    ];
    "var id alias test",
    [
      assign_to_non_source "ret_id";
      var_assign_id "var1" "ret_id";
      assign_to_source "source_id";
      assign_id_to_field "ret_id" "f" "source_id";
      invariant "{ source_id$0 => (SOURCE -> ?), &var1.f => (SOURCE -> ?) }";
      read_field_to_id "read_id" "ret_id" "f";
      invariant "{ source_id$0 => (SOURCE -> ?), &var1.f => (SOURCE -> ?) }";
      var_assign_id "var2" "read_id";
      invariant
        "{ source_id$0 => (SOURCE -> ?), &var1.f => (SOURCE -> ?), &var2 => (SOURCE -> ?) }";
    ];
    "field id alias test1",
    [
      assign_to_non_source "ret_id";
      assign_to_source "source_id";
      assign_id_to_field "ret_id" "g" "source_id";
      assign_to_non_source "var_id";
      var_assign_id "var" "var_id";
      assign_id_to_field "var_id" "f" "ret_id";
      invariant
        "{ ret_id$0.g => (SOURCE -> ?), source_id$0 => (SOURCE -> ?), &var.f.g => (SOURCE -> ?) }";
    ];
    "field id alias test2",
    [
      assign_to_non_source "ret_id";
      read_field_to_id "g_id" "ret_id" "g";
      var_assign_id "var1" "g_id";
      assign_to_source "source_id";
      invariant "{ source_id$0 => (SOURCE -> ?) }";
      assign_id_to_field "g_id" "f" "source_id";
      invariant "{ source_id$0 => (SOURCE -> ?), &var1.g.f => (SOURCE -> ?) }";
      id_assign_var "var_id" "var1";
      read_field_to_id "var_g_id" "var_id" "g";
      read_field_to_id "var_g_f_id" "var_g_id" "f";
      var_assign_id "var2" "var_g_f_id";
      invariant
        "{ source_id$0 => (SOURCE -> ?), &var1.g.f => (SOURCE -> ?), &var2 => (SOURCE -> ?) }";
    ];
    "sink without source not tracked",
    [
      assign_to_non_source "ret_id";
      call_sink "ret_id";
      assert_empty;
    ];
    "source -> sink direct",
    [
      assign_to_source "ret_id";
      call_sink "ret_id";
      invariant "{ ret_id$0 => (SOURCE -> SINK) }";
    ];
    "source -> sink via var",
    [
      assign_to_source "ret_id";
      var_assign_id "actual" "ret_id";
      call_sink_with_exp (var_of_str "actual");
      invariant "{ ret_id$0 => (SOURCE -> ?), &actual => (SOURCE -> SINK) }";
    ];
    "source -> sink via var then ident",
    [
      assign_to_source "ret_id";
      var_assign_id "x" "ret_id";
      id_assign_var "actual_id" "x";
      call_sink "actual_id";
      invariant "{ ret_id$0 => (SOURCE -> ?), &x => (SOURCE -> SINK) }";
    ];
    "source -> sink via field",
    [
      assign_to_source "ret_id";
      assign_id_to_field "base_id" "f" "ret_id";
      read_field_to_id "actual_id" "base_id" "f";
      call_sink "actual_id";
      invariant "{ base_id$0.f => (SOURCE -> SINK), ret_id$0 => (SOURCE -> ?) }";
    ];
    "source -> sink via field read from var",
    [
      assign_to_source "ret_id";
      assign_id_to_field "base_id" "f" "ret_id";
      var_assign_id "var" "base_id";
      id_assign_var "var_id" "var";
      read_field_to_id "read_id" "var_id" "f";
      call_sink "read_id";
      invariant
        "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?), &var.f => (SOURCE -> SINK) }";
    ];
    "source -> sink via cast",
    [
      assign_to_source "ret_id";
      cast_id_to_id "cast_id" Typ.Tvoid "ret_id";
      call_sink "cast_id";
      invariant "{ ret_id$0 => (SOURCE -> SINK) }";
    ];

  ] |> TestInterpreter.create_tests ~pp_opt:pp_sparse AccessPath.BaseMap.empty in
  "taint_test_suite">:::test_list
