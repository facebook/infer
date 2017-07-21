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

module MockTrace = Trace.Make (struct
  module MockTraceElem = CallSite

  module Source = Source.Make (struct
    include MockTraceElem

    let unknown = CallSite.dummy

    let get pname _ _ =
      if String.is_prefix ~prefix:"SOURCE" (Typ.Procname.to_string pname) then
        Some (CallSite.make pname Location.dummy, None)
      else None

    let get_tainted_formals _ _ = []
  end)

  module Sink = Sink.Make (struct
    include MockTraceElem

    let get pname _ _ =
      if String.is_prefix ~prefix:"SINK" (Typ.Procname.to_string pname) then
        Some (CallSite.make pname Location.dummy, IntSet.singleton 0)
      else None

    let indexes _ = IntSet.empty
  end)

  let should_report _ _ = false
end)

module MockTaintAnalysis = TaintAnalysis.Make (struct
  module Trace = MockTrace
  module AccessTree = AccessTree.Make (Trace)

  let of_summary_access_tree _ = assert false

  let to_summary_access_tree _ = assert false

  let handle_unknown_call _ _ _ _ = []

  let is_taintable_type _ = true

  let get_model _ _ _ _ _ = None

  let get_sanitizer _ = None
end)

module TestInterpreter =
  AnalyzerTester.Make (ProcCfg.Normal) (LowerHil.Make (MockTaintAnalysis.TransferFunctions))

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  (* less verbose form of pretty-printing to make writing tests easy *)
  let pp_sparse fmt astate =
    let pp_call_site fmt call_site =
      F.fprintf fmt "%a" Typ.Procname.pp (CallSite.pname call_site)
    in
    let pp_sources fmt sources =
      if MockTrace.Sources.is_empty sources then F.fprintf fmt "?"
      else
        MockTrace.Sources.iter
          (fun source -> pp_call_site fmt (MockTrace.Source.call_site source))
          sources
    in
    let pp_sinks fmt sinks =
      if MockTrace.Sinks.is_empty sinks then F.fprintf fmt "?"
      else
        MockTrace.Sinks.iter (fun sink -> pp_call_site fmt (MockTrace.Sink.call_site sink)) sinks
    in
    (* just print source -> sink, no line nums or passthroughs *)
    let pp_trace fmt trace =
      F.fprintf fmt "(%a -> %a)" pp_sources (MockTrace.sources trace) pp_sinks
        (MockTrace.sinks trace)
    in
    let pp_item fmt (ap, trace) = F.fprintf fmt "%a => %a" AccessPath.pp ap pp_trace trace in
    (* flatten access tree into list of access paths with associated traces *)
    let trace_assocs =
      MockTaintAnalysis.TaintDomain.trace_fold
        (fun acc ap t -> if not (MockTrace.is_empty t) then (ap, t) :: acc else acc)
        (fst astate) []
    in
    PrettyPrintable.pp_collection ~pp_item fmt (List.rev trace_assocs)
  in
  let assign_to_source ret_str =
    let procname = Typ.Procname.from_string_c_fun "SOURCE" in
    make_call ~procname (Some (ident_of_str ret_str, dummy_typ)) []
  in
  let assign_to_non_source ret_str =
    let procname = Typ.Procname.from_string_c_fun "NON-SOURCE" in
    make_call ~procname (Some (ident_of_str ret_str, dummy_typ)) []
  in
  let call_sink_with_exp exp =
    let procname = Typ.Procname.from_string_c_fun "SINK" in
    make_call ~procname None [(exp, dummy_typ)]
  in
  let call_sink actual_str = call_sink_with_exp (Exp.Var (ident_of_str actual_str)) in
  let assign_id_to_field root_str fld_str rhs_id_str =
    let rhs_exp = Exp.Var (ident_of_str rhs_id_str) in
    make_store ~rhs_typ:(Typ.mk Tvoid) (Exp.Var (ident_of_str root_str)) fld_str ~rhs_exp
  in
  let read_field_to_id lhs_id_str root_str fld_str =
    make_load_fld ~rhs_typ:(Typ.mk Tvoid) lhs_id_str fld_str (Exp.Var (ident_of_str root_str))
  in
  let assert_empty = invariant "{ }" in
  (* hack: register an empty analyze_ondemand to prevent a crash because the callback is unset *)
  let analyze_ondemand summary _ = summary in
  let get_proc_desc _ = None in
  let callbacks = {Ondemand.analyze_ondemand= analyze_ondemand; get_proc_desc} in
  Ondemand.set_callbacks callbacks ;
  let test_list =
    [ ("source recorded", [assign_to_source "ret_id"; invariant "{ ret_id$0 => (SOURCE -> ?) }"])
    ; ("non-source not recorded", [assign_to_non_source "ret_id"; assert_empty])
    ; ( "source flows to var"
      , [ assign_to_source "ret_id"
        ; var_assign_id "var" "ret_id"
        ; invariant "{ ret_id$0 => (SOURCE -> ?), &var => (SOURCE -> ?) }" ] )
    ; ( "source flows to field"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; invariant "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?) }" ] )
    ; ( "source flows to field then var"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; read_field_to_id "read_id" "base_id" "f"
        ; var_assign_id "var" "read_id"
        ; invariant
            "{ base_id$0.f => (SOURCE -> ?),\n  ret_id$0 => (SOURCE -> ?),\n  &var => (SOURCE -> ?) }"
        ] )
    ; ( "source flows to var then cleared"
      , [ assign_to_source "ret_id"
        ; var_assign_id "var" "ret_id"
        ; invariant "{ ret_id$0 => (SOURCE -> ?), &var => (SOURCE -> ?) }"
        ; assign_to_non_source "non_source_id"
        ; var_assign_id "var" "non_source_id"
        ; invariant "{ ret_id$0 => (SOURCE -> ?) }" ] )
    ; ( "source flows to field then cleared"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; invariant "{ base_id$0.f => (SOURCE -> ?), ret_id$0 => (SOURCE -> ?) }"
        ; assign_to_non_source "non_source_id"
        ; assign_id_to_field "base_id" "f" "non_source_id"
        ; invariant "{ ret_id$0 => (SOURCE -> ?) }" ] )
    ; ( "sink without source not tracked"
      , [assign_to_non_source "ret_id"; call_sink "ret_id"; assert_empty] )
    ; ( "source -> sink direct"
      , [ assign_to_source "ret_id"
        ; call_sink "ret_id"
        ; invariant "{ ret_id$0* => (SOURCE -> SINK) }" ] )
    ; ( "source -> sink via var"
      , [ assign_to_source "ret_id"
        ; var_assign_id "actual" "ret_id"
        ; call_sink_with_exp (var_of_str "actual")
        ; invariant "{ ret_id$0 => (SOURCE -> ?), &actual* => (SOURCE -> SINK) }" ] )
    ; ( "source -> sink via var then ident"
      , [ assign_to_source "ret_id"
        ; var_assign_id "x" "ret_id"
        ; id_assign_var "actual_id" "x"
        ; call_sink "actual_id"
        ; invariant "{ ret_id$0 => (SOURCE -> ?), &x* => (SOURCE -> SINK) }" ] )
    ; ( "source -> sink via field"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; read_field_to_id "actual_id" "base_id" "f"
        ; call_sink "actual_id"
        ; invariant "{ base_id$0.f* => (SOURCE -> SINK), ret_id$0 => (SOURCE -> ?) }" ] )
    ; ( "source -> sink via field read from var"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; var_assign_id "var" "base_id"
        ; id_assign_var "var_id" "var"
        ; read_field_to_id "read_id" "var_id" "f"
        ; call_sink "read_id"
        ; invariant
            "{ base_id$0.f => (SOURCE -> ?),\n  ret_id$0 => (SOURCE -> ?),\n  &var.f* => (SOURCE -> SINK) }"
        ] )
    ; ( "source -> sink via cast"
      , [ assign_to_source "ret_id"
        ; cast_id_to_id "cast_id" (Typ.mk Tvoid) "ret_id"
        ; call_sink "cast_id"
        ; invariant "{ ret_id$0* => (SOURCE -> SINK) }" ] ) ]
    |> TestInterpreter.create_tests ~pp_opt:pp_sparse
         {formal_map= FormalMap.empty; summary= Specs.dummy}
         ~initial:(MockTaintAnalysis.Domain.empty, IdAccessPathMapDomain.empty)
  in
  "taint_test_suite" >::: test_list
