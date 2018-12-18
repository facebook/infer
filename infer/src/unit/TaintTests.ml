(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module MockTrace = Trace.Make (struct
  module MockTraceElem = struct
    include CallSite

    let matches ~caller ~callee = equal caller callee
  end

  module Source = Source.Make (struct
    include MockTraceElem

    let get ~caller_pname:_ pname _ _ =
      if String.is_prefix ~prefix:"SOURCE" (Typ.Procname.to_string pname) then
        [(CallSite.make pname Location.dummy, None)]
      else []


    let get_tainted_formals _ _ = []
  end)

  module Sink = Sink.Make (struct
    include MockTraceElem

    let get pname _ _ _ =
      if String.is_prefix ~prefix:"SINK" (Typ.Procname.to_string pname) then
        [(CallSite.make pname Location.dummy, IntSet.singleton 0)]
      else []
  end)

  module Sanitizer = Sanitizer.Dummy

  let get_report _ _ _ = None
end)

module MockTaintAnalysis = TaintAnalysis.Make (struct
  module Trace = MockTrace
  module AccessTree = AccessTree.Make (Trace) (AccessTree.DefaultConfig)

  let of_summary_access_tree _ = assert false

  let to_summary_access_tree _ = assert false

  let handle_unknown_call _ _ _ _ = []

  let is_taintable_type _ = true

  let get_model _ _ _ _ _ = None

  let name = ""
end)

module TestInterpreter =
  AnalyzerTester.Make
    (LowerHil.Make (MockTaintAnalysis.TransferFunctions (ProcCfg.Normal)) (LowerHil.DefaultConfig))

let tests =
  let open OUnit2 in
  let open AnalyzerTester.StructuredSil in
  (* less verbose form of pretty-printing to make writing tests easy *)
  let pp_sparse fmt astate =
    let pp_call_site fmt call_site = Typ.Procname.pp fmt (CallSite.pname call_site) in
    let pp_sources fmt sources =
      if MockTrace.Sources.is_empty sources then F.pp_print_char fmt '?'
      else
        MockTrace.Sources.Known.iter
          (fun source -> pp_call_site fmt (MockTrace.Source.call_site source))
          sources.MockTrace.Sources.known
    in
    let pp_sinks fmt sinks =
      if MockTrace.Sinks.is_empty sinks then F.pp_print_char fmt '?'
      else
        MockTrace.Sinks.iter (fun sink -> pp_call_site fmt (MockTrace.Sink.call_site sink)) sinks
    in
    (* just print source -> sink, no line nums or passthroughs *)
    let pp_trace fmt trace =
      F.fprintf fmt "(%a -> %a)" pp_sources (MockTrace.sources trace) pp_sinks
        (MockTrace.sinks trace)
    in
    let pp_item fmt (ap, trace) = F.fprintf fmt "%a => %a" AccessPath.Abs.pp ap pp_trace trace in
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
    make_call ~procname ~return:(ident_of_str ret_str, dummy_typ) []
  in
  let assign_to_non_source ret_str =
    let procname = Typ.Procname.from_string_c_fun "NON-SOURCE" in
    make_call ~procname ~return:(ident_of_str ret_str, dummy_typ) []
  in
  let call_sink_with_exp exp =
    let procname = Typ.Procname.from_string_c_fun "SINK" in
    make_call ~procname [(exp, dummy_typ)]
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
  let callbacks = {Ondemand.exe_env= Exe_env.mk (); analyze_ondemand} in
  Ondemand.set_callbacks callbacks ;
  let test_list =
    [ ("source recorded", [assign_to_source "ret_id"; invariant "{ ret_id$0* => (SOURCE -> ?) }"])
    ; ("non-source not recorded", [assign_to_non_source "ret_id"; assert_empty])
    ; ( "source flows to var"
      , [ assign_to_source "ret_id"
        ; var_assign_id "var" "ret_id"
        ; invariant "{ ret_id$0* => (SOURCE -> ?), var* => (SOURCE -> ?) }" ] )
    ; ( "source flows to field"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; invariant "{ base_id$0.f* => (SOURCE -> ?), ret_id$0* => (SOURCE -> ?) }" ] )
    ; ( "source flows to field then var"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; read_field_to_id "read_id" "base_id" "f"
        ; var_assign_id "var" "read_id"
        ; invariant
            "{ base_id$0.f* => (SOURCE -> ?),\n\
            \  ret_id$0* => (SOURCE -> ?),\n\
            \  var* => (SOURCE -> ?) }" ] )
    ; ( "source flows to var then cleared"
      , [ assign_to_source "ret_id"
        ; var_assign_id "var" "ret_id"
        ; invariant "{ ret_id$0* => (SOURCE -> ?), var* => (SOURCE -> ?) }"
        ; assign_to_non_source "non_source_id"
        ; var_assign_id "var" "non_source_id"
        ; invariant "{ ret_id$0* => (SOURCE -> ?) }" ] )
    ; ( "source flows to field then cleared"
      , [ assign_to_source "ret_id"
        ; assign_id_to_field "base_id" "f" "ret_id"
        ; invariant "{ base_id$0.f* => (SOURCE -> ?), ret_id$0* => (SOURCE -> ?) }"
        ; assign_to_non_source "non_source_id"
        ; assign_id_to_field "base_id" "f" "non_source_id"
        ; invariant "{ ret_id$0* => (SOURCE -> ?) }" ] )
    ; ( "sink without source not tracked"
      , [assign_to_non_source "ret_id"; call_sink "ret_id"; assert_empty] ) ]
    |> TestInterpreter.create_tests ~pp_opt:pp_sparse
         {formal_map= FormalMap.empty; summary= Summary.dummy}
         ~initial:(MockTaintAnalysis.Domain.empty, Bindings.empty)
  in
  "taint_test_suite" >::: test_list
