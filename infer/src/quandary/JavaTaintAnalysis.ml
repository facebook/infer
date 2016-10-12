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

include
  TaintAnalysis.Make(struct
    module Trace = JavaTrace

    let to_summary_trace trace = QuandarySummary.Java trace

    let of_summary_trace = function
      | QuandarySummary.Java trace -> trace
      | QuandarySummary.Unknown -> assert false

    (* propagate the trace from the nth parameter of [site.pname] to the return value of
       [site.pname]. if [propagate_reachable] is true, all traces reachable from the parameter will
       be propagated as well (e.g., for foo(x), we'll also propagate the traces associated with x.f,
       x.f.g, and so on) *)
    let propagate_nth_to_return n site ret_typ ~propagate_all =
      let pname = CallSite.pname site in
      let dummy_param_ap =
        let raw_ap =
          (* base of this access path is always ignored, so type/name don't matter *)
          AccessPath.of_pvar (Pvar.mk (Mangled.from_string "fake_param") pname) Typ.Tvoid in
        if propagate_all then AccessPath.Abstracted raw_ap else AccessPath.Exact raw_ap in
      let input = QuandarySummary.make_formal_input n dummy_param_ap in
      let output =
        QuandarySummary.make_return_output
          (AccessPath.Exact (AccessPath.of_pvar (Pvar.get_ret_pvar pname) ret_typ)) in
      let footprint_source = Trace.Source.make_footprint dummy_param_ap site in
      let footprint_trace = Trace.of_source footprint_source in
      QuandarySummary.make_in_out_summary input output (to_summary_trace footprint_trace)

    let handle_unknown_call site ret_typ_opt =
      match CallSite.pname site with
      | Procname.Java pname ->
          begin
            match Procname.java_get_class_name pname,
                  Procname.java_get_method pname,
                  ret_typ_opt with
            | "java.lang.String", "valueOf", Some ret_typ ->
                [propagate_nth_to_return 0 site ret_typ ~propagate_all:true]
            | _ ->
                []
          end
      | pname when Builtin.is_registered pname ->
          []
      | pname ->
          failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname
  end)
