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
      | _ -> assert false

    let make_nth_param_ap n pname ~propagate_all =
      let raw_ap =
        (* base of this access path is always ignored, so type/name don't matter *)
        AccessPath.of_pvar
          (Pvar.mk (Mangled.from_string ("fake_param" ^ string_of_int n)) pname) Typ.Tvoid in
      if propagate_all then AccessPath.Abstracted raw_ap else AccessPath.Exact raw_ap

    (* propagate the trace from the nth parameter of [site.pname] to the return value of
       [site.pname]. if [propagate_all] is true, all traces reachable from the parameter will
       be propagated as well (e.g., for foo(x), we'll also propagate the traces associated with x.f,
       x.f.g, and so on) *)
    let propagate_nth_to_return n site ret_typ ~propagate_all =
      let pname = CallSite.pname site in
      let nth_param_ap = make_nth_param_ap n pname ~propagate_all in
      let input = QuandarySummary.make_formal_input n nth_param_ap in
      let output =
        QuandarySummary.make_return_output
          (AccessPath.Exact (AccessPath.of_pvar (Pvar.get_ret_pvar pname) ret_typ)) in
      let footprint_source = Trace.Source.make_footprint nth_param_ap site in
      let footprint_trace = Trace.of_source footprint_source in
      QuandarySummary.make_in_out_summary input output (to_summary_trace footprint_trace)

    (* propagate the trace associated with each actual to the actual corresponding to the
       constructed object *)
    let propagate_to_constructor site actuals ~propagate_all =
      match actuals with
      | [] ->
          failwithf
            "Constructor %a has 0 actuals, which should never happen"
            Procname.pp (CallSite.pname site)
      | _ :: [] ->
          (* constructor has no actuals, nothing to propagate *)
          []
      | _ :: actuals ->
          let pname = CallSite.pname site in
          let constructor_ap = make_nth_param_ap 0 pname ~propagate_all in
          let output = QuandarySummary.make_formal_output 0 constructor_ap in
          let make_propagation_summary acc n _ =
            let n = n + 1 in (* skip the constructor actual *)
            let nth_param_ap = make_nth_param_ap n pname ~propagate_all in
            let input = QuandarySummary.make_formal_input n nth_param_ap in
            let footprint_source = Trace.Source.make_footprint nth_param_ap site in
            let footprint_trace = Trace.of_source footprint_source in
            let summary =
              QuandarySummary.make_in_out_summary input output (to_summary_trace footprint_trace) in
            summary :: acc in
          IList.fold_lefti make_propagation_summary [] actuals

    let propagate_actuals_to_return site ret_type actuals ~propagate_all =
      IList.mapi
        (fun actual_num _-> propagate_nth_to_return actual_num site ret_type ~propagate_all)
        actuals

    let handle_unknown_call site ret_typ_opt actuals =
      match CallSite.pname site with
      | (Procname.Java java_pname) as pname ->
          begin
            match Procname.java_get_class_name java_pname,
                  Procname.java_get_method java_pname,
                  ret_typ_opt with
            | _ when Procname.is_constructor pname ->
                propagate_to_constructor site actuals ~propagate_all:true
            | _, _, Some ret_typ ->
                propagate_actuals_to_return site ret_typ actuals ~propagate_all:true
            | _ ->
                []
          end
      | pname when Builtin.is_registered pname ->
          []
      | pname ->
          failwithf "Non-Java procname %a in Java analysis@." Procname.pp pname
  end)
