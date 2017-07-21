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
module L = Logging

include TaintAnalysis.Make (struct
  module Trace = ClangTrace
  module AccessTree = AccessTree.Make (Trace)

  let to_summary_access_tree tree = QuandarySummary.AccessTree.Clang tree

  let of_summary_access_tree = function
    | QuandarySummary.AccessTree.Clang tree
     -> tree
    | _
     -> assert false

  let handle_unknown_call pname ret_typ_opt actuals _ =
    let handle_generic_unknown ret_typ_opt actuals =
      match (ret_typ_opt, List.rev actuals) with
      | Some _, _
       -> (* propagate taint from actuals to return value *)
          [TaintSpec.Propagate_to_return]
      | None, []
       -> []
      | None, _ when Typ.Procname.is_constructor pname
       -> (* "this" is always the first arg of a constructor; propagate taint there *)
          [TaintSpec.Propagate_to_receiver]
      | ( None
        , (HilExp.AccessPath ((Var.ProgramVar pvar, {desc= Typ.Tptr (_, Typ.Pk_pointer)}), []))
          :: _ )
        when Pvar.is_frontend_tmp pvar
       -> (* no return value, but the frontend has introduced a dummy return variable and will
               assign the return value to this variable. So propagate taint to the dummy return
               variable *)
          let actual_index = List.length actuals - 1 in
          [TaintSpec.Propagate_to_actual actual_index]
      | None, _
       -> (* no return value; propagate taint from actuals to receiver *)
          [TaintSpec.Propagate_to_receiver]
    in
    (* if we have a specific model for a procedure, use that. otherwise, use the generic
         heuristics for dealing with unknown code *)
    match Typ.Procname.get_method pname with
    | "operator+="
    | "operator-="
    | "operator*="
    | "operator/="
    | "operator%="
    | "operator<<="
    | "operator>>="
    | "operator&="
    | "operator^="
    | "operator|="
     -> [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
    | "memcpy" | "memmove" | "strcpy" | "strncpy"
     -> [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
    | "sprintf"
     -> [TaintSpec.Propagate_to_receiver]
    | _
     -> handle_generic_unknown ret_typ_opt actuals

  (* treat folly functions as unknown library code. we often specify folly functions as sinks,
       and we don't want to double-report if these functions eventually call other sinks (e.g.,
       when folly::Subprocess calls exec), in addition some folly functions are heavily optimized in
       a way that obscures what they're actually doing (e.g., they use assembly code). it's better
       to write models for these functions or treat them as unknown *)
  let models_matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["folly"]

  let get_model pname ret_typ_opt actuals tenv summary =
    (* hack for default C++ constructors, which get translated as an empty body (and will thus
         have an empty summary). We don't want that because we want to be able to propagate taint
         from comstructor parameters to the constructed object. so we treat the empty constructor
         as a skip function instead *)
    let is_default_constructor pname =
      Typ.Procname.is_c_method pname && Typ.Procname.is_constructor pname
      && AccessTree.BaseMap.is_empty summary
    in
    match pname with
    | Typ.Procname.ObjC_Cpp _
      when is_default_constructor pname
           || QualifiedCppName.Match.match_qualifiers models_matcher
                (Typ.Procname.get_qualifiers pname)
     -> Some (handle_unknown_call pname ret_typ_opt actuals tenv)
    | _
     -> None

  let external_sanitizers =
    List.map
      ~f:(fun {QuandaryConfig.Sanitizer.procedure} ->
        QualifiedCppName.Match.of_fuzzy_qual_names [procedure])
      (QuandaryConfig.Sanitizer.of_json Config.quandary_sanitizers)

  let get_sanitizer pname =
    let qualified_pname = Typ.Procname.get_qualifiers pname in
    List.find_map
      ~f:(fun qualifiers ->
        if QualifiedCppName.Match.match_qualifiers qualifiers qualified_pname then
          Some TaintSpec.Return
        else None)
      external_sanitizers

  let is_taintable_type _ = true
end)
