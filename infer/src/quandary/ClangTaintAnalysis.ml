(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include TaintAnalysis.Make (struct
  module Trace = ClangTrace
  module AccessTree = AccessTree.Make (Trace) (AccessTree.DefaultConfig)

  let to_summary_access_tree tree = QuandarySummary.AccessTree.Clang tree

  let of_summary_access_tree = function
    | QuandarySummary.AccessTree.Clang tree ->
        tree
    | _ ->
        assert false


  let handle_unknown_call pname ret_typ actuals _ =
    let handle_generic_unknown ret_typ actuals =
      match ((ret_typ.Typ.desc : Typ.desc), List.rev_map actuals ~f:HilExp.ignore_cast) with
      (* everything but Tvoid*)
      | (Tint _ | Tfloat _ | Tfun _ | Tptr (_, _) | Tstruct _ | TVar _ | Tarray _), _ ->
          (* propagate taint from actuals to return value *)
          [TaintSpec.Propagate_to_return]
      | Tvoid, [] ->
          []
      | Tvoid, _ when Typ.Procname.is_constructor pname ->
          (* "this" is always the first arg of a constructor; propagate taint there *)
          [TaintSpec.Propagate_to_receiver]
      | Tvoid, HilExp.AccessExpression access_expr :: _ -> (
        match HilExp.AccessExpression.to_access_path access_expr with
        | (Var.ProgramVar pvar, {desc= Typ.Tptr (_, Typ.Pk_pointer)}), []
          when Pvar.is_frontend_tmp pvar ->
            (* no return value, but the frontend has introduced a dummy return variable and will
               assign the return value to this variable. So propagate taint to the dummy return
               variable *)
            let actual_index = List.length actuals - 1 in
            [TaintSpec.Propagate_to_actual actual_index]
        | _ ->
            [TaintSpec.Propagate_to_receiver] )
      | Tvoid, _ ->
          (* no return value; propagate taint from actuals to receiver *)
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
    | "operator|=" ->
        [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
    | "memcpy" | "memmove" | "strcpy" | "strncpy" ->
        [TaintSpec.Propagate_to_receiver; TaintSpec.Propagate_to_return]
    | "sprintf" ->
        [TaintSpec.Propagate_to_receiver]
    | "strlen" ->
        (* don't propagate taint for strlen *)
        []
    | _ ->
        handle_generic_unknown ret_typ actuals


  (* treat folly functions as unknown library code. we often specify folly functions as sinks,
       and we don't want to double-report if these functions eventually call other sinks (e.g.,
       when folly::Subprocess calls exec), in addition some folly functions are heavily optimized in
       a way that obscures what they're actually doing (e.g., they use assembly code). it's better
       to write models for these functions or treat them as unknown *)
  let models_matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["folly"]

  let get_model pname ret_typ actuals tenv summary =
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
                (Typ.Procname.get_qualifiers pname) ->
        Some (handle_unknown_call pname ret_typ actuals tenv)
    | _ ->
        None


  let is_taintable_type _ = true

  let name = "clang"
end)
