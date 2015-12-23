(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging

(* Automatically detect if a method is considered as performance critical
   by looking for the annotation in the super-types *)
let infer_performance_critical_methods = true


(* Warning name when a performance critical method directly or indirectly
   calls a method annotatd as expensive *)
let calls_expensive_method =
  "CHECKERS_CALLS_EXPENSIVE_METHOD"

(* Warning name for the subtyping rule: method not annotated as expensive cannot be overridden
   by a method annotated as expensive *)
let expensive_overrides_unexpensive =
  "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"

(* Warning name for the subtyping rule: methods overriding methods annotated as performance critical
   should also be annotated as performance critical *)
let unannotated_overrides_performance_critical =
  "CHECKERS_UNANNOTATED_OVERRIDES_PERFOMANCE_CRITICAL"

(* Triggers report on violation of the sybtyping rule for the performance critical annotation *)
let enforce_performance_critical_subtyping_rule = false


let check_attributes check attributes =
  let annotated_signature = Annotations.get_annotated_signature attributes in
  let ret_annotation, _ = annotated_signature.Annotations.ret in
  check ret_annotation


let is_performance_critical attributes =
  check_attributes Annotations.ia_is_performance_critical attributes


let is_expensive attributes =
  check_attributes Annotations.ia_is_expensive attributes


let check_method check pname =
  match Specs.proc_resolve_attributes pname with
  | None -> false
  | Some attributes ->
      check_attributes check attributes


let method_is_performance_critical pname =
  check_method Annotations.ia_is_performance_critical pname


let method_overrides_performance_critical tenv pname =
  let overrides () =
    let found = ref false in
    PatternMatch.proc_iter_overridden_methods
      (fun pn -> found := method_is_performance_critical pn)
      tenv pname;
    !found in
  method_is_performance_critical pname
  || overrides ()


let is_modeled_expensive tenv pname =
  if SymExec.function_is_builtin pname then false
  else if Procname.java_get_method pname <> "findViewById" then false
  else
    let package =
      match Procname.java_get_package pname with
      | None -> ""
      | Some p -> p in
    let classname =
      Mangled.from_package_class package (Procname.java_get_simple_class pname) in
    match Sil.tenv_lookup tenv (Typename.TN_csu (Csu.Class, classname)) with
    | None -> false
    | Some typ ->
        AndroidFramework.is_view typ tenv
        || AndroidFramework.is_activity typ tenv


let method_is_expensive tenv pname =
  is_modeled_expensive tenv pname
  || check_method Annotations.ia_is_expensive pname


let lookup_call_trees pname =
  match Specs.get_summary pname with
  | None -> []
  | Some summary ->
      begin
        match summary.Specs.payload.Specs.calls with
        | Some tree -> tree
        | None -> []
      end


let collect_expensive_call tenv caller_pdesc checked_pnames call_trees (pname, _) =
  if Procname.Set.mem pname !checked_pnames then call_trees
  else
    begin
      Ondemand.do_analysis caller_pdesc pname;
      checked_pnames := Procname.Set.add pname !checked_pnames;
      if method_is_expensive tenv pname then
        (CallTree.Direct pname) :: call_trees
      else
        match lookup_call_trees pname with
        | [] -> call_trees
        | calls -> (CallTree.Indirect (pname, calls)) :: call_trees
    end


let update_summary call_trees pname =
  match Specs.get_summary pname with
  | None -> ()
  | Some summary ->
      let updated_summary =
        { summary with
          Specs.payload =
            { summary.Specs.payload with
              Specs.calls = Some call_trees; }
        } in
      Specs.add_summary pname updated_summary


let report_expensive_calls pname pdesc loc call_trees =
  let string_of_pname = Procname.to_simplified_string ~withclass:true in
  let rec report_call_tree stack_str call_tree =
    match call_tree with
    | CallTree.Direct expensive_pname ->
        let exp_pname_str = string_of_pname expensive_pname in
        let description =
          Printf.sprintf
            "Method `%s` annotated with `@%s` calls `%s%s` where `%s` is annotated with `@%s`"
            (Procname.to_simplified_string pname)
            Annotations.performance_critical
            stack_str
            exp_pname_str
            exp_pname_str
            Annotations.expensive in
        Checkers.ST.report_error
          pname pdesc calls_expensive_method loc description
    | CallTree.Indirect (callee_pname, sub_trees) ->
        let callee_pname_str = string_of_pname callee_pname in
        let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
        IList.iter (report_call_tree new_stack_str) sub_trees in
  IList.iter (report_call_tree "") call_trees


let check_one_procedure tenv pname pdesc =

  let loc = Cfg.Procdesc.get_loc pdesc in
  let attributes = Cfg.Procdesc.get_attributes pdesc in
  let expensive = is_expensive attributes
  and performance_critical = is_performance_critical attributes in

  let check_expensive_subtyping_rules overridden_pname =
    if not (method_is_expensive tenv overridden_pname) then
      let description =
        Printf.sprintf
          "Method `%s` overrides unannotated method `%s` and cannot be annotated with `@%s`"
          (Procname.to_string pname)
          (Procname.to_string overridden_pname)
          Annotations.expensive in
      Checkers.ST.report_error
        pname pdesc expensive_overrides_unexpensive loc description

  and check_performance_critical_subtyping_rules overridden_pname =
    if method_is_performance_critical overridden_pname then
      let description =
        Printf.sprintf
          "Method `%s` overrides method `%s` annotated with `%s` and should also be annotated"
          (Procname.to_string pname)
          (Procname.to_string overridden_pname)
          Annotations.performance_critical in
      Checkers.ST.report_error
        pname pdesc unannotated_overrides_performance_critical loc description in

  if expensive then
    PatternMatch.proc_iter_overridden_methods
      check_expensive_subtyping_rules tenv pname;
  if enforce_performance_critical_subtyping_rule && not performance_critical then
    PatternMatch.proc_iter_overridden_methods
      check_performance_critical_subtyping_rules tenv pname;

  let expensive_call_trees =
    let checked_pnames = ref Procname.Set.empty in
    Cfg.Procdesc.fold_calls
      (collect_expensive_call tenv pdesc checked_pnames) [] pdesc in

  update_summary expensive_call_trees pname;

  match expensive_call_trees with
  | [] -> ()
  | call_trees when performance_critical ->
      report_expensive_calls pname pdesc loc call_trees
  | call_trees when infer_performance_critical_methods ->
      if method_overrides_performance_critical tenv pname then
        report_expensive_calls pname pdesc loc call_trees
  | _ -> ()


let callback_performance_checker _ get_proc_desc _ tenv pname pdesc =
  let callbacks =
    let analyze_ondemand pn =
      match get_proc_desc pn with
      | None -> ()
      | Some pd -> check_one_procedure tenv pn pd in
    { Ondemand.analyze_ondemand; get_proc_desc; } in
  if !Config.ondemand_enabled
     || Ondemand.procedure_should_be_analyzed pdesc pname
  then
    begin
      Ondemand.set_callbacks callbacks;
      check_one_procedure tenv pname pdesc;
      Ondemand.unset_callbacks ()
    end
