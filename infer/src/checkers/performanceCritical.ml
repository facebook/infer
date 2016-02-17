(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging

(* Warning name when a performance critical method directly or indirectly
   calls a method annotatd as expensive *)
let calls_expensive_method =
  "CHECKERS_CALLS_EXPENSIVE_METHOD"

(* Warning name for the subtyping rule: method not annotated as expensive cannot be overridden
   by a method annotated as expensive *)
let expensive_overrides_unexpensive =
  "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"


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
    match Sil.tenv_lookup tenv (Typename.TN_csu (Csu.Class Csu.Java, classname)) with
    | None -> false
    | Some typ ->
        AndroidFramework.is_view typ tenv
        || AndroidFramework.is_activity typ tenv


let method_is_expensive tenv pname =
  is_modeled_expensive tenv pname
  || check_method Annotations.ia_is_expensive pname


let lookup_expensive_calls pname =
  match Specs.get_summary pname with
  | None -> []
  | Some summary ->
      begin
        match summary.Specs.payload.Specs.calls with
        | Some calls -> calls
        | None -> []
      end


let method_calls_expensive tenv pname =
  method_is_expensive tenv pname
  || lookup_expensive_calls pname <> []


let lookup_location pname =
  match Specs.get_summary pname with
  | None -> Location.dummy
  | Some summary -> summary.Specs.attributes.ProcAttributes.loc


let collect_expensive_call tenv caller_pdesc checked_pnames call_list (pname, _) =
  if Procname.Set.mem pname !checked_pnames then call_list
  else
    begin
      Ondemand.do_analysis caller_pdesc pname;
      checked_pnames := Procname.Set.add pname !checked_pnames;
      let call_loc = lookup_location pname in
      if method_calls_expensive tenv pname then
        (pname, call_loc) :: call_list
      else
        call_list
    end

let update_summary calls pname =
  match Specs.get_summary pname with
  | None -> ()
  | Some summary ->
      let updated_summary =
        { summary with
          Specs.payload =
            { summary.Specs.payload with
              Specs.calls = Some calls; }
        } in
      Specs.add_summary pname updated_summary


let report_expensive_calls tenv pname pdesc loc calls =
  let string_of_pname = Procname.to_simplified_string ~withclass:true in
  let update_trace trace loc =
    if Location.equal loc Location.dummy then trace
    else
      let trace_elem = {
        Errlog.lt_level = 0;
        lt_loc = loc;
        lt_description = "";
        lt_node_tags = [];
      } in
      trace_elem :: trace in
  let rec report_expensive_call visited_pnames (trace, stack_str) (callee_pname, callee_loc) =
    if method_is_expensive tenv callee_pname then
      let final_trace = IList.rev (update_trace trace callee_loc) in
      let exp_pname_str = string_of_pname callee_pname in
      let description =
        Printf.sprintf
          "Method `%s` annotated with `@%s` calls `%s%s` where `%s` is annotated with `@%s`"
          (Procname.to_simplified_string pname)
          Annotations.performance_critical
          stack_str
          exp_pname_str
          exp_pname_str
          Annotations.expensive in
      let exn =
        Exceptions.Checkers (calls_expensive_method, Localise.verbatim_desc description) in
      Reporting.log_error pname ~loc: (Some loc) ~ltr: (Some final_trace) exn
    else
      let next_calls = lookup_expensive_calls callee_pname in
      let callee_pname_str = string_of_pname callee_pname in
      let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
      let new_trace = update_trace trace callee_loc in
      let unseen_pnames, updated_visited =
        IList.fold_left
          (fun (accu, set) (p, loc) ->
             if Procname.Set.mem p set then (accu, set)
             else ((p, loc) :: accu, Procname.Set.add p set))
          ([], visited_pnames) next_calls in
      IList.iter (report_expensive_call updated_visited (new_trace, new_stack_str)) unseen_pnames in
  let start_trace = update_trace [] (Cfg.Procdesc.get_loc pdesc) in
  IList.iter (report_expensive_call Procname.Set.empty (start_trace, "")) calls


let check_one_procedure tenv pname pdesc =

  let loc = Cfg.Procdesc.get_loc pdesc in
  let attributes = Cfg.Procdesc.get_attributes pdesc in
  let expensive = is_expensive attributes
  and performance_critical =
    method_overrides_performance_critical tenv pname in

  let check_expensive_subtyping_rules overridden_pname =
    if not (method_is_expensive tenv overridden_pname) then
      let description =
        Printf.sprintf
          "Method `%s` overrides unannotated method `%s` and cannot be annotated with `@%s`"
          (Procname.to_string pname)
          (Procname.to_string overridden_pname)
          Annotations.expensive in
      let exn =
        Exceptions.Checkers
          (expensive_overrides_unexpensive, Localise.verbatim_desc description) in
      Reporting.log_error pname ~loc: (Some loc) ~ltr: None exn in

  if expensive then
    PatternMatch.proc_iter_overridden_methods
      check_expensive_subtyping_rules tenv pname;

  let expensive_calls =
    let checked_pnames = ref Procname.Set.empty in
    Cfg.Procdesc.fold_calls
      (collect_expensive_call tenv pdesc checked_pnames) [] pdesc in

  update_summary expensive_calls pname;

  if performance_critical then
    report_expensive_calls tenv pname pdesc loc expensive_calls


let callback_performance_checker { Callbacks.proc_desc; proc_name; get_proc_desc; tenv } =
  let callbacks =
    let analyze_ondemand pn =
      match get_proc_desc pn with
      | None -> ()
      | Some pd -> check_one_procedure tenv pn pd in
    { Ondemand.analyze_ondemand; get_proc_desc; } in
  if !Config.ondemand_enabled
  || Ondemand.procedure_should_be_analyzed proc_desc proc_name
  then
    begin
      Ondemand.set_callbacks callbacks;
      check_one_procedure tenv proc_name proc_desc;
      Ondemand.unset_callbacks ()
    end
