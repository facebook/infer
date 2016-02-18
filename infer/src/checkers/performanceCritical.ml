(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module L = Logging


let performance_critical_implies_no_allocation = true


(* Warning name when a performance critical method directly or indirectly
   calls a method annotatd as expensive *)
let calls_expensive_method =
  "CHECKERS_CALLS_EXPENSIVE_METHOD"

(* Warning name when a performance critical method directly or indirectly
   calls a method allocating memory *)
let allocates_memory =
  "CHECKERS_ALLOCATES_MEMORY"

(* Warning name for the subtyping rule: method not annotated as expensive cannot be overridden
   by a method annotated as expensive *)
let expensive_overrides_unexpensive =
  "CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED"


let check_attributes check attributes =
  let annotated_signature = Annotations.get_annotated_signature attributes in
  let ret_annotation, _ = annotated_signature.Annotations.ret in
  check ret_annotation


let is_expensive attributes =
  check_attributes Annotations.ia_is_expensive attributes


let check_method check pname =
  match Specs.proc_resolve_attributes pname with
  | None -> false
  | Some attributes ->
      check_attributes check attributes


let method_is_performance_critical pname =
  check_method Annotations.ia_is_performance_critical pname


let method_is_no_allcation pname =
  (performance_critical_implies_no_allocation
   && method_is_performance_critical pname)
  || check_method Annotations.ia_is_no_allocation pname


let method_overrides is_annotated tenv pname =
  let overrides () =
    let found = ref false in
    PatternMatch.proc_iter_overridden_methods
      (fun pn -> found := is_annotated pn)
      tenv pname;
    !found in
  is_annotated pname
  || overrides ()


let method_overrides_performance_critical tenv pname =
  method_overrides method_is_performance_critical tenv pname


let method_overrides_no_allocation tenv pname =
  method_overrides method_is_no_allcation tenv pname


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


let lookup_call_summary pname =
  match Specs.get_summary pname with
  | None -> None
  | Some summary -> summary.Specs.payload.Specs.calls


let lookup_expensive_calls pname =
  match lookup_call_summary pname with
  | None -> []
  | Some { Specs.expensive_calls } -> expensive_calls


let lookup_allocations pname =
  match lookup_call_summary pname with
  | None -> []
  | Some { Specs.allocations } -> allocations


let method_calls_expensive tenv pname =
  let calls_expensive () =
    match lookup_call_summary pname with
    | Some { Specs.expensive_calls } ->
        expensive_calls <> []
    | None -> false in
  method_is_expensive tenv pname
  || calls_expensive ()


let method_allocates pname =
  let allocates () =
    match lookup_call_summary pname with
    | Some { Specs.allocations } ->
        allocations <> []
    | None -> false in
  Procname.is_constructor pname
  || allocates ()


let lookup_location pname =
  match Specs.get_summary pname with
  | None -> Location.dummy
  | Some summary -> summary.Specs.attributes.ProcAttributes.loc


let collect_calls tenv caller_pdesc checked_pnames call_summary (pname, _) =
  if Procname.Set.mem pname !checked_pnames then call_summary
  else
    begin
      Ondemand.do_analysis caller_pdesc pname;
      checked_pnames := Procname.Set.add pname !checked_pnames;
      let call_loc = lookup_location pname in
      let updated_expensive_calls =
        if method_calls_expensive tenv pname then
          (pname, call_loc) :: call_summary.Specs.expensive_calls
        else
          call_summary.Specs.expensive_calls in
      let updated_allocations =
        if method_allocates pname then
          (pname, call_loc) :: call_summary.Specs.allocations
        else
          call_summary.Specs.allocations in
      { Specs.expensive_calls = updated_expensive_calls;
        Specs.allocations = updated_allocations }
    end


let update_summary call_summary pname =
  match Specs.get_summary pname with
  | None -> ()
  | Some summary ->
      let updated_summary =
        { summary with
          Specs.payload =
            { summary.Specs.payload with
              Specs.calls = Some call_summary }
        } in
      Specs.add_summary pname updated_summary


let string_of_pname =
  Procname.to_simplified_string ~withclass:true

let update_trace trace loc =
  if Location.equal loc Location.dummy then trace
  else
    let trace_elem = {
      Errlog.lt_level = 0;
      lt_loc = loc;
      lt_description = "";
      lt_node_tags = [];
    } in
    trace_elem :: trace


let report_expensive_call_stack pname loc trace stack_str expensive_pname call_loc =
  let final_trace = IList.rev (update_trace trace call_loc) in
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
  let exn =
    Exceptions.Checkers (calls_expensive_method, Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc: (Some loc) ~ltr: (Some final_trace) exn


let report_allocation_stack pname loc trace stack_str constructor_pname call_loc =
  let final_trace = IList.rev (update_trace trace call_loc) in
  let constr_str = string_of_pname constructor_pname in
  let description =
    Printf.sprintf
      "Method `%s` annotated with `@%s` allocates `%s` via `%s%s`"
      (Procname.to_simplified_string pname)
      Annotations.no_allocation
      constr_str
      stack_str
      ("new "^constr_str) in
  let exn =
    Exceptions.Checkers (allocates_memory, Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc: (Some loc) ~ltr: (Some final_trace) exn


let report_call_stack end_of_stack lookup_next_calls report tenv pname pdesc loc calls =
  let rec loop visited_pnames (trace, stack_str) (callee_pname, callee_loc) =
    if end_of_stack tenv callee_pname then
      report pname loc trace stack_str callee_pname callee_loc
    else
      let next_calls = lookup_next_calls callee_pname in
      let callee_pname_str = string_of_pname callee_pname in
      let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
      let new_trace = update_trace trace callee_loc in
      let unseen_pnames, updated_visited =
        IList.fold_left
          (fun (accu, set) (p, loc) ->
             if Procname.Set.mem p set then (accu, set)
             else ((p, loc) :: accu, Procname.Set.add p set))
          ([], visited_pnames) next_calls in
      IList.iter (loop updated_visited (new_trace, new_stack_str)) unseen_pnames in
  let start_trace = update_trace [] (Cfg.Procdesc.get_loc pdesc) in
  IList.iter (loop Procname.Set.empty (start_trace, "")) calls


let report_expensive_calls tenv pname pdesc loc calls =
  report_call_stack
    method_is_expensive lookup_expensive_calls report_expensive_call_stack
    tenv pname pdesc loc calls


let report_allocations tenv pname pdesc loc calls =
  report_call_stack
    (fun _ p -> Procname.is_constructor p) lookup_allocations report_allocation_stack
    tenv pname pdesc loc calls


let check_one_procedure tenv pname pdesc =

  let loc = Cfg.Procdesc.get_loc pdesc in
  let attributes = Cfg.Procdesc.get_attributes pdesc in
  let expensive = is_expensive attributes
  and performance_critical =
    method_overrides_performance_critical tenv pname
  and no_allocation =
    method_overrides_no_allocation tenv pname in

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

  let call_summary =
    let checked_pnames = ref Procname.Set.empty in
    let empty_summary =
      { Specs.expensive_calls = [];
        allocations = [] } in
    Cfg.Procdesc.fold_calls
      (collect_calls tenv pdesc checked_pnames) empty_summary pdesc in

  update_summary call_summary pname;

  if performance_critical then
    report_expensive_calls tenv pname pdesc loc call_summary.Specs.expensive_calls;
  if no_allocation then
    report_allocations tenv pname pdesc loc call_summary.Specs.allocations


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

(*
let is_performance_critical attributes =
  check_attributes Annotations.ia_is_performance_critical attributes
*)
