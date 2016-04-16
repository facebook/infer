(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

let compare_call (pname1, loc1) (pname2, loc2) =
  let n = Procname.compare pname1 pname2 in
  if n <> 0
  then n
  else Location.compare loc1 loc2

let pp_call fmt (pname, loc) = F.fprintf fmt "%a at %a" Procname.pp pname Location.pp loc

module CallSet = PrettyPrintable.MakePPSet(struct
    type t = Specs.call
    let compare = compare_call
    let pp_element = pp_call
  end)

module CallSetDomain = AbstractDomain.FiniteSet(CallSet)

module Domain = struct
  include AbstractDomain.Pair(CallSetDomain)(CallSetDomain)

  let add_expensive call (expensive_calls, allocations) =
    CallSetDomain.add call expensive_calls, allocations

  let add_allocation alloc (expensive_calls, allocations) =
    expensive_calls, CallSetDomain.add alloc allocations
end

let call_summary_of_astate (astate_expensive, astate_allocations) =
  let expensive_calls = CallSet.elements astate_expensive in
  let allocations = CallSet.elements astate_allocations in
  { Specs.expensive_calls; allocations; }

module Summary = Summary.Make (struct
    type summary = Domain.astate

    let update_payload astate payload =
      let call_summary = call_summary_of_astate astate in
      { payload with Specs.calls = Some call_summary }

    let read_from_payload payload =
      match payload.Specs.calls with
      | Some call_summary ->
          CallSet.of_list call_summary.Specs.expensive_calls,
          CallSet.of_list call_summary.Specs.allocations
      | None -> Domain.initial
  end)

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

let is_modeled_expensive =
  let matcher =
    lazy (let config_file = Inferconfig.inferconfig () in
          Inferconfig.ModeledExpensiveMatcher.load_matcher config_file) in
  fun tenv proc_name -> match proc_name with
    | Procname.Java proc_name_java ->
        not (Builtin.is_registered proc_name) &&
        let classname =
          Typename.Java.from_string (Procname.java_get_class_name proc_name_java) in
        (Lazy.force matcher) (AndroidFramework.is_subclass tenv classname) proc_name
    | _ ->
        false

let check_attributes check tenv pname =
  let check_class_attributes check tenv = function
    | Procname.Java java_pname ->
        begin
          match Annotations.get_declaring_class_annotations java_pname tenv with
          | Some annotations -> check annotations
          | None -> false
        end
    | _ -> false in
  let check_method_attributes check pname =
    match Specs.proc_resolve_attributes pname with
    | None -> false
    | Some attributes ->
        let annotated_signature = Annotations.get_annotated_signature attributes in
        let ret_annotation, _ = annotated_signature.Annotations.ret in
        check ret_annotation in
  check_class_attributes check tenv pname || check_method_attributes check pname

let is_expensive tenv pname =
  check_attributes Annotations.ia_is_expensive tenv pname


let method_is_performance_critical tenv pname =
  check_attributes Annotations.ia_is_performance_critical tenv pname


let method_is_no_allocation tenv pname =
  check_attributes Annotations.ia_is_no_allocation tenv pname


let method_overrides is_annotated tenv pname =
  let overrides () =
    let found = ref false in
    PatternMatch.proc_iter_overridden_methods
      (fun pn -> found := is_annotated tenv pn)
      tenv pname;
    !found in
  is_annotated tenv pname ||
  overrides ()

let method_overrides_performance_critical tenv pname =
  method_overrides method_is_performance_critical tenv pname

let method_overrides_no_allocation tenv pname =
  method_overrides method_is_no_allocation tenv pname

let method_is_expensive tenv pname =
  is_modeled_expensive tenv pname ||
  check_attributes Annotations.ia_is_expensive tenv pname

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
  method_is_expensive tenv pname ||
  calls_expensive ()

let is_allocator tenv pname = match pname with
  | Procname.Java pname_java ->
      let is_throwable () =
        let class_name =
          Typename.Java.from_string (Procname.java_get_class_name pname_java) in
        AndroidFramework.is_throwable tenv class_name in
      Procname.is_constructor pname
      && not (Builtin.is_registered pname)
      && not (is_throwable ())
  | _ ->
      false

let method_allocates tenv pname =
  let annotated_ignore_allocation =
    check_attributes Annotations.ia_is_ignore_allocations tenv pname in
  let allocates () =
    match lookup_call_summary pname with
    | Some { Specs.allocations } ->
        allocations <> []
    | None -> false in
  not annotated_ignore_allocation
  && (is_allocator tenv pname || allocates ())

let lookup_location pname =
  match Specs.get_summary pname with
  | None -> Location.dummy
  | Some summary -> summary.Specs.attributes.ProcAttributes.loc

let string_of_pname =
  Procname.to_simplified_string ~withclass:true

let update_trace loc trace =
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
  let final_trace = IList.rev (update_trace call_loc trace) in
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

let report_allocation_stack pname fst_call_loc trace stack_str constructor_pname call_loc =
  let final_trace = IList.rev (update_trace call_loc trace) in
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
  Reporting.log_error pname ~loc: (Some fst_call_loc) ~ltr: (Some final_trace) exn

let report_call_stack end_of_stack lookup_next_calls report pname loc calls =
  let rec loop fst_call_loc visited_pnames (trace, stack_str) (callee_pname, call_loc) =
    if end_of_stack callee_pname then
      report pname fst_call_loc trace stack_str callee_pname call_loc
    else
      let callee_def_loc = lookup_location callee_pname in
      let next_calls = lookup_next_calls callee_pname in
      let callee_pname_str = string_of_pname callee_pname in
      let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
      let new_trace = update_trace call_loc trace |> update_trace callee_def_loc in
      let unseen_pnames, updated_visited =
        IList.fold_left
          (fun (accu, set) (p, loc) ->
             if Procname.Set.mem p set then (accu, set)
             else ((p, loc) :: accu, Procname.Set.add p set))
          ([], visited_pnames) next_calls in
      IList.iter (loop fst_call_loc updated_visited (new_trace, new_stack_str)) unseen_pnames in
  IList.iter
    (fun (fst_callee_pname, fst_call_loc) ->
       let start_trace = update_trace loc [] in
       loop fst_call_loc Procname.Set.empty (start_trace, "") (fst_callee_pname, fst_call_loc))
    calls

let report_expensive_calls tenv pname loc calls =
  report_call_stack
    (method_is_expensive tenv) lookup_expensive_calls
    report_expensive_call_stack pname loc calls

let report_allocations pname loc calls =
  report_call_stack
    Procname.is_constructor lookup_allocations
    report_allocation_stack pname loc calls

module TransferFunctions = struct
  type astate = Domain.astate

  let exec_instr astate { ProcData.pdesc; tenv; } = function
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        (* Run the analysis of callee_pname if not already analyzed *)
        ignore (Summary.read_summary pdesc callee_pname);
        let add_expensive_calls astate =
          if method_calls_expensive tenv callee_pname
          then Domain.add_expensive (callee_pname, call_loc) astate
          else astate in
        let add_allocations astate =
          if method_allocates tenv callee_pname
          then Domain.add_allocation (callee_pname, call_loc) astate
          else astate in
        add_expensive_calls astate
        |> add_allocations
    | _ -> astate
end

module Analyzer =
  AbstractInterpreter.Make
    (ProcCfg.Exceptional)
    (Scheduler.ReversePostorder)
    (Domain)
    (TransferFunctions)

module Interprocedural = struct
  include Analyzer.Interprocedural(Summary)

  let check_and_report ({ Callbacks.proc_desc; proc_name; tenv; } as proc_data) =
    let loc = Cfg.Procdesc.get_loc proc_desc in
    let expensive = is_expensive tenv proc_name
    and performance_critical =
      method_overrides_performance_critical tenv proc_name
    and no_allocation =
      method_overrides_no_allocation tenv proc_name in

    let check_expensive_subtyping_rules overridden_pname =
      if not (method_is_expensive tenv overridden_pname) then
        let description =
          Printf.sprintf
            "Method `%s` overrides unannotated method `%s` and cannot be annotated with `@%s`"
            (Procname.to_string proc_name)
            (Procname.to_string overridden_pname)
            Annotations.expensive in
        let exn =
          Exceptions.Checkers
            (expensive_overrides_unexpensive, Localise.verbatim_desc description) in
        Reporting.log_error proc_name ~loc: (Some loc) ~ltr: None exn in

    if expensive then
      PatternMatch.proc_iter_overridden_methods
        check_expensive_subtyping_rules tenv proc_name;

    match checker proc_data with
    | Some astate ->
        if performance_critical then
          report_expensive_calls tenv proc_name loc (CallSet.elements (fst astate));
        if no_allocation then
          report_allocations proc_name loc (CallSet.elements (snd astate))
    | None -> ()

end
