(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

module CallSiteSet = AbstractDomain.FiniteSet (CallSite.Set)
module CallsDomain = AbstractDomain.Map (Annot.Map) (CallSiteSet)

let dummy_constructor_annot = "__infer_is_constructor"

let annotation_of_str annot_str =
  { Annot.class_name = annot_str; parameters = []; }

let src_snk_pairs () =
  (* parse user-defined specs from .inferconfig *)
  let parse_user_defined_specs = function
    | `List user_specs ->
        let parse_user_spec json =
          let open Yojson.Basic.Util in
          let sources = member "sources" json |> to_list |> List.map ~f:to_string in
          let sinks = member "sink" json |> to_string in
          sources, sinks in
        List.map ~f:parse_user_spec user_specs
    | _ ->
        [] in
  let specs =
    ([Annotations.performance_critical], Annotations.expensive) ::
    ([Annotations.no_allocation], dummy_constructor_annot) ::
    ([Annotations.any_thread; Annotations.for_non_ui_thread], Annotations.ui_thread) ::
    ([Annotations.ui_thread; Annotations.for_ui_thread], Annotations.for_non_ui_thread) ::
    (parse_user_defined_specs Config.annotation_reachability) in
  IList.map
    (fun (src_annot_str_list, snk_annot_str) ->
       IList.map annotation_of_str src_annot_str_list, annotation_of_str snk_annot_str)
    specs

module Domain = struct
  module TrackingVar = AbstractDomain.FiniteSet (Var.Set)
  module TrackingDomain = AbstractDomain.Pair (CallsDomain) (TrackingVar)
  include AbstractDomain.BottomLifted (TrackingDomain)

  let add_call key call = function
    | Bottom -> Bottom
    | NonBottom (call_map, vars) as astate ->
        let call_set =
          try CallsDomain.find key call_map
          with Not_found -> CallSiteSet.empty in
        let call_set' = CallSiteSet.add call call_set in
        if phys_equal call_set' call_set
        then astate
        else NonBottom (CallsDomain.add key call_set' call_map, vars)

  let stop_tracking (_ : astate) = Bottom

  let add_tracking_var var = function
    | Bottom -> Bottom
    | NonBottom (calls, previous_vars) ->
        NonBottom (calls, TrackingVar.add var previous_vars)

  let remove_tracking_var var = function
    | Bottom -> Bottom
    | NonBottom (calls, previous_vars) ->
        NonBottom (calls, TrackingVar.remove var previous_vars)

  let is_tracked_var var = function
    | Bottom -> false
    | NonBottom (_, vars) ->
        TrackingVar.mem var vars
end

module Summary = Summary.Make (struct
    type summary = Domain.astate

    let call_summary_of_astate = function
      | Domain.Bottom -> assert false
      | Domain.NonBottom (call_map, _) ->
          call_map

    let update_payload astate payload =
      let calls = Some (call_summary_of_astate astate) in
      { payload with Specs.calls; }

    let read_from_payload payload =
      match payload.Specs.calls with
      | Some call_summary -> Some (Domain.NonBottom (call_summary, Domain.TrackingVar.empty))
      | None -> None
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

let annotation_reachability_error = "CHECKERS_ANNOTATION_REACHABILITY_ERROR"

let is_modeled_expensive tenv = function
  | Procname.Java proc_name_java as proc_name ->
      not (BuiltinDecl.is_declared proc_name) &&
      let is_subclass =
        let classname = Typename.Java.from_string (Procname.java_get_class_name proc_name_java) in
        PatternMatch.is_subtype_of_str tenv classname in
      Inferconfig.modeled_expensive_matcher is_subclass proc_name
  | _ ->
      false

let is_allocator tenv pname =
  match pname with
  | Procname.Java pname_java ->
      let is_throwable () =
        let class_name =
          Typename.Java.from_string (Procname.java_get_class_name pname_java) in
        PatternMatch.is_throwable tenv class_name in
      Procname.is_constructor pname
      && not (BuiltinDecl.is_declared pname)
      && not (is_throwable ())
  | _ ->
      false

let check_attributes check tenv pname =
  PatternMatch.check_class_attributes check tenv pname ||
  Annotations.pname_has_return_annot pname ~attrs_of_pname:Specs.proc_resolve_attributes check

let method_overrides is_annotated tenv pname =
  PatternMatch.override_exists (fun pn -> is_annotated tenv pn) tenv pname

let method_has_annot annot tenv pname =
  let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
  if Annotations.annot_ends_with annot dummy_constructor_annot
  then is_allocator tenv pname
  else if Annotations.annot_ends_with annot Annotations.expensive
  then check_attributes has_annot tenv pname || is_modeled_expensive tenv pname
  else check_attributes has_annot tenv pname

let method_overrides_annot annot tenv pname =
  method_overrides (method_has_annot annot) tenv pname

let lookup_annotation_calls annot pname : CallSite.t list =
  match Specs.get_summary pname with
  | Some { Specs.payload = { Specs.calls = Some call_map; }; } ->
      begin
        try
          Annot.Map.find annot call_map
          |> CallSiteSet.elements
        with Not_found ->
          []
      end
  | _ -> []

let update_trace loc trace =
  if Location.equal loc Location.dummy then trace
  else
    Errlog.make_trace_element 0 loc "" [] :: trace

let string_of_pname =
  Procname.to_simplified_string ~withclass:true

let report_allocation_stack
    src_annot pname fst_call_loc trace stack_str constructor_pname call_loc =
  let final_trace = IList.rev (update_trace call_loc trace) in
  let constr_str = string_of_pname constructor_pname in
  let description =
    Printf.sprintf
      "Method `%s` annotated with `@%s` allocates `%s` via `%s%s`"
      (Procname.to_simplified_string pname)
      src_annot
      constr_str
      stack_str
      ("new "^constr_str) in
  let exn =
    Exceptions.Checkers (allocates_memory, Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc:fst_call_loc ~ltr:final_trace exn

let report_annotation_stack src_annot snk_annot src_pname loc trace stack_str snk_pname call_loc =
  if String.equal snk_annot dummy_constructor_annot
  then report_allocation_stack src_annot src_pname loc trace stack_str snk_pname call_loc
  else
    let final_trace = IList.rev (update_trace call_loc trace) in
    let exp_pname_str = string_of_pname snk_pname in
    let description =
      Printf.sprintf
        "Method `%s` annotated with `@%s` calls `%s%s` where `%s` is annotated with `@%s`"
        (Procname.to_simplified_string src_pname)
        src_annot
        stack_str
        exp_pname_str
        exp_pname_str
        snk_annot in
    let msg =
      if String.equal src_annot Annotations.performance_critical
      then calls_expensive_method
      else annotation_reachability_error in
    let exn =
      Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error src_pname ~loc ~ltr:final_trace exn

let report_call_stack end_of_stack lookup_next_calls report call_site calls =
  (* TODO: stop using this; we can use the call site instead *)
  let lookup_location pname =
    match Specs.get_summary pname with
    | None -> Location.dummy
    | Some summary -> summary.Specs.attributes.ProcAttributes.loc in
  let rec loop fst_call_loc visited_pnames (trace, stack_str) (callee_pname, call_loc) =
    if end_of_stack callee_pname then
      report (CallSite.pname call_site) fst_call_loc trace stack_str callee_pname call_loc
    else
      let callee_def_loc = lookup_location callee_pname in
      let next_calls = lookup_next_calls callee_pname in
      let callee_pname_str = string_of_pname callee_pname in
      let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
      let new_trace = update_trace call_loc trace |> update_trace callee_def_loc in
      let unseen_pnames, updated_visited =
        List.fold
          ~f:(fun (accu, set) call_site ->
              let p = CallSite.pname call_site in
              let loc = CallSite.loc call_site in
              if Procname.Set.mem p set then (accu, set)
              else ((p, loc) :: accu, Procname.Set.add p set))
          ~init:([], visited_pnames)
          next_calls in
      IList.iter (loop fst_call_loc updated_visited (new_trace, new_stack_str)) unseen_pnames in
  IList.iter
    (fun fst_call_site ->
       let fst_callee_pname = CallSite.pname fst_call_site in
       let fst_call_loc = CallSite.loc fst_call_site in
       let start_trace = update_trace (CallSite.loc call_site) [] in
       loop fst_call_loc Procname.Set.empty (start_trace, "") (fst_callee_pname, fst_call_loc))
    calls

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  (* This is specific to the @NoAllocation and @PerformanceCritical checker
     and the "unlikely" method is used to guard branches that are expected to run sufficiently
     rarely to not affect the performances *)
  let is_unlikely pname =
    match pname with
    | Procname.Java java_pname ->
        String.equal (Procname.java_get_method java_pname) "unlikely"
    | _ -> false

  let is_tracking_exp astate = function
    | Exp.Var id -> Domain.is_tracked_var (Var.of_id id) astate
    | Exp.Lvar pvar -> Domain.is_tracked_var (Var.of_pvar pvar) astate
    | _ -> false

  let prunes_tracking_var astate = function
    | Exp.BinOp (Binop.Eq, lhs, rhs)
      when is_tracking_exp astate lhs ->
        Exp.equal rhs Exp.one
    | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, lhs, rhs), _)
      when is_tracking_exp astate lhs ->
        Exp.equal rhs Exp.zero
    | _ ->
        false

  let method_has_ignore_allocation_annot tenv pname =
    check_attributes Annotations.ia_is_ignore_allocations tenv pname

  (* TODO: generalize this to allow sanitizers for other annotation types, store it in [extras] so
     we can compute it just once *)
  let method_is_sanitizer annot tenv pname =
    if String.equal annot.Annot.class_name dummy_constructor_annot
    then method_has_ignore_allocation_annot tenv pname
    else false

  let add_call call_map tenv callee_pname caller_pname call_site astate =
    let add_call_for_annot annot _ astate =
      let calls =
        try Annot.Map.find annot call_map
        with Not_found -> CallSiteSet.empty in
      if (not (CallSiteSet.is_empty calls) || method_has_annot annot tenv callee_pname) &&
         (not (method_is_sanitizer annot tenv caller_pname))
      then
        Domain.add_call annot call_site astate
      else
        astate in
    match astate with
    | Domain.Bottom -> astate
    | Domain.NonBottom (map, _) ->
        (* for each annotation type T in domain(astate), check if method calls something annotated
           with T *)
        Annot.Map.fold add_call_for_annot map astate

  let exec_instr astate { ProcData.pdesc; tenv; } _ = function
    | Sil.Call (Some (id, _), Const (Cfun callee_pname), _, _, _)
      when is_unlikely callee_pname ->
        Domain.add_tracking_var (Var.of_id id) astate
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        let caller_pname = Procdesc.get_proc_name pdesc in
        let call_site = CallSite.make callee_pname call_loc in
        begin
          (* Runs the analysis of callee_pname if not already analyzed *)
          match Summary.read_summary pdesc callee_pname with
          | Some Domain.NonBottom (call_map, _) ->
              add_call call_map tenv callee_pname caller_pname call_site astate
          | None ->
              add_call Annot.Map.empty tenv callee_pname caller_pname call_site astate
          | Some Domain.Bottom ->
              astate
        end
    | Sil.Load (id, exp, _, _)
      when is_tracking_exp astate exp ->
        Domain.add_tracking_var (Var.of_id id) astate
    | Sil.Store (Exp.Lvar pvar, _, exp, _)
      when is_tracking_exp astate exp ->
        Domain.add_tracking_var (Var.of_pvar pvar) astate
    | Sil.Store (Exp.Lvar pvar, _, _, _) ->
        Domain.remove_tracking_var (Var.of_pvar pvar) astate
    | Sil.Prune (exp, _, _, _)
      when prunes_tracking_var astate exp ->
        Domain.stop_tracking astate
    | Sil.Call (None, _, _, _, _) ->
        failwith "Expecting a return identifier"
    | _ ->
        astate
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Exceptional) (TransferFunctions)

module Interprocedural = struct
  include AbstractInterpreter.Interprocedural(Summary)

  let is_expensive tenv pname =
    check_attributes Annotations.ia_is_expensive tenv pname

  let method_is_expensive tenv pname =
    is_modeled_expensive tenv pname || is_expensive tenv pname

  let check_and_report ({ Callbacks.proc_desc; proc_name; tenv; } as proc_data) =
    let loc = Procdesc.get_loc proc_desc in
    let expensive = is_expensive tenv proc_name in
    (* TODO: generalize so we can check subtyping on arbitrary annotations *)
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
        Reporting.log_error proc_name ~loc exn in

    if expensive then
      PatternMatch.override_iter check_expensive_subtyping_rules tenv proc_name;

    let report_src_snk_paths call_map (src_annot_list, (snk_annot: Annot.t)) =
      let extract_calls_with_annot annot call_map =
        try
          Annot.Map.find annot call_map
          |> CallSiteSet.elements
        with Not_found -> [] in
      let report_src_snk_path (calls : CallSite.t list) (src_annot: Annot.t) =
        if method_overrides_annot src_annot tenv proc_name
        then
          let f_report =
            report_annotation_stack src_annot.class_name snk_annot.class_name in
          report_call_stack
            (method_has_annot snk_annot tenv)
            (lookup_annotation_calls snk_annot)
            f_report
            (CallSite.make proc_name loc)
            calls in
      let calls = extract_calls_with_annot snk_annot call_map in
      if not (Int.equal (IList.length calls) 0)
      then IList.iter (report_src_snk_path calls) src_annot_list in

    let initial =
      let init_map =
        List.fold
          ~f:(fun astate_acc (_, snk_annot) ->
              CallsDomain.add snk_annot CallSiteSet.empty astate_acc)
          ~init:CallsDomain.empty
          (src_snk_pairs ()) in
      Domain.NonBottom
        (init_map, Domain.TrackingVar.empty) in
    match compute_and_store_post
            ~compute_post:(Analyzer.compute_post ~initial)
            ~make_extras:ProcData.make_empty_extras
            proc_data with
    | Some Domain.NonBottom (call_map, _) ->
        IList.iter (report_src_snk_paths call_map) (src_snk_pairs ())
    | Some Domain.Bottom | None ->
        ()
end
