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
module MF = MarkupFormatter

let dummy_constructor_annot = "__infer_is_constructor"

let annotation_of_str annot_str =
  { Annot.class_name = annot_str; parameters = []; }

(* parse user-defined specs from .inferconfig *)
let parse_user_defined_specs = function
  | `List user_specs ->
      let parse_user_spec json =
        let open Yojson.Basic in
        let sources = Util.member "sources" json |> Util.to_list |> List.map ~f:Util.to_string in
        let sinks = Util.member "sink" json |> Util.to_string in
        sources, sinks in
      List.map ~f:parse_user_spec user_specs
  | _ ->
      []

let src_snk_pairs =
  let specs =
    ([Annotations.performance_critical], Annotations.expensive) ::
    ([Annotations.no_allocation], dummy_constructor_annot) ::
    ([Annotations.any_thread; Annotations.for_non_ui_thread], Annotations.ui_thread) ::
    ([Annotations.ui_thread; Annotations.for_ui_thread], Annotations.for_non_ui_thread) ::
    (parse_user_defined_specs Config.annotation_reachability_custom_pairs) in
  List.map
    ~f:(fun (src_annot_str_list, snk_annot_str) ->
        List.map ~f:annotation_of_str src_annot_str_list, annotation_of_str snk_annot_str)
    specs

module Domain = struct
  module TrackingVar = AbstractDomain.FiniteSet (Var)
  module TrackingDomain = AbstractDomain.BottomLifted (TrackingVar)
  include AbstractDomain.Pair (AnnotReachabilityDomain) (TrackingDomain)

  let add_call_site annot sink call_site ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | TrackingDomain.Bottom -> astate
    | TrackingDomain.NonBottom _ ->
        let sink_map =
          try AnnotReachabilityDomain.find annot annot_map
          with Not_found -> AnnotReachabilityDomain.SinkMap.empty in
        let sink_map' =
          if AnnotReachabilityDomain.SinkMap.mem sink sink_map
          then sink_map
          else
            let singleton = AnnotReachabilityDomain.CallSites.singleton call_site in
            AnnotReachabilityDomain.SinkMap.singleton sink singleton in
        if phys_equal sink_map' sink_map
        then astate
        else (AnnotReachabilityDomain.add annot sink_map' annot_map, previous_vstate)

  let stop_tracking (annot_map, _ : astate) =
    (annot_map, TrackingDomain.Bottom)

  let add_tracking_var var ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | TrackingDomain.Bottom -> astate
    | TrackingDomain.NonBottom vars ->
        (annot_map, TrackingDomain.NonBottom (TrackingVar.add var vars))

  let remove_tracking_var var ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | TrackingDomain.Bottom -> astate
    | TrackingDomain.NonBottom vars ->
        (annot_map, TrackingDomain.NonBottom (TrackingVar.remove var vars))

  let is_tracked_var var (_, vstate) =
    match vstate with
    | TrackingDomain.Bottom -> false
    | TrackingDomain.NonBottom vars ->
        TrackingVar.mem var vars

end

module Summary = Summary.Make (struct
    type payload = AnnotReachabilityDomain.astate

    let update_payload annot_map (summary : Specs.summary) =
      { summary with payload = { summary.payload with annot_map = Some annot_map }}

    let read_payload (summary : Specs.summary) =
      summary.payload.annot_map
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
  | Typ.Procname.Java proc_name_java as proc_name ->
      not (BuiltinDecl.is_declared proc_name) &&
      let is_subclass =
        let classname = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name proc_name_java) in
        PatternMatch.is_subtype_of_str tenv classname in
      Inferconfig.modeled_expensive_matcher is_subclass proc_name
  | _ ->
      false

let is_allocator tenv pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      let is_throwable () =
        let class_name =
          Typ.Name.Java.from_string (Typ.Procname.java_get_class_name pname_java) in
        PatternMatch.is_throwable tenv class_name in
      Typ.Procname.is_constructor pname
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

let lookup_annotation_calls caller_pdesc annot pname =
  match Ondemand.analyze_proc_name ~propagate_exceptions:false caller_pdesc pname with
  | Some { Specs.payload = { Specs.annot_map = Some annot_map; }; } ->
      begin
        try
          AnnotReachabilityDomain.find annot annot_map
        with Not_found ->
          AnnotReachabilityDomain.SinkMap.empty
      end
  | _ ->
      AnnotReachabilityDomain.SinkMap.empty

let update_trace loc trace =
  if Location.equal loc Location.dummy then trace
  else
    Errlog.make_trace_element 0 loc "" [] :: trace

let string_of_pname =
  Typ.Procname.to_simplified_string ~withclass:true

let report_allocation_stack
    src_annot summary fst_call_loc trace stack_str constructor_pname call_loc =
  let pname = Specs.get_proc_name summary in
  let final_trace = List.rev (update_trace call_loc trace) in
  let constr_str = string_of_pname constructor_pname in
  let description =
    Format.asprintf
      "Method %a annotated with %a allocates %a via %a"
      MF.pp_monospaced (Typ.Procname.to_simplified_string pname)
      MF.pp_monospaced ("@" ^ src_annot)
      MF.pp_monospaced constr_str
      MF.pp_monospaced (stack_str ^ ("new "^constr_str)) in
  let exn =
    Exceptions.Checkers (allocates_memory, Localise.verbatim_desc description) in
  Reporting.log_error summary ~loc:fst_call_loc ~ltr:final_trace exn

let report_annotation_stack src_annot snk_annot src_summary loc trace stack_str snk_pname call_loc =
  let src_pname = Specs.get_proc_name src_summary in
  if String.equal snk_annot dummy_constructor_annot
  then report_allocation_stack src_annot src_summary loc trace stack_str snk_pname call_loc
  else
    let final_trace = List.rev (update_trace call_loc trace) in
    let exp_pname_str = string_of_pname snk_pname in
    let description =
      Format.asprintf
        "Method %a annotated with %a calls %a where %a is annotated with %a"
        MF.pp_monospaced (Typ.Procname.to_simplified_string src_pname)
        MF.pp_monospaced ("@" ^ src_annot)
        MF.pp_monospaced (stack_str ^ exp_pname_str)
        MF.pp_monospaced exp_pname_str
        MF.pp_monospaced ("@" ^ snk_annot) in
    let msg =
      if String.equal src_annot Annotations.performance_critical
      then calls_expensive_method
      else annotation_reachability_error in
    let exn =
      Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error src_summary ~loc ~ltr:final_trace exn

let report_call_stack summary end_of_stack lookup_next_calls report call_site sink_map =
  (* TODO: stop using this; we can use the call site instead *)
  let lookup_location pname =
    match Specs.get_summary pname with
    | None -> Location.dummy
    | Some summary -> summary.Specs.attributes.ProcAttributes.loc in
  let rec loop fst_call_loc visited_pnames (trace, stack_str) (callee_pname, call_loc) =
    if end_of_stack callee_pname then
      report summary fst_call_loc trace stack_str callee_pname call_loc
    else
      let callee_def_loc = lookup_location callee_pname in
      let next_calls = lookup_next_calls callee_pname in
      let callee_pname_str = string_of_pname callee_pname in
      let new_stack_str = stack_str ^ callee_pname_str ^ " -> " in
      let new_trace = update_trace call_loc trace |> update_trace callee_def_loc in
      let unseen_callees, updated_callees =
        AnnotReachabilityDomain.SinkMap.fold
          (fun _ call_sites ((unseen, visited) as accu) ->
             try
               let call_site = AnnotReachabilityDomain.CallSites.choose call_sites in
               let p = CallSite.pname call_site in
               let loc = CallSite.loc call_site in
               if Typ.Procname.Set.mem p visited then accu
               else ((p, loc) :: unseen, Typ.Procname.Set.add p visited)
             with Not_found -> accu)
          next_calls
          ([], visited_pnames) in
      List.iter ~f:(loop fst_call_loc updated_callees (new_trace, new_stack_str)) unseen_callees in
  AnnotReachabilityDomain.SinkMap.iter
    (fun _ call_sites ->
       try
         let fst_call_site = AnnotReachabilityDomain.CallSites.choose call_sites in
         let fst_callee_pname = CallSite.pname fst_call_site in
         let fst_call_loc = CallSite.loc fst_call_site in
         let start_trace = update_trace (CallSite.loc call_site) [] in
         loop fst_call_loc Typ.Procname.Set.empty (start_trace, "") (fst_callee_pname, fst_call_loc)
       with Not_found -> ())
    sink_map

let method_has_ignore_allocation_annot tenv pname =
  check_attributes Annotations.ia_is_ignore_allocations tenv pname

(* TODO: generalize this to allow sanitizers for other annotation types, store it in [extras] so
   we can compute it just once *)
let method_is_sanitizer annot tenv pname =
  if String.equal annot.Annot.class_name dummy_constructor_annot
  then method_has_ignore_allocation_annot tenv pname
  else false

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain
  type extras = ProcData.no_extras

  (* This is specific to the @NoAllocation and @PerformanceCritical checker
     and the "unlikely" method is used to guard branches that are expected to run sufficiently
     rarely to not affect the performances *)
  let is_unlikely pname =
    match pname with
    | Typ.Procname.Java java_pname ->
        String.equal (Typ.Procname.java_get_method java_pname) "unlikely"
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

  let check_call tenv callee_pname caller_pname call_site astate =
    List.fold
      ~init:astate
      ~f:(fun astate (_, annot) ->
          if method_has_annot annot tenv callee_pname &&
             not (method_is_sanitizer annot tenv caller_pname)
          then Domain.add_call_site annot callee_pname call_site astate
          else astate)
      src_snk_pairs

  let merge_callee_map call_site pdesc callee_pname astate =
    match Summary.read_summary pdesc callee_pname with
    | None -> astate
    | Some callee_call_map ->
        let add_call_site annot sink calls astate =
          if AnnotReachabilityDomain.CallSites.is_empty calls
          then astate
          else Domain.add_call_site annot sink call_site astate in
        AnnotReachabilityDomain.fold
          (fun annot sink_map astate ->
             AnnotReachabilityDomain.SinkMap.fold
               (add_call_site annot)
               sink_map
               astate)
          callee_call_map
          astate

  let exec_instr astate { ProcData.pdesc; tenv; } _ = function
    | Sil.Call (Some (id, _), Const (Cfun callee_pname), _, _, _)
      when is_unlikely callee_pname ->
        Domain.add_tracking_var (Var.of_id id) astate
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        let caller_pname = Procdesc.get_proc_name pdesc in
        let call_site = CallSite.make callee_pname call_loc in
        check_call tenv callee_pname caller_pname call_site astate
        |> merge_callee_map call_site pdesc callee_pname
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

let report_src_snk_path { Callbacks.proc_desc; tenv; summary } sink_map snk_annot src_annot =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  if method_overrides_annot src_annot tenv proc_name
  then
    let f_report =
      report_annotation_stack src_annot.Annot.class_name snk_annot.Annot.class_name in
    report_call_stack
      summary
      (method_has_annot snk_annot tenv)
      (lookup_annotation_calls proc_desc snk_annot)
      f_report
      (CallSite.make proc_name loc)
      sink_map

let report_src_snk_paths proc_data annot_map src_annot_list snk_annot =
  try
    let sink_map = AnnotReachabilityDomain.find snk_annot annot_map in
    List.iter ~f:(report_src_snk_path proc_data sink_map snk_annot) src_annot_list
  with Not_found -> ()

(* New implementation starts here *)

module AnnotationSpec = struct
  type predicate = Tenv.t -> Typ.Procname.t -> bool

  type t = {
    source_predicate: predicate;
    sink_predicate: predicate;
    sanitizer_predicate: predicate;
    report: Callbacks.proc_callback_args -> AnnotReachabilityDomain.astate -> unit;
  }

  (* The default sanitizer does not sanitize anything *)
  let default_sanitizer _ _ = false

  (* The default report function does not report anything *)
  let default_report _ _ = ()
end

module StandardAnnotationSpec = struct
  let from_annotations src_annots snk_annot = AnnotationSpec.{
      source_predicate = (fun tenv pname ->
          List.exists src_annots ~f:(fun a -> method_overrides_annot a tenv pname));
      sink_predicate = (method_has_annot snk_annot);
      sanitizer_predicate = default_sanitizer;
      report = (fun proc_data annot_map ->
          report_src_snk_paths proc_data annot_map src_annots snk_annot)
    }
end

module NoAllocationAnnotationSpec = struct
  let no_allocation_annot = annotation_of_str Annotations.no_allocation
  let constructor_annot = annotation_of_str dummy_constructor_annot

  let spec = AnnotationSpec.{
      source_predicate = (fun tenv pname ->
          method_overrides_annot no_allocation_annot tenv pname);
      sink_predicate = (method_has_annot constructor_annot);
      sanitizer_predicate = method_has_ignore_allocation_annot;
      report = (fun proc_data annot_map ->
          report_src_snk_paths proc_data annot_map [no_allocation_annot] constructor_annot);
    }
end

module ExpensiveAnnotationSpec = struct
  let performance_critical_annot = annotation_of_str Annotations.performance_critical
  let expensive_annot = annotation_of_str Annotations.expensive

  let is_expensive tenv pname =
    check_attributes Annotations.ia_is_expensive tenv pname

  let method_is_expensive tenv pname =
    is_modeled_expensive tenv pname || is_expensive tenv pname

  let check_expensive_subtyping_rules { Callbacks.proc_desc; tenv; summary } overridden_pname =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let loc = Procdesc.get_loc proc_desc in
    if not (method_is_expensive tenv overridden_pname) then
      let description =
        Format.asprintf
          "Method %a overrides unannotated method %a and cannot be annotated with %a"
          MF.pp_monospaced (Typ.Procname.to_string proc_name)
          MF.pp_monospaced (Typ.Procname.to_string overridden_pname)
          MF.pp_monospaced ("@" ^ Annotations.expensive) in
      let exn =
        Exceptions.Checkers
          (expensive_overrides_unexpensive, Localise.verbatim_desc description) in
      Reporting.log_error summary ~loc exn

  let spec = AnnotationSpec.{
      source_predicate = is_expensive;
      sink_predicate = (method_has_annot expensive_annot);
      sanitizer_predicate = default_sanitizer;
      report = (fun ({ Callbacks.tenv; proc_desc } as proc_data) astate ->
          let proc_name = Procdesc.get_proc_name proc_desc in
          if is_expensive tenv proc_name then
            PatternMatch.override_iter (check_expensive_subtyping_rules proc_data) tenv proc_name;
          report_src_snk_paths proc_data astate [performance_critical_annot] expensive_annot
        );
    }
end

let annot_specs =
  let user_defined_specs =
    let specs = parse_user_defined_specs Config.annotation_reachability_custom_pairs in
    List.map specs ~f:(fun (src_annots, snk_annot) ->
        StandardAnnotationSpec.from_annotations
          (List.map ~f:annotation_of_str src_annots) (annotation_of_str snk_annot))
  in
  ExpensiveAnnotationSpec.spec ::
  NoAllocationAnnotationSpec.spec ::
  (StandardAnnotationSpec.from_annotations
     [annotation_of_str Annotations.any_thread ; annotation_of_str Annotations.for_non_ui_thread]
     (annotation_of_str Annotations.ui_thread)) ::
  (StandardAnnotationSpec.from_annotations
     [annotation_of_str Annotations.ui_thread  ; annotation_of_str Annotations.for_ui_thread]
     (annotation_of_str Annotations.for_non_ui_thread)) ::
  user_defined_specs

let checker ({ Callbacks.proc_desc; tenv; summary} as callback) : Specs.summary =
  let initial =
    (AnnotReachabilityDomain.empty, Domain.TrackingDomain.NonBottom Domain.TrackingVar.empty) in
  let proc_data = ProcData.make_default proc_desc tenv in
  match Analyzer.compute_post proc_data ~initial with
  | Some (annot_map, _) ->
      List.iter annot_specs ~f:(fun (spec: AnnotationSpec.t) -> spec.report callback annot_map);
      Summary.update_summary annot_map summary
  | None ->
      summary
