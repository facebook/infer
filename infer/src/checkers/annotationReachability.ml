(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module MF = MarkupFormatter

let dummy_constructor_annot = "__infer_is_constructor"

module Domain = struct
  module TrackingVar = AbstractDomain.FiniteSet (Var)
  module TrackingDomain = AbstractDomain.BottomLifted (TrackingVar)
  include AbstractDomain.Pair (AnnotReachabilityDomain) (TrackingDomain)

  let add_call_site annot sink call_site ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | Bottom ->
        astate
    | NonBottom _ ->
        let sink_map =
          try AnnotReachabilityDomain.find annot annot_map with Caml.Not_found ->
            AnnotReachabilityDomain.SinkMap.empty
        in
        let sink_map' =
          if AnnotReachabilityDomain.SinkMap.mem sink sink_map then sink_map
          else
            let singleton = AnnotReachabilityDomain.CallSites.singleton call_site in
            AnnotReachabilityDomain.SinkMap.singleton sink singleton
        in
        if phys_equal sink_map' sink_map then astate
        else (AnnotReachabilityDomain.add annot sink_map' annot_map, previous_vstate)


  let stop_tracking ((annot_map, _) : t) = (annot_map, Bottom)

  let add_tracking_var var ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | Bottom ->
        astate
    | NonBottom vars ->
        (annot_map, NonBottom (TrackingVar.add var vars))


  let remove_tracking_var var ((annot_map, previous_vstate) as astate) =
    match previous_vstate with
    | Bottom ->
        astate
    | NonBottom vars ->
        (annot_map, NonBottom (TrackingVar.remove var vars))


  let is_tracked_var var (_, vstate) =
    match vstate with Bottom -> false | NonBottom vars -> TrackingVar.mem var vars
end

module Payload = SummaryPayload.Make (struct
  type t = AnnotReachabilityDomain.t

  let update_payloads annot_map (payloads : Payloads.t) = {payloads with annot_map= Some annot_map}

  let of_payloads (payloads : Payloads.t) = payloads.annot_map
end)

let is_modeled_expensive tenv = function
  | Typ.Procname.Java proc_name_java as proc_name ->
      (not (BuiltinDecl.is_declared proc_name))
      &&
      let is_subclass =
        let classname =
          Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name proc_name_java)
        in
        PatternMatch.is_subtype_of_str tenv classname
      in
      Inferconfig.modeled_expensive_matcher is_subclass proc_name
  | _ ->
      false


let is_allocator tenv pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      let is_throwable () =
        let class_name = Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name pname_java) in
        PatternMatch.is_throwable tenv class_name
      in
      Typ.Procname.is_constructor pname
      && (not (BuiltinDecl.is_declared pname))
      && not (is_throwable ())
  | _ ->
      false


let check_attributes check tenv pname =
  PatternMatch.check_class_attributes check tenv pname
  || Annotations.pname_has_return_annot pname ~attrs_of_pname:Summary.proc_resolve_attributes check


let method_overrides is_annotated tenv pname =
  PatternMatch.override_exists (fun pn -> is_annotated tenv pn) tenv pname


let method_has_annot annot tenv pname =
  let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
  if Annotations.annot_ends_with annot dummy_constructor_annot then is_allocator tenv pname
  else if Annotations.annot_ends_with annot Annotations.expensive then
    check_attributes has_annot tenv pname || is_modeled_expensive tenv pname
  else check_attributes has_annot tenv pname


let method_overrides_annot annot tenv pname = method_overrides (method_has_annot annot) tenv pname

let lookup_annotation_calls ~caller_pdesc annot pname =
  match Ondemand.analyze_proc_name ~caller_pdesc pname with
  | Some {Summary.payloads= {Payloads.annot_map= Some annot_map}} -> (
    try AnnotReachabilityDomain.find annot annot_map with Caml.Not_found ->
      AnnotReachabilityDomain.SinkMap.empty )
  | _ ->
      AnnotReachabilityDomain.SinkMap.empty


let update_trace loc trace =
  if Location.equal loc Location.dummy then trace
  else Errlog.make_trace_element 0 loc "" [] :: trace


let string_of_pname = Typ.Procname.to_simplified_string ~withclass:true

let report_allocation_stack src_annot summary fst_call_loc trace stack_str constructor_pname
    call_loc =
  let pname = Summary.get_proc_name summary in
  let final_trace = List.rev (update_trace call_loc trace) in
  let constr_str = string_of_pname constructor_pname in
  let description =
    Format.asprintf "Method %a annotated with %a allocates %a via %a" MF.pp_monospaced
      (Typ.Procname.to_simplified_string pname)
      MF.pp_monospaced ("@" ^ src_annot) MF.pp_monospaced constr_str MF.pp_monospaced
      (stack_str ^ "new " ^ constr_str)
  in
  Reporting.log_error summary ~loc:fst_call_loc ~ltr:final_trace
    IssueType.checkers_allocates_memory description


let report_annotation_stack src_annot snk_annot src_summary loc trace stack_str snk_pname call_loc
    =
  let src_pname = Summary.get_proc_name src_summary in
  if String.equal snk_annot dummy_constructor_annot then
    report_allocation_stack src_annot src_summary loc trace stack_str snk_pname call_loc
  else
    let final_trace = List.rev (update_trace call_loc trace) in
    let exp_pname_str = string_of_pname snk_pname in
    let description =
      Format.asprintf "Method %a annotated with %a calls %a where %a is annotated with %a"
        MF.pp_monospaced
        (Typ.Procname.to_simplified_string src_pname)
        MF.pp_monospaced ("@" ^ src_annot) MF.pp_monospaced (stack_str ^ exp_pname_str)
        MF.pp_monospaced exp_pname_str MF.pp_monospaced ("@" ^ snk_annot)
    in
    let issue_type =
      if String.equal src_annot Annotations.performance_critical then
        IssueType.checkers_calls_expensive_method
      else IssueType.checkers_annotation_reachability_error
    in
    Reporting.log_error src_summary ~loc ~ltr:final_trace issue_type description


let report_call_stack summary end_of_stack lookup_next_calls report call_site sink_map =
  let lookup_location pname =
    Option.value_map ~f:Procdesc.get_loc ~default:Location.dummy (Ondemand.get_proc_desc pname)
  in
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
              let call_site = AnnotReachabilityDomain.CallSites.min_elt call_sites in
              let p = CallSite.pname call_site in
              let loc = CallSite.loc call_site in
              if Typ.Procname.Set.mem p visited then accu
              else ((p, loc) :: unseen, Typ.Procname.Set.add p visited)
            with Caml.Not_found -> accu )
          next_calls ([], visited_pnames)
      in
      List.iter ~f:(loop fst_call_loc updated_callees (new_trace, new_stack_str)) unseen_callees
  in
  AnnotReachabilityDomain.SinkMap.iter
    (fun _ call_sites ->
      try
        let fst_call_site = AnnotReachabilityDomain.CallSites.min_elt call_sites in
        let fst_callee_pname = CallSite.pname fst_call_site in
        let fst_call_loc = CallSite.loc fst_call_site in
        let start_trace = update_trace (CallSite.loc call_site) [] in
        loop fst_call_loc Typ.Procname.Set.empty (start_trace, "") (fst_callee_pname, fst_call_loc)
      with Caml.Not_found -> () )
    sink_map


let report_src_snk_path {Callbacks.proc_desc; tenv; summary} sink_map snk_annot src_annot =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  if method_overrides_annot src_annot tenv proc_name then
    let f_report = report_annotation_stack src_annot.Annot.class_name snk_annot.Annot.class_name in
    report_call_stack summary (method_has_annot snk_annot tenv)
      (lookup_annotation_calls ~caller_pdesc:proc_desc snk_annot)
      f_report (CallSite.make proc_name loc) sink_map


let report_src_snk_paths proc_data annot_map src_annot_list snk_annot =
  try
    let sink_map = AnnotReachabilityDomain.find snk_annot annot_map in
    List.iter ~f:(report_src_snk_path proc_data sink_map snk_annot) src_annot_list
  with Caml.Not_found -> ()


(* New implementation starts here *)

let annotation_of_str annot_str = {Annot.class_name= annot_str; parameters= []}

module AnnotationSpec = struct
  type predicate = Tenv.t -> Typ.Procname.t -> bool

  type t =
    { source_predicate: predicate
    ; sink_predicate: predicate
    ; sanitizer_predicate: predicate
    ; sink_annotation: Annot.t
    ; report: Callbacks.proc_callback_args -> AnnotReachabilityDomain.t -> unit }

  (* The default sanitizer does not sanitize anything *)
  let default_sanitizer _ _ = false
end

module StandardAnnotationSpec = struct
  let from_annotations src_annots snk_annot =
    let open AnnotationSpec in
    { source_predicate=
        (fun tenv pname -> List.exists src_annots ~f:(fun a -> method_overrides_annot a tenv pname))
    ; sink_predicate=
        (fun tenv pname ->
          let has_annot ia = Annotations.ia_ends_with ia snk_annot.Annot.class_name in
          check_attributes has_annot tenv pname )
    ; sanitizer_predicate= default_sanitizer
    ; sink_annotation= snk_annot
    ; report=
        (fun proc_data annot_map -> report_src_snk_paths proc_data annot_map src_annots snk_annot)
    }
end

module NoAllocationAnnotationSpec = struct
  let no_allocation_annot = annotation_of_str Annotations.no_allocation

  let constructor_annot = annotation_of_str dummy_constructor_annot

  let spec =
    let open AnnotationSpec in
    { source_predicate= (fun tenv pname -> method_overrides_annot no_allocation_annot tenv pname)
    ; sink_predicate= (fun tenv pname -> is_allocator tenv pname)
    ; sanitizer_predicate=
        (fun tenv pname -> check_attributes Annotations.ia_is_ignore_allocations tenv pname)
    ; sink_annotation= constructor_annot
    ; report=
        (fun proc_data annot_map ->
          report_src_snk_paths proc_data annot_map [no_allocation_annot] constructor_annot ) }
end

module ExpensiveAnnotationSpec = struct
  let performance_critical_annot = annotation_of_str Annotations.performance_critical

  let expensive_annot = annotation_of_str Annotations.expensive

  let is_expensive tenv pname = check_attributes Annotations.ia_is_expensive tenv pname

  let method_is_expensive tenv pname = is_modeled_expensive tenv pname || is_expensive tenv pname

  let check_expensive_subtyping_rules {Callbacks.proc_desc; tenv; summary} overridden_pname =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let loc = Procdesc.get_loc proc_desc in
    if not (method_is_expensive tenv overridden_pname) then
      let description =
        Format.asprintf "Method %a overrides unannotated method %a and cannot be annotated with %a"
          MF.pp_monospaced
          (Typ.Procname.to_string proc_name)
          MF.pp_monospaced
          (Typ.Procname.to_string overridden_pname)
          MF.pp_monospaced ("@" ^ Annotations.expensive)
      in
      Reporting.log_error summary ~loc IssueType.checkers_expensive_overrides_unexpensive
        description


  let spec =
    let open AnnotationSpec in
    { source_predicate= is_expensive
    ; sink_predicate=
        (fun tenv pname ->
          let has_annot ia = Annotations.ia_ends_with ia expensive_annot.class_name in
          check_attributes has_annot tenv pname || is_modeled_expensive tenv pname )
    ; sanitizer_predicate= default_sanitizer
    ; sink_annotation= expensive_annot
    ; report=
        (fun ({Callbacks.tenv; proc_desc} as proc_data) astate ->
          let proc_name = Procdesc.get_proc_name proc_desc in
          if is_expensive tenv proc_name then
            PatternMatch.override_iter (check_expensive_subtyping_rules proc_data) tenv proc_name ;
          report_src_snk_paths proc_data astate [performance_critical_annot] expensive_annot ) }
end

(* parse user-defined specs from .inferconfig *)
let parse_user_defined_specs = function
  | `List user_specs ->
      let parse_user_spec json =
        let open Yojson.Basic in
        let sources = Util.member "sources" json |> Util.to_list |> List.map ~f:Util.to_string in
        let sinks = Util.member "sink" json |> Util.to_string in
        (sources, sinks)
      in
      List.map ~f:parse_user_spec user_specs
  | _ ->
      []


let annot_specs =
  let user_defined_specs =
    let specs = parse_user_defined_specs Config.annotation_reachability_custom_pairs in
    List.map specs ~f:(fun (src_annots, snk_annot) ->
        StandardAnnotationSpec.from_annotations
          (List.map ~f:annotation_of_str src_annots)
          (annotation_of_str snk_annot) )
  in
  ExpensiveAnnotationSpec.spec :: NoAllocationAnnotationSpec.spec
  :: StandardAnnotationSpec.from_annotations
       [annotation_of_str Annotations.any_thread; annotation_of_str Annotations.for_non_ui_thread]
       (annotation_of_str Annotations.ui_thread)
  :: StandardAnnotationSpec.from_annotations
       [annotation_of_str Annotations.ui_thread; annotation_of_str Annotations.for_ui_thread]
       (annotation_of_str Annotations.for_non_ui_thread)
  :: user_defined_specs


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
        String.equal (Typ.Procname.Java.get_method java_pname) "unlikely"
    | _ ->
        false


  let is_tracking_exp astate = function
    | Exp.Var id ->
        Domain.is_tracked_var (Var.of_id id) astate
    | Exp.Lvar pvar ->
        Domain.is_tracked_var (Var.of_pvar pvar) astate
    | _ ->
        false


  let prunes_tracking_var astate = function
    | Exp.BinOp (Binop.Eq, lhs, rhs) when is_tracking_exp astate lhs ->
        Exp.equal rhs Exp.one
    | Exp.UnOp (Unop.LNot, Exp.BinOp (Binop.Eq, lhs, rhs), _) when is_tracking_exp astate lhs ->
        Exp.equal rhs Exp.zero
    | _ ->
        false


  let check_call tenv callee_pname caller_pname call_site astate =
    List.fold ~init:astate
      ~f:(fun astate (spec : AnnotationSpec.t) ->
        if
          spec.sink_predicate tenv callee_pname && not (spec.sanitizer_predicate tenv caller_pname)
        then Domain.add_call_site spec.sink_annotation callee_pname call_site astate
        else astate )
      annot_specs


  let merge_callee_map call_site pdesc callee_pname astate =
    match Payload.read pdesc callee_pname with
    | None ->
        astate
    | Some callee_call_map ->
        let add_call_site annot sink calls astate =
          if AnnotReachabilityDomain.CallSites.is_empty calls then astate
          else Domain.add_call_site annot sink call_site astate
        in
        AnnotReachabilityDomain.fold
          (fun annot sink_map astate ->
            AnnotReachabilityDomain.SinkMap.fold (add_call_site annot) sink_map astate )
          callee_call_map astate


  let exec_instr astate {ProcData.pdesc; tenv} _ = function
    | Sil.Call ((id, _), Const (Cfun callee_pname), _, _, _) when is_unlikely callee_pname ->
        Domain.add_tracking_var (Var.of_id id) astate
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        let caller_pname = Procdesc.get_proc_name pdesc in
        let call_site = CallSite.make callee_pname call_loc in
        check_call tenv callee_pname caller_pname call_site astate
        |> merge_callee_map call_site pdesc callee_pname
    | Sil.Load (id, exp, _, _) when is_tracking_exp astate exp ->
        Domain.add_tracking_var (Var.of_id id) astate
    | Sil.Store (Exp.Lvar pvar, _, exp, _) when is_tracking_exp astate exp ->
        Domain.add_tracking_var (Var.of_pvar pvar) astate
    | Sil.Store (Exp.Lvar pvar, _, _, _) ->
        Domain.remove_tracking_var (Var.of_pvar pvar) astate
    | Sil.Prune (exp, _, _, _) when prunes_tracking_var astate exp ->
        Domain.stop_tracking astate
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "annotation reachability"
end

module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (ProcCfg.Exceptional))

let checker ({Callbacks.proc_desc; tenv; summary} as callback) : Summary.t =
  let initial = (AnnotReachabilityDomain.empty, NonBottom Domain.TrackingVar.empty) in
  let proc_data = ProcData.make_default proc_desc tenv in
  match Analyzer.compute_post proc_data ~initial with
  | Some (annot_map, _) ->
      List.iter annot_specs ~f:(fun (spec : AnnotationSpec.t) -> spec.report callback annot_map) ;
      Payload.update_summary annot_map summary
  | None ->
      summary
