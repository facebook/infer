(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter
module Domain = AnnotationReachabilityDomain

let annotation_of_str annot_str = {Annot.class_name= annot_str; parameters= []}

let dummy_constructor_annot = annotation_of_str "__infer_is_constructor"

let is_dummy_constructor annot =
  String.equal annot.Annot.class_name dummy_constructor_annot.class_name


let dummy_field_method_prefix = "__infer_field_"

let is_dummy_field_pname pname =
  String.is_prefix ~prefix:dummy_field_method_prefix (Procname.get_method pname)


let dummy_pname_for_field fieldname typ =
  let class_name = Option.value_exn (Typ.name typ) in
  Procname.make_java ~class_name ~return_type:None
    ~method_name:(dummy_field_method_prefix ^ Fieldname.get_field_name fieldname)
    ~parameters:[] ~kind:Non_Static


let classname_from_dummy_pname pname = Option.value_exn (Procname.get_class_type_name pname)

let fieldname_from_dummy_pname pname =
  let classname = classname_from_dummy_pname pname in
  let field_name =
    String.chop_prefix_if_exists ~prefix:dummy_field_method_prefix (Procname.get_method pname)
  in
  Fieldname.make classname field_name


let struct_from_dummy_pname tenv pname =
  Option.value_exn (Tenv.lookup tenv (classname_from_dummy_pname pname))


let is_modeled_expensive tenv = function
  | Procname.Java proc_name_java as proc_name ->
      (not (BuiltinDecl.is_declared proc_name))
      &&
      let is_subclass =
        let classname = Procname.Java.get_class_type_name proc_name_java in
        PatternMatch.is_subtype_of_str tenv classname
      in
      Inferconfig.modeled_expensive_matcher is_subclass proc_name
  | _ ->
      false


let is_allocator tenv pname =
  match pname with
  | Procname.Java pname_java ->
      let is_throwable () =
        let class_name = Procname.Java.get_class_type_name pname_java in
        PatternMatch.Java.is_throwable tenv class_name
      in
      Procname.is_constructor pname
      && (not (BuiltinDecl.is_declared pname))
      && not (is_throwable ())
  | _ ->
      false


type custom_model = {method_regex: string; annotation: string} [@@deriving of_yojson]

type custom_models = custom_model list [@@deriving of_yojson]

let parse_custom_models () =
  match Config.annotation_reachability_custom_models with
  (* The default value for JSON options is an empty list and not an empty object *)
  | `List [] ->
      String.Map.empty
  | json ->
      json |> Yojson.Safe.Util.to_assoc
      |> List.map ~f:(fun (key, val_arr) ->
             ( key
             , val_arr |> Yojson.Safe.Util.to_list
               |> List.map ~f:Yojson.Safe.Util.to_string
               |> List.map ~f:Str.regexp ) )
      |> String.Map.of_alist_exn


let check_attributes check tenv pname =
  let proc_has_attribute = Annotations.pname_has_return_annot pname check in
  let class_has_attribute =
    ( if Config.annotation_reachability_apply_superclass_annotations then
        PatternMatch.Java.check_class_attributes
      else PatternMatch.Java.check_current_class_attributes )
      check tenv pname
  in
  class_has_attribute || proc_has_attribute


let check_modeled_annotation models annot pname =
  let method_name = Procname.to_string ~verbosity:FullNameOnly pname in
  Option.exists (String.Map.find models annot.Annot.class_name) ~f:(fun methods ->
      List.exists methods ~f:(fun r -> Str.string_match r method_name 0) )


let method_has_annot annot models tenv pname =
  let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
  if Config.annotation_reachability_no_allocation && is_dummy_constructor annot then
    is_allocator tenv pname
  else if
    Config.annotation_reachability_expensive
    && Annotations.annot_ends_with annot Annotations.expensive
  then check_attributes has_annot tenv pname || is_modeled_expensive tenv pname
  else check_attributes has_annot tenv pname || check_modeled_annotation models annot pname


let find_override_with_annot annot models tenv pname =
  if is_dummy_field_pname pname then
    (* Get back the original field from the fake call *)
    let struct_typ = struct_from_dummy_pname tenv pname in
    let fieldname = fieldname_from_dummy_pname pname in
    let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
    if Annotations.field_has_annot fieldname struct_typ has_annot then Some pname else None
  else
    let is_annotated = method_has_annot annot models in
    PatternMatch.override_find (fun pn -> is_annotated tenv pn) tenv pname


let method_overrides_annot annot models tenv pname =
  find_override_with_annot annot models tenv pname |> Option.is_some


let lookup_annotation_calls {InterproceduralAnalysis.analyze_dependency} annot pname =
  analyze_dependency pname |> AnalysisResult.to_option
  |> Option.bind ~f:(Domain.find_opt annot)
  |> Option.value ~default:Domain.SinkMap.empty


let update_trace loc trace =
  if Location.equal loc Location.dummy then trace
  else Errlog.make_trace_element 0 loc "" [] :: trace


let str_of_pname ?(withclass = false) pname =
  if is_dummy_field_pname pname then
    (* Get back the original field from the fake call *)
    let fieldname = fieldname_from_dummy_pname pname in
    if withclass then Fieldname.to_simplified_string fieldname
    else Fieldname.get_field_name fieldname
  else Procname.to_simplified_string ~withclass pname


let get_issue_type ~src ~snk =
  if is_dummy_constructor snk then IssueType.checkers_allocates_memory
  else if
    Config.annotation_reachability_expensive
    && String.equal src.Annot.class_name Annotations.performance_critical
  then IssueType.checkers_calls_expensive_method
  else IssueType.checkers_annotation_reachability_error


let report_src_to_snk_path {InterproceduralAnalysis.proc_desc; tenv; err_log} ~src ~snk models loc
    trace snk_pname call_loc =
  let get_original_pname annot pname =
    find_override_with_annot annot models tenv pname |> Option.value ~default:pname
  in
  (* Check if the annotation is inherited from a base class method. *)
  let get_details annot pname =
    let origin_pname = get_original_pname annot pname in
    if Procname.equal origin_pname pname then ""
    else
      Format.asprintf ", inherited from %a" MF.pp_monospaced
        (str_of_pname ~withclass:true origin_pname)
  in
  (* Check if the annotation is inherited from a base class/interface. *)
  let get_class_details annot pname =
    let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
    let pname = get_original_pname annot pname in
    match Procname.get_class_type_name pname with
    | Some typ -> (
      match PatternMatch.Java.find_superclasses_with_attributes has_annot tenv typ with
      | [] ->
          ""
      | types ->
          let typ_to_str t =
            Option.map
              ~f:(fun name -> Format.asprintf "%a" MF.pp_monospaced (JavaClassName.classname name))
              (Typ.Name.Java.get_java_class_name_opt t)
          in
          ", defined on " ^ String.concat ~sep:", " (List.filter_map ~f:typ_to_str types) )
    | None ->
        ""
  in
  (* Check if the annotation is there directly or is modeled. *)
  let get_kind annot pname =
    let pname = get_original_pname annot pname in
    if check_modeled_annotation models annot pname then "modeled as" else "annotated with"
  in
  let src_pname = Procdesc.get_proc_name proc_desc in
  let snk_annot_str = snk.Annot.class_name in
  let src_annot_str = src.Annot.class_name in
  let access_or_call = if is_dummy_field_pname snk_pname then "accesses" else "calls" in
  let description =
    if is_dummy_constructor snk then
      let constr_str = str_of_pname ~withclass:true snk_pname in
      Format.asprintf "Method %a annotated with %a allocates %a via %a" MF.pp_monospaced
        (str_of_pname src_pname) MF.pp_monospaced ("@" ^ src_annot_str) MF.pp_monospaced constr_str
        MF.pp_monospaced ("new " ^ constr_str)
    else
      Format.asprintf "Method %a (%s %a%s%s) %s %a (%s %a%s%s)" MF.pp_monospaced
        (str_of_pname src_pname) (get_kind src src_pname) MF.pp_monospaced ("@" ^ src_annot_str)
        (get_details src src_pname) (get_class_details src src_pname) access_or_call
        MF.pp_monospaced
        (str_of_pname ~withclass:true snk_pname)
        (get_kind snk snk_pname) MF.pp_monospaced ("@" ^ snk_annot_str) (get_details snk snk_pname)
        (get_class_details snk snk_pname)
  in
  let issue_type = get_issue_type ~src ~snk in
  let final_trace = List.rev (update_trace call_loc trace) in
  Reporting.log_issue proc_desc err_log ~loc ~ltr:final_trace AnnotationReachability issue_type
    description


let find_paths_to_snk ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) ~src ~snk
    sink_map models =
  let rec loop fst_call_loc visited_pnames trace (callee_pname, call_loc) =
    let is_end_of_stack proc_name = method_overrides_annot snk models tenv proc_name in
    if is_end_of_stack callee_pname then
      report_src_to_snk_path analysis_data ~src ~snk models fst_call_loc trace callee_pname call_loc
    else if
      Config.annotation_reachability_minimize_sources
      && method_overrides_annot src models tenv callee_pname
    then (* Found a source in the middle, this path is not minimal *)
      ()
    else
      let callee_def_loc =
        Option.value_map ~f:ProcAttributes.get_loc ~default:Location.dummy
          (Attributes.load callee_pname)
      in
      let next_calls = lookup_annotation_calls analysis_data snk callee_pname in
      let new_trace = update_trace call_loc trace |> update_trace callee_def_loc in
      let unseen_callees, updated_callees =
        Domain.SinkMap.fold
          (fun _ call_sites ((unseen, visited) as accu) ->
            try
              let call_site = Domain.CallSites.min_elt call_sites in
              let p = CallSite.pname call_site in
              let loc = CallSite.loc call_site in
              if Procname.Set.mem p visited then accu
              else ((p, loc) :: unseen, Procname.Set.add p visited)
            with Caml.Not_found -> accu )
          next_calls ([], visited_pnames)
      in
      List.iter ~f:(loop fst_call_loc updated_callees new_trace) unseen_callees
  in
  let call_site = CallSite.make (Procdesc.get_proc_name proc_desc) (Procdesc.get_loc proc_desc) in
  Domain.SinkMap.iter
    (fun _ call_sites ->
      try
        let fst_call_site = Domain.CallSites.min_elt call_sites in
        let fst_callee_pname = CallSite.pname fst_call_site in
        let fst_call_loc = CallSite.loc fst_call_site in
        let start_trace = update_trace (CallSite.loc call_site) [] in
        loop fst_call_loc Procname.Set.empty start_trace (fst_callee_pname, fst_call_loc)
      with Caml.Not_found -> () )
    sink_map


let report_src_and_sink {InterproceduralAnalysis.proc_desc; err_log} ~src ~snk =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  let issue_type = get_issue_type ~src ~snk in
  let description =
    Format.asprintf "Method %a is annotated with both %a and %a" MF.pp_monospaced
      (str_of_pname proc_name) MF.pp_monospaced ("@" ^ src.Annot.class_name) MF.pp_monospaced
      ("@" ^ snk.Annot.class_name)
  in
  Reporting.log_issue proc_desc err_log ~loc ~ltr:[] AnnotationReachability issue_type description


let check_srcs_and_find_snk ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) src_list
    snk annot_map models =
  let check_one_src_and_find_snk src =
    let proc_name = Procdesc.get_proc_name proc_desc in
    if method_overrides_annot src models tenv proc_name then (
      (* If there are callsites to sinks, find/report such paths. *)
      Option.iter (Domain.find_opt snk annot_map) ~f:(fun sink_map ->
          find_paths_to_snk analysis_data ~src ~snk sink_map models ) ;
      (* Reporting something that is both a source and a sink at the same time needs to be
         treated as a special case because there is no call/callsite (path of length 0). *)
      if
        Config.annotation_reachability_report_source_and_sink
        && method_overrides_annot snk models tenv proc_name
      then report_src_and_sink analysis_data ~src ~snk )
  in
  List.iter ~f:check_one_src_and_find_snk src_list


module AnnotationSpec = struct
  type predicate = Tenv.t -> Procname.t -> bool

  type t =
    { description: string  (** for debugging *)
    ; sink_predicate: predicate  (** decide if something is a sink *)
    ; sanitizer_predicate: predicate
    ; sink_annotation: Annot.t  (** used as key in the domain (sink -> procedure -> callsite) *)
    ; report: Domain.t InterproceduralAnalysis.t -> Domain.t -> unit }

  (* The default sanitizer does not sanitize anything *)
  let default_sanitizer _ _ = false
end

module StandardAnnotationSpec = struct
  let from_annotations str_src_annots str_snk_annot str_sanitizer_annots models =
    let src_list = List.map str_src_annots ~f:annotation_of_str in
    let sanitizer_annots = List.map str_sanitizer_annots ~f:annotation_of_str in
    let snk = annotation_of_str str_snk_annot in
    let open AnnotationSpec in
    { description= "StandardAnnotationSpec"
    ; sink_predicate= (fun tenv pname -> method_overrides_annot snk models tenv pname)
    ; sanitizer_predicate=
        (fun tenv pname ->
          List.exists sanitizer_annots ~f:(fun s -> method_overrides_annot s models tenv pname) )
    ; sink_annotation= snk
    ; report=
        (fun proc_data annot_map -> check_srcs_and_find_snk proc_data src_list snk annot_map models)
    }
end

module NoAllocationAnnotationSpec = struct
  let no_allocation_annot = annotation_of_str Annotations.no_allocation

  let spec =
    let open AnnotationSpec in
    { description= "NoAllocationAnnotationSpec"
    ; sink_predicate= (fun tenv pname -> is_allocator tenv pname)
    ; sanitizer_predicate=
        (fun tenv pname -> check_attributes Annotations.ia_is_ignore_allocations tenv pname)
    ; sink_annotation= dummy_constructor_annot
    ; report=
        (fun proc_data annot_map ->
          check_srcs_and_find_snk proc_data [no_allocation_annot] dummy_constructor_annot annot_map
            String.Map.empty ) }
end

module ExpensiveAnnotationSpec = struct
  let performance_critical_annot = annotation_of_str Annotations.performance_critical

  let expensive_annot = annotation_of_str Annotations.expensive

  let is_expensive tenv pname = check_attributes Annotations.ia_is_expensive tenv pname

  let method_is_expensive tenv pname = is_modeled_expensive tenv pname || is_expensive tenv pname

  let check_expensive_subtyping_rules {InterproceduralAnalysis.proc_desc; tenv; err_log}
      overridden_pname =
    let proc_name = Procdesc.get_proc_name proc_desc in
    let loc = Procdesc.get_loc proc_desc in
    if not (method_is_expensive tenv overridden_pname) then
      let description =
        Format.asprintf "Method %a overrides unannotated method %a and cannot be annotated with %a"
          MF.pp_monospaced (Procname.to_string proc_name) MF.pp_monospaced
          (Procname.to_string overridden_pname)
          MF.pp_monospaced ("@" ^ Annotations.expensive)
      in
      Reporting.log_issue proc_desc err_log ~loc AnnotationReachability
        IssueType.checkers_expensive_overrides_unexpensive description


  let spec =
    let open AnnotationSpec in
    { description= "ExpensiveAnnotationSpec"
    ; sink_predicate=
        (fun tenv pname ->
          let has_annot ia = Annotations.ia_ends_with ia expensive_annot.class_name in
          check_attributes has_annot tenv pname || is_modeled_expensive tenv pname )
    ; sanitizer_predicate= default_sanitizer
    ; sink_annotation= expensive_annot
    ; report=
        (fun ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) annot_map ->
          let proc_name = Procdesc.get_proc_name proc_desc in
          if is_expensive tenv proc_name then
            PatternMatch.override_iter
              (check_expensive_subtyping_rules analysis_data)
              tenv proc_name ;
          check_srcs_and_find_snk analysis_data [performance_critical_annot] expensive_annot
            annot_map String.Map.empty ) }
end

module MakeTransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data =
    {specs: AnnotationSpec.t list; analysis_data: Domain.t InterproceduralAnalysis.t}

  let is_sink tenv (spec : AnnotationSpec.t) ~caller_pname ~callee_pname =
    spec.sink_predicate tenv callee_pname
    && (not (spec.sanitizer_predicate tenv callee_pname))
    && not (spec.sanitizer_predicate tenv caller_pname)


  let check_call tenv ~caller_pname ~callee_pname call_site astate specs =
    List.fold ~init:astate specs ~f:(fun astate (spec : AnnotationSpec.t) ->
        if is_sink tenv spec ~caller_pname ~callee_pname then (
          L.d_printfln "%s: Adding sink call `%a -> %a`" spec.description Procname.pp caller_pname
            Procname.pp callee_pname ;
          Domain.add_call_site spec.sink_annotation callee_pname call_site astate )
        else astate )


  let merge_callee_map {analysis_data= {proc_desc; tenv; analyze_dependency}; specs} call_site
      ~callee_pname astate =
    match analyze_dependency callee_pname with
    | Error _ ->
        astate
    | Ok callee_call_map ->
        L.d_printf "Applying summary for `%a`@\n" Procname.pp callee_pname ;
        let add_call_site annot sink calls astate =
          if Domain.CallSites.is_empty calls then astate
          else
            (* Add the sink to the current state only if the caller pname is not a sanitizer for
               that sink. Ideally we would check only the spec that was responsible for adding the
               sink but it is not obvious how to link back from annot to specs. Instead see if one
               of the specs thinks that this sink is indeed a sink. *)
            let caller_pname = Procdesc.get_proc_name proc_desc in
            List.fold ~init:astate specs ~f:(fun astate (spec : AnnotationSpec.t) ->
                if is_sink tenv spec ~caller_pname ~callee_pname:sink then (
                  L.d_printf "%s: Adding sink call from `%a`'s summary `%a -> %a`@\n"
                    spec.description Procname.pp callee_pname Procname.pp caller_pname Procname.pp
                    sink ;
                  Domain.add_call_site annot sink call_site astate )
                else astate )
        in
        Domain.fold
          (fun annot sink_map astate -> Domain.SinkMap.fold (add_call_site annot) sink_map astate)
          callee_call_map astate


  let exec_instr astate ({analysis_data= {proc_desc; tenv}; specs} as analysis_data) _ _ = function
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        let caller_pname = Procdesc.get_proc_name proc_desc in
        let call_site = CallSite.make callee_pname call_loc in
        check_call tenv ~callee_pname ~caller_pname call_site astate specs
        |> merge_callee_map analysis_data call_site ~callee_pname
    | Sil.Load {e= Exp.Lfield (_, fieldname, typ); loc} ->
        (* Pretend that field access is a call to a fake method (containing the name of the field) *)
        let caller_pname = Procdesc.get_proc_name proc_desc in
        let callee_pname = dummy_pname_for_field fieldname typ in
        let call_site = CallSite.make callee_pname loc in
        check_call tenv ~callee_pname ~caller_pname call_site astate specs
        |> merge_callee_map analysis_data call_site ~callee_pname
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "annotation reachability"
end

module TransferFunctions = MakeTransferFunctions (ProcCfg.Exceptional)
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

type custom_spec =
  {sources: string list; sinks: string list; sanitizers: string list [@yojson.default []]}
[@@deriving of_yojson]

type custom_specs = custom_spec list [@@deriving of_yojson]

let parse_custom_specs () =
  let models = parse_custom_models () in
  let make_standard_spec_from_custom_spec {sources; sinks; sanitizers} =
    List.map
      ~f:(fun sink -> StandardAnnotationSpec.from_annotations sources sink sanitizers models)
      sinks
  in
  let custom_specs =
    let specs =
      try custom_specs_of_yojson Config.annotation_reachability_custom_pairs
      with _ -> L.die ExternalError "Could not parse annotation reachability custom pairs@."
    in
    List.map specs ~f:make_standard_spec_from_custom_spec
  in
  List.concat custom_specs


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) : Domain.t option =
  let initial = Domain.empty in
  let custom_specs = parse_custom_specs () in
  let expensive_specs =
    if Config.annotation_reachability_expensive then [ExpensiveAnnotationSpec.spec] else []
  in
  let no_alloc_specs =
    if Config.annotation_reachability_no_allocation then [NoAllocationAnnotationSpec.spec] else []
  in
  let specs = expensive_specs @ no_alloc_specs @ custom_specs in
  let proc_data = {TransferFunctions.analysis_data; specs} in
  let post = Analyzer.compute_post proc_data ~initial proc_desc in
  Option.iter post ~f:(fun annot_map ->
      List.iter specs ~f:(fun spec -> spec.AnnotationSpec.report analysis_data annot_map) ) ;
  post
