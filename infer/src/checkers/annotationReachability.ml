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
module U = Utils

let dummy_constructor_annot = "__infer_is_constructor"

module Domain = AnnotationReachabilityDomain

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


let check_attributes check tenv pname =
  PatternMatch.Java.check_class_attributes check tenv pname
  || Annotations.pname_has_return_annot pname check


let method_overrides is_annotated tenv pname =
  PatternMatch.override_exists (fun pn -> is_annotated tenv pn) tenv pname


let method_has_annot annot tenv pname =
  let has_annot ia = Annotations.ia_ends_with ia annot.Annot.class_name in
  if Annotations.annot_ends_with annot dummy_constructor_annot then is_allocator tenv pname
  else if Annotations.annot_ends_with annot Annotations.expensive then
    check_attributes has_annot tenv pname || is_modeled_expensive tenv pname
  else check_attributes has_annot tenv pname


let method_overrides_annot annot tenv pname = method_overrides (method_has_annot annot) tenv pname

let lookup_annotation_calls {InterproceduralAnalysis.analyze_dependency} annot pname =
  analyze_dependency pname
  |> Option.bind ~f:(fun (_, astate) -> Domain.find_opt annot astate)
  |> Option.value ~default:Domain.SinkMap.empty


let update_trace loc trace =
  if Location.equal loc Location.dummy then trace
  else Errlog.make_trace_element 0 loc "" [] :: trace


let string_of_pname = Procname.to_simplified_string ~withclass:true

let report_allocation_stack {InterproceduralAnalysis.proc_desc; err_log} src_annot fst_call_loc
    trace constructor_pname call_loc =
  let pname = Procdesc.get_proc_name proc_desc in
  let final_trace = List.rev (update_trace call_loc trace) in
  let constr_str = string_of_pname constructor_pname in
  let description =
    Format.asprintf "Method %a annotated with %a allocates %a via %a" MF.pp_monospaced
      (Procname.to_simplified_string pname)
      MF.pp_monospaced ("@" ^ src_annot) MF.pp_monospaced constr_str MF.pp_monospaced
      ("new " ^ constr_str)
  in
  Reporting.log_issue proc_desc err_log ~loc:fst_call_loc ~ltr:final_trace AnnotationReachability
    IssueType.checkers_allocates_memory description


let report_annotation_stack ({InterproceduralAnalysis.proc_desc; err_log} as analysis_data)
    src_annot snk_annot loc trace snk_pname call_loc =
  let src_pname = Procdesc.get_proc_name proc_desc in
  if String.equal snk_annot dummy_constructor_annot then
    report_allocation_stack analysis_data src_annot loc trace snk_pname call_loc
  else
    let final_trace = List.rev (update_trace call_loc trace) in
    let exp_pname_str = string_of_pname snk_pname in
    let description =
      Format.asprintf "Method %a annotated with %a calls %a where %a is annotated with %a"
        MF.pp_monospaced
        (Procname.to_simplified_string src_pname)
        MF.pp_monospaced ("@" ^ src_annot) MF.pp_monospaced exp_pname_str MF.pp_monospaced
        exp_pname_str MF.pp_monospaced ("@" ^ snk_annot)
    in
    let issue_type =
      if String.equal src_annot Annotations.performance_critical then
        IssueType.checkers_calls_expensive_method
      else IssueType.checkers_annotation_reachability_error
    in
    Reporting.log_issue proc_desc err_log ~loc ~ltr:final_trace AnnotationReachability issue_type
      description


let report_call_stack end_of_stack lookup_next_calls report call_site sink_map =
  let lookup_location pname =
    Option.value_map ~f:ProcAttributes.get_loc ~default:Location.dummy
      (AnalysisCallbacks.proc_resolve_attributes pname)
  in
  let rec loop fst_call_loc visited_pnames trace (callee_pname, call_loc) =
    if end_of_stack callee_pname then report fst_call_loc trace callee_pname call_loc
    else
      let callee_def_loc = lookup_location callee_pname in
      let next_calls = lookup_next_calls callee_pname in
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


let report_src_snk_path ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) sink_map
    snk_annot src_annot =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let loc = Procdesc.get_loc proc_desc in
  if method_overrides_annot src_annot tenv proc_name then
    let f_report =
      report_annotation_stack analysis_data src_annot.Annot.class_name snk_annot.Annot.class_name
    in
    report_call_stack (method_has_annot snk_annot tenv)
      (lookup_annotation_calls analysis_data snk_annot)
      f_report (CallSite.make proc_name loc) sink_map


let report_src_snk_paths proc_data annot_map src_annot_list snk_annot =
  try
    let sink_map = Domain.find snk_annot annot_map in
    List.iter ~f:(report_src_snk_path proc_data sink_map snk_annot) src_annot_list
  with Caml.Not_found -> ()


let annotation_of_str annot_str = {Annot.class_name= annot_str; parameters= []}

module AnnotationSpec = struct
  type predicate = Tenv.t -> Procname.t -> bool

  type t =
    { description: string  (** for debugging *)
    ; source_predicate: predicate
    ; sink_predicate: predicate
    ; sanitizer_predicate: predicate
    ; sink_annotation: Annot.t
    ; report: Domain.t InterproceduralAnalysis.t -> Domain.t -> unit }

  (* The default sanitizer does not sanitize anything *)
  let default_sanitizer _ _ = false
end

module StandardAnnotationSpec = struct
  let from_annotations str_src_annots str_snk_annot =
    let src_annots = List.map str_src_annots ~f:annotation_of_str in
    let snk_annot = annotation_of_str str_snk_annot in
    let has_annot ia = Annotations.ia_ends_with ia snk_annot.Annot.class_name in
    let open AnnotationSpec in
    { description= "StandardAnnotationSpec"
    ; source_predicate=
        (fun tenv pname -> List.exists src_annots ~f:(fun a -> method_overrides_annot a tenv pname))
    ; sink_predicate= (fun tenv pname -> check_attributes has_annot tenv pname)
    ; sanitizer_predicate= default_sanitizer
    ; sink_annotation= snk_annot
    ; report=
        (fun proc_data annot_map -> report_src_snk_paths proc_data annot_map src_annots snk_annot)
    }
end

module CxxAnnotationSpecs = struct
  let src_path_of pname =
    match AnalysisCallbacks.proc_resolve_attributes pname with
    | Some proc_attrs ->
        let loc = ProcAttributes.get_loc proc_attrs in
        SourceFile.to_string loc.file
    | None ->
        ""


  (* Does <str_or_prefix> equal <str> or a delimited prefix of <prefix>? *)
  let prefix_match ~delim str str_or_prefix =
    String.equal str str_or_prefix
    || (String.is_prefix ~prefix:str_or_prefix str && String.is_suffix ~suffix:delim str_or_prefix)


  let symbol_match = prefix_match ~delim:"::"

  let path_match = prefix_match ~delim:"/"

  let option_name = "--annotation-reachability-cxx"

  let src_option_name = "--annotation-reachability-cxx-sources"

  let cxx_string_of_pname pname =
    let chop_prefix s =
      String.chop_prefix s ~prefix:Config.clang_inner_destructor_prefix |> Option.value ~default:s
    in
    let pname_str = Procname.to_string pname in
    let i = Option.value (String.rindex pname_str ':') ~default:(-1) + 1 in
    let slen = String.length pname_str in
    String.sub pname_str ~pos:0 ~len:i
    ^ chop_prefix (String.sub pname_str ~pos:i ~len:(slen - i))
    ^ "()"


  let debug_pred ~spec_name ~desc pred pname =
    L.d_printf "%s: Checking if `%a` is a %s... " spec_name Procname.pp pname desc ;
    let r = pred pname in
    L.d_printf "%b %s.@." r desc ;
    r


  let at_least_one_nonempty ~src symbols symbol_regexps paths =
    if List.is_empty symbols && Option.is_none symbol_regexps && List.is_empty paths then
      L.die UserError "Must specify at least one of `paths`, `symbols`, or `symbols_regexps` in %s"
        src


  let spec_from_config spec_name spec_cfg source_overrides =
    let src = option_name ^ " -> " ^ spec_name in
    let make_pname_pred entry ~src : Procname.t -> bool =
      let symbols = U.yojson_lookup entry "symbols" ~src ~f:U.string_list_of_yojson ~default:[] in
      let symbol_regexps =
        U.yojson_lookup entry "symbol_regexps" ~src ~default:None ~f:(fun json ~src ->
            U.string_list_of_yojson json ~src |> String.concat ~sep:"\\|" |> Str.regexp
            |> Option.some )
      in
      let paths = U.yojson_lookup entry "paths" ~src ~f:U.string_list_of_yojson ~default:[] in
      at_least_one_nonempty ~src symbols symbol_regexps paths ;
      let sym_pred pname_string = List.exists ~f:(symbol_match pname_string) symbols in
      let sym_regexp_pred pname_string =
        match symbol_regexps with
        | None ->
            false
        | Some regexp ->
            Str.string_match regexp pname_string 0
      in
      let path_pred pname = List.exists ~f:(path_match (src_path_of pname)) paths in
      fun pname ->
        let pname_string = Procname.to_string pname in
        sym_pred pname_string || sym_regexp_pred pname_string || path_pred pname
    in
    let sources, sources_src =
      if List.length source_overrides > 0 then (source_overrides, src_option_name)
      else
        ( U.yojson_lookup spec_cfg "sources" ~src ~f:U.assoc_of_yojson ~default:[]
        , src ^ " -> sources" )
    in
    let src_name = spec_name ^ "-source" in
    let src_desc =
      U.yojson_lookup sources "desc" ~src:sources_src ~f:U.string_of_yojson ~default:src_name
    in
    let src_pred pname =
      make_pname_pred sources ~src:sources_src pname
      &&
      match pname with
      | Procname.ObjC_Cpp cname ->
          not (Procname.ObjC_Cpp.is_inner_destructor cname)
      | _ ->
          true
    in
    let src_pred = debug_pred ~spec_name ~desc:"source" src_pred in
    let sinks = U.yojson_lookup spec_cfg "sinks" ~src ~f:U.assoc_of_yojson ~default:[] in
    let sinks_src = src ^ " -> sinks" in
    let snk_name = spec_name ^ "-sink" in
    let snk_desc =
      U.yojson_lookup sinks "desc" ~src:sinks_src ~f:U.string_of_yojson ~default:snk_name
    in
    let snk_pred = make_pname_pred sinks ~src:sinks_src in
    let snk_pred = debug_pred ~spec_name ~desc:"sink" snk_pred in
    let overrides =
      U.yojson_lookup sinks "overrides" ~src:sinks_src ~f:U.assoc_of_yojson ~default:[]
    in
    let sanitizer_pred =
      if List.is_empty overrides then fun _ -> false
      else make_pname_pred overrides ~src:(sinks_src ^ " -> overrides")
    in
    let sanitizer_pred = debug_pred ~spec_name ~desc:"sanitizer" sanitizer_pred in
    let call_str = "\n    -> " in
    let report_cxx_annotation_stack {InterproceduralAnalysis.proc_desc; err_log} loc trace snk_pname
        call_loc =
      let src_pname = Procdesc.get_proc_name proc_desc in
      let final_trace = List.rev (update_trace call_loc trace) in
      let snk_pname_str = cxx_string_of_pname snk_pname in
      let src_pname_str = cxx_string_of_pname src_pname in
      let description =
        Format.asprintf "%s can reach %s:\n    %s%s%s\n" src_desc snk_desc src_pname_str call_str
          snk_pname_str
      in
      let issue_type =
        let doc_url =
          Option.value_map ~default:""
            ~f:(U.string_of_yojson ~src:(src ^ " -> doc_url"))
            (List.Assoc.find ~equal:String.equal spec_cfg "doc_url")
        in
        let linters_def_file = Option.value_map ~default:"" ~f:Fn.id Config.inferconfig_file in
        IssueType.register_dynamic ~id:spec_name ~doc_url ~linters_def_file:(Some linters_def_file)
          Error AnnotationReachability
      in
      Reporting.log_issue proc_desc err_log ~loc ~ltr:final_trace AnnotationReachability issue_type
        description
    in
    let snk_annot = annotation_of_str snk_name in
    let report ({InterproceduralAnalysis.proc_desc} as analysis_data) annot_map =
      let proc_name = Procdesc.get_proc_name proc_desc in
      if src_pred proc_name then
        let loc = Procdesc.get_loc proc_desc in
        try
          let sink_map = Domain.find snk_annot annot_map in
          report_call_stack snk_pred
            (lookup_annotation_calls analysis_data snk_annot)
            (report_cxx_annotation_stack analysis_data)
            (CallSite.make proc_name loc) sink_map
        with Caml.Not_found -> ()
    in
    { AnnotationSpec.description= Printf.sprintf "CxxAnnotationSpecs %s from config" spec_name
    ; source_predicate= (fun _ pname -> src_pred pname) (* not used! *)
    ; sink_predicate= (fun _ pname -> snk_pred pname)
    ; sanitizer_predicate= (fun _ pname -> sanitizer_pred pname)
    ; sink_annotation= snk_annot
    ; report }


  let annotation_reachability_cxx =
    U.assoc_of_yojson Config.annotation_reachability_cxx ~src:option_name


  let annotation_reachability_cxx_sources =
    U.assoc_of_yojson Config.annotation_reachability_cxx_sources ~src:src_option_name


  let from_config () : 'AnnotationSpec list =
    List.map
      ~f:(fun (spec_name, spec_cfg) ->
        let src = option_name ^ " -> " ^ spec_name in
        spec_from_config spec_name (U.assoc_of_yojson spec_cfg ~src)
          annotation_reachability_cxx_sources )
      annotation_reachability_cxx
end

module NoAllocationAnnotationSpec = struct
  let no_allocation_annot = annotation_of_str Annotations.no_allocation

  let constructor_annot = annotation_of_str dummy_constructor_annot

  let spec =
    let open AnnotationSpec in
    { description= "NoAllocationAnnotationSpec"
    ; source_predicate= (fun tenv pname -> method_overrides_annot no_allocation_annot tenv pname)
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
    ; source_predicate= is_expensive
    ; sink_predicate=
        (fun tenv pname ->
          let has_annot ia = Annotations.ia_ends_with ia expensive_annot.class_name in
          check_attributes has_annot tenv pname || is_modeled_expensive tenv pname )
    ; sanitizer_predicate= default_sanitizer
    ; sink_annotation= expensive_annot
    ; report=
        (fun ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) astate ->
          let proc_name = Procdesc.get_proc_name proc_desc in
          if is_expensive tenv proc_name then
            PatternMatch.override_iter
              (check_expensive_subtyping_rules analysis_data)
              tenv proc_name ;
          report_src_snk_paths analysis_data astate [performance_critical_annot] expensive_annot )
    }
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
    parse_user_defined_specs Config.annotation_reachability_custom_pairs
    |> List.map ~f:(fun (str_src_annots, str_snk_annot) ->
           StandardAnnotationSpec.from_annotations str_src_annots str_snk_annot )
  in
  let open Annotations in
  let cannot_call_ui_annots = [any_thread; worker_thread] in
  let cannot_call_non_ui_annots = [any_thread; mainthread; ui_thread] in
  [ (Language.Clang, CxxAnnotationSpecs.from_config ())
  ; ( Language.Java
    , ExpensiveAnnotationSpec.spec :: NoAllocationAnnotationSpec.spec
      :: StandardAnnotationSpec.from_annotations cannot_call_ui_annots ui_thread
      :: StandardAnnotationSpec.from_annotations cannot_call_ui_annots mainthread
      :: StandardAnnotationSpec.from_annotations cannot_call_non_ui_annots worker_thread
      :: user_defined_specs ) ]


let get_annot_specs pname =
  let language =
    match pname with
    | Procname.Java _ ->
        Language.Java
    | Procname.ObjC_Cpp _ | Procname.C _ | Procname.Block _ ->
        Language.Clang
    | _ ->
        L.die InternalError "Cannot find language for proc %s" (Procname.to_string pname)
  in
  List.Assoc.find_exn ~equal:Language.equal annot_specs language


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
    | None ->
        astate
    | Some (_, callee_call_map) ->
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


  let exec_instr astate ({analysis_data= {proc_desc; tenv}; specs} as analysis_data) _ = function
    | Sil.Call (_, Const (Cfun callee_pname), _, call_loc, _) ->
        let caller_pname = Procdesc.get_proc_name proc_desc in
        let call_site = CallSite.make callee_pname call_loc in
        check_call tenv ~callee_pname ~caller_pname call_site astate specs
        |> merge_callee_map analysis_data call_site ~callee_pname
    | _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "annotation reachability"
end

module TransferFunctions = MakeTransferFunctions (ProcCfg.Exceptional)
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions)

let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) : Domain.t option =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let initial = Domain.empty in
  let specs = get_annot_specs proc_name in
  let proc_data = {TransferFunctions.analysis_data; specs} in
  let post = Analyzer.compute_post proc_data ~initial proc_desc in
  Option.iter post ~f:(fun annot_map ->
      List.iter specs ~f:(fun spec -> spec.AnnotationSpec.report analysis_data annot_map) ) ;
  post
