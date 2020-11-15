(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module F = Format

(* make sure that this is initialized *)
let () = NullsafeInit.init ()

let callback1 ({IntraproceduralAnalysis.proc_desc= curr_pdesc; _} as analysis_data) ~curr_java_pname
    find_canonical_duplicate calls_this checks idenv annotated_signature linereader proc_loc :
    bool * TypeState.t option =
  let curr_pname = Procdesc.get_proc_name curr_pdesc in
  let add_formal typestate (param_signature : AnnotatedSignature.param_signature) =
    let pvar = Pvar.mk param_signature.mangled curr_pname in
    let formal_name = param_signature.mangled in
    let origin =
      if Mangled.is_this formal_name then
        (* `this` is technically an implicit method param, but from syntactic and semantic points of view it
           has very special meaning and nullability limitations (it can never be null).
        *)
        TypeOrigin.This
        (* An extra feature of Nullsafe. If the method overrides `Object.equals()`, but does not specify @Nullable annotation for the param,
           a normal "inconsistent subclass parameter annotation" will be issued.
           But apart from this we want to issue additional violations every time the value is used as a non-nullable.
           This is to distinct "benign" cases where the value is simply not annotated from real bugs where e.g. the value is actually dereferenced.
           We achieve this via having a special TypeOrigin for this param, which will be considered as nullable in relevant places.
        *)
      else if
        PatternMatch.Java.is_override_of_lang_object_equals curr_pname
        && (* Turn this feature off for annotation graph mode. In this mode not annotated, but potentially nullable params
              are treated in special way, and this conflicts with the trick that is being made here.
           *)
        not Config.nullsafe_annotation_graph
      then TypeOrigin.CurrMethodParameter ObjectEqualsOverride
      else TypeOrigin.CurrMethodParameter (Normal param_signature)
    in
    let inferred_nullability = InferredNullability.create origin in
    TypeState.add pvar
      (param_signature.param_annotated_type.typ, inferred_nullability)
      typestate ~descr:"registering formal param"
  in
  let get_initial_typestate () =
    let typestate_empty = TypeState.empty in
    List.fold ~f:add_formal ~init:typestate_empty annotated_signature.AnnotatedSignature.params
  in
  (* Check the nullable flag computed for the return value and report inconsistencies. *)
  let check_return ~java_pname find_canonical_duplicate exit_node final_typestate
      annotated_signature loc : unit =
    let {AnnotatedSignature.ret_annotated_type} = annotated_signature.AnnotatedSignature.ret in
    let ret_pvar = Procdesc.get_ret_var curr_pdesc in
    let ret_range = TypeState.lookup_pvar ret_pvar final_typestate in
    let typ_found_opt =
      match ret_range with Some (typ_found, _) -> Some typ_found | None -> None
    in
    (* TODO(T54088319): model this in AnnotatedNullability *)
    let ret_implicitly_nullable =
      String.equal (PatternMatch.get_type_name ret_annotated_type.typ) "java.lang.Void"
    in
    AnalysisState.set_node exit_node ;
    if not (List.is_empty checks.TypeCheck.check_ret_type) then
      List.iter
        ~f:(fun f -> f curr_pdesc ret_annotated_type.typ typ_found_opt loc)
        checks.TypeCheck.check_ret_type ;
    if checks.TypeCheck.eradicate then
      EradicateChecks.check_return_annotation analysis_data ~java_pname find_canonical_duplicate
        ret_range annotated_signature ret_implicitly_nullable loc
  in
  let do_before_dataflow initial_typestate =
    if Config.eradicate_verbose then
      L.result "Initial Typestate@\n%a@." TypeState.pp initial_typestate
  in
  let do_after_dataflow ~java_pname find_canonical_duplicate final_typestate =
    let exit_node = Procdesc.get_exit_node curr_pdesc in
    check_return ~java_pname find_canonical_duplicate exit_node final_typestate annotated_signature
      proc_loc
  in
  let module DFTypeCheck = DataFlow.MakeDF (struct
    type t = TypeState.t

    let equal = TypeState.equal

    let join = TypeState.join

    let pp_name fmt = F.pp_print_string fmt "eradicate"

    let do_node node typestate =
      AnalysisCallbacks.html_debug_new_node_session ~pp_name node ~f:(fun () ->
          AnalysisState.set_node node ;
          if Config.write_html then L.d_printfln "before:@\n%a@\n" TypeState.pp typestate ;
          let {TypeCheck.normal_flow_typestate; exception_flow_typestates} =
            TypeCheck.typecheck_node analysis_data calls_this checks idenv find_canonical_duplicate
              annotated_signature typestate node linereader
          in
          let normal_flow_typestates =
            Option.value_map normal_flow_typestate ~f:(fun a -> [a]) ~default:[]
          in
          (normal_flow_typestates, exception_flow_typestates) )


    let proc_throws _ = DataFlow.DontKnow
  end) in
  let initial_typestate = get_initial_typestate () in
  do_before_dataflow initial_typestate ;
  let transitions = DFTypeCheck.run curr_pdesc initial_typestate in
  match transitions (Procdesc.get_exit_node curr_pdesc) with
  | DFTypeCheck.Transition (final_typestate, _, _) ->
      do_after_dataflow ~java_pname:curr_java_pname find_canonical_duplicate final_typestate ;
      (!calls_this, Some final_typestate)
  | DFTypeCheck.Dead_state ->
      (!calls_this, None)


let analyze_one_procedure ~java_pname
    ({IntraproceduralAnalysis.tenv; proc_desc; _} as analysis_data) calls_this checks
    annotated_signature linereader proc_loc : unit =
  let idenv = IDEnv.create proc_desc in
  let find_duplicate_nodes = State.mk_find_duplicate_nodes proc_desc in
  let find_canonical_duplicate node =
    let duplicate_nodes = find_duplicate_nodes node in
    try Procdesc.NodeSet.min_elt duplicate_nodes with Caml.Not_found -> node
  in
  let proc_name = Procdesc.get_proc_name proc_desc in
  let caller_nullsafe_mode = NullsafeMode.of_procname tenv proc_name in
  let typecheck_proc do_checks pname pdesc proc_details_opt =
    let ann_sig, loc, idenv_pn =
      match proc_details_opt with
      | Some (ann_sig, loc, idenv_pn) ->
          (ann_sig, loc, idenv_pn)
      | None ->
          let is_callee_in_trust_list =
            let callee_class = Procname.get_class_type_name pname in
            Option.value_map callee_class
              ~f:(fun class_name ->
                Typ.Name.Java.get_java_class_name_exn class_name
                |> NullsafeMode.is_in_trust_list caller_nullsafe_mode )
              ~default:false
          in
          let ann_sig =
            Models.get_modelled_annotated_signature ~is_callee_in_trust_list tenv
              (Procdesc.get_attributes pdesc)
          in
          let loc = Procdesc.get_loc pdesc in
          (ann_sig, loc, IDEnv.create pdesc)
    in
    let checks', calls_this' =
      if do_checks then (checks, calls_this)
      else ({TypeCheck.eradicate= false; check_ret_type= []}, ref false)
    in
    (* NOTE: [analysis_data] has the new [pdesc] *)
    callback1
      {analysis_data with proc_desc= pdesc}
      find_canonical_duplicate calls_this' checks' idenv_pn ann_sig linereader loc
  in
  let do_final_typestate typestate_opt calls_this =
    let do_typestate typestate =
      let start_node = Procdesc.get_start_node proc_desc in
      if
        (not calls_this)
        (* if 'this(...)' is called, no need to check initialization *)
        && checks.TypeCheck.eradicate
      then (
        let typestates_for_curr_constructor_and_all_initializer_methods =
          Initializers.final_initializer_typestates_lazy tenv proc_name proc_desc
            (typecheck_proc ~curr_java_pname:java_pname)
        in
        let typestates_for_all_constructors_incl_current =
          Initializers.final_constructor_typestates_lazy tenv proc_name
            (typecheck_proc ~curr_java_pname:java_pname)
        in
        EradicateChecks.check_constructor_initialization analysis_data find_canonical_duplicate
          start_node ~nullsafe_mode:annotated_signature.AnnotatedSignature.nullsafe_mode
          ~typestates_for_curr_constructor_and_all_initializer_methods
          ~typestates_for_all_constructors_incl_current proc_loc ;
        if Config.eradicate_verbose then L.result "Final Typestate@\n%a@." TypeState.pp typestate )
    in
    match typestate_opt with None -> () | Some typestate -> do_typestate typestate
  in
  let calls_this, final_typestate_opt =
    typecheck_proc ~curr_java_pname:java_pname true proc_name proc_desc
      (Some (annotated_signature, proc_loc, idenv))
  in
  do_final_typestate final_typestate_opt calls_this ;
  if checks.TypeCheck.eradicate then
    EradicateChecks.check_overridden_annotations analysis_data find_canonical_duplicate
      ~proc_name:java_pname annotated_signature ;
  ()


(* If we need to skip analysis, returns a string reason for debug, otherwise None *)
let find_reason_to_skip_analysis proc_name proc_desc =
  match proc_name with
  | Procname.Java java_pname ->
      if Procname.Java.is_access_method java_pname then Some "access method"
      else if
        ThirdPartyAnnotationInfo.is_third_party_proc
          (ThirdPartyAnnotationGlobalRepo.get_repo ())
          java_pname
      then Some "third party method"
      else if (Procdesc.get_attributes proc_desc).ProcAttributes.is_bridge_method then
        Some "bridge method"
      else if Procname.Java.is_autogen_method java_pname then Some "autogenerated method"
      else None
  | _ ->
      Some "not a Java method"


(** Entry point for the nullsafe procedure-level analysis. *)
let analyze checks ({IntraproceduralAnalysis.proc_desc; tenv; _} as analysis_data) :
    NullsafeSummary.t option =
  let proc_name = Procdesc.get_proc_name proc_desc in
  L.debug Analysis Medium "Analysis of %a@\n" Procname.pp proc_name ;
  match find_reason_to_skip_analysis proc_name proc_desc with
  | Some reason ->
      L.debug Analysis Medium "Skipping analysis: %s@\n" reason ;
      None
  | None ->
      (* start the analysis! *)
      let calls_this = ref false in
      let java_pname =
        Procname.as_java_exn ~explanation:"Would have skipped the analysis otherwise" proc_name
      in
      let annotated_signature =
        AnnotatedSignature.get_for_class_under_analysis tenv (Procdesc.get_attributes proc_desc)
      in
      L.debug Analysis Medium "Signature: %a@\n" (AnnotatedSignature.pp proc_name)
        annotated_signature ;
      let loc = Procdesc.get_loc proc_desc in
      let linereader = LineReader.create () in
      (* Initializing TypeErr signleton. *)
      TypeErr.reset () ;
      (* The main method - during this the actual analysis will happen and TypeErr will be populated with
         issues (and some of them - reported).
      *)
      analyze_one_procedure ~java_pname analysis_data calls_this checks annotated_signature
        linereader loc ;
      (* Collect issues that were detected during analysis and put them in summary for further processing *)
      let issues = TypeErr.get_errors () |> List.map ~f:(fun (issues, _) -> issues) in
      (* Report errors of "farall" class - those could not be reported during analysis phase. *)
      TypeErr.report_forall_issues_and_reset analysis_data
        ~nullsafe_mode:annotated_signature.nullsafe_mode ;
      Some {NullsafeSummary.issues}


let analyze_procedure analysis_data =
  let checks = {TypeCheck.eradicate= true; check_ret_type= []} in
  analyze checks analysis_data


let analyze_for_immutable_cast_checker check_return_type analysis_data =
  let checks = {TypeCheck.eradicate= false; check_ret_type= [check_return_type]} in
  analyze checks analysis_data
