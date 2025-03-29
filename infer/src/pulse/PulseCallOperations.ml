(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

let is_ptr_to_const formal_typ_opt = Option.exists formal_typ_opt ~f:Typ.is_ptr_to_const

let add_returned_from_unknown callee_pname_opt ret_val actuals astate =
  if
    (not (List.is_empty actuals))
    && Option.value_map callee_pname_opt ~default:true ~f:(fun pname ->
           not (Procname.is_constructor pname) )
  then
    AbductiveDomain.AddressAttributes.add_one ret_val
      (ReturnedFromUnknown (List.map actuals ~f:(fun ((v, _), _) -> v)))
      astate
  else astate


(** if the procedure has a variadic number of arguments, its known [formals] will be less than the
    [actuals] we get but currently there is no support for handling the remaining arguments (the
    ones in [...]) so we just drop them *)
let trim_actuals_if_var_arg proc_name_opt ~formals ~actuals =
  let proc_attrs = Option.bind ~f:IRAttributes.load proc_name_opt in
  if Option.exists proc_attrs ~f:(fun {ProcAttributes.is_clang_variadic} -> is_clang_variadic) then
    List.take actuals (List.length formals)
  else actuals


let is_const_version pname_method other_method =
  String.equal pname_method (Procname.get_method other_method)
  && Option.exists (IRAttributes.load other_method) ~f:(fun attr ->
         attr.ProcAttributes.is_cpp_const_member_fun )


let is_const_version_available tenv pname =
  Procname.get_class_type_name pname
  |> Option.exists ~f:(fun name ->
         Tenv.lookup tenv name
         |> Option.exists ~f:(fun Struct.{methods} ->
                let pname_method = Procname.get_method pname in
                List.exists ~f:(is_const_version pname_method) methods ) )


let matches_iter =
  QualifiedCppName.Match.of_fuzzy_qual_names
    [ "std::__detail::_Node_iterator"
    ; "std::__wrap_iter"
    ; "std::_Rb_tree_iterator"
    ; "__gnu_cxx::__normal_iterator" ]


module GlobalForStats = struct
  type t = {node_is_not_stuck: bool; one_call_is_stuck: bool}

  let empty = {node_is_not_stuck= false; one_call_is_stuck= false}

  let global = AnalysisGlobalState.make_dls ~init:(fun () -> empty)

  let init_before_call () =
    Utils.with_dls global ~f:(fun global -> {global with node_is_not_stuck= false})


  let is_node_not_stuck () = (DLS.get global).node_is_not_stuck

  let node_is_not_stuck () =
    Utils.with_dls global ~f:(fun global -> {global with node_is_not_stuck= true})


  let is_one_call_stuck () = (DLS.get global).one_call_is_stuck

  let one_call_is_stuck () =
    Utils.with_dls global ~f:(fun global -> {global with one_call_is_stuck= true})
end

let print_arity_mismatch_message ?(extra_call_prefix = "") callee_pname_opt ~formals ~actuals =
  let formals_n = List.length formals in
  let actuals_n = List.length actuals in
  let pp_args pp_val fmt args =
    Pp.seq ~sep:"," (Pp.pair ~fst:pp_val ~snd:(Typ.pp_full Pp.text)) fmt args
  in
  let message =
    F.asprintf
      "Arities mismatch in%s call to %a, something bogus is going on.@\n\
      \  %d formals: %a@\n\
      \  %d actuals: %a@\n"
      extra_call_prefix (Pp.option Procname.pp_verbose) callee_pname_opt formals_n
      (pp_args (Pvar.pp Pp.text))
      formals actuals_n
      (pp_args (fun fmt (v, _hist) -> AbstractValue.pp fmt v))
      actuals
  in
  L.debug Analysis Medium "%s" message ;
  L.d_printfln "%s" message ;
  ()


let unknown_call tenv ({PathContext.timestamp} as path) call_loc (reason : CallEvent.t)
    ?(force_pure = false) callee_pname_opt ~ret ~actuals ~formals_opt astate0 =
  let hist =
    let actuals_hists = List.map actuals ~f:(fun ((_, hist), _) -> hist) in
    ValueHistory.unknown_call reason actuals_hists call_loc timestamp
  in
  let ret_val = AbstractValue.mk_fresh () in
  (* record the [ReturnedFromUnknown] attribute from ret_v -> actuals for checking for modifications to copies *)
  let astate = add_returned_from_unknown callee_pname_opt ret_val actuals astate0 in
  let astate = PulseOperations.write_id (fst ret) (ret_val, hist) astate in
  let astate = Decompiler.add_call_source ret_val reason actuals astate in
  (* set to [false] if we think the procedure called does not behave "purely", i.e. return the same
     value for the same inputs *)
  let is_pure = ref true in
  let should_havoc actual_typ formal_typ_opt =
    match actual_typ.Typ.desc with
    | _ when not Config.pulse_havoc_arguments ->
        `DoNotHavoc
    | _ when Language.curr_language_is Erlang ->
        `DoNotHavoc
    | Tstruct (CppClass {name})
    | Tptr ({desc= Tstruct (CppClass {name})}, _)
    (* Sometimes the iterator is given to the next function as a reference. *)
      when QualifiedCppName.Match.match_qualifiers matches_iter name ->
        `ShouldHavoc
    | Tptr _ when Language.curr_language_is CIL ->
        `DoNotHavoc
    | Tptr _ when not (is_ptr_to_const formal_typ_opt) ->
        AbductiveDomain.should_havoc_if_unknown ()
    | _ ->
        `DoNotHavoc
  in
  let havoc_actual_if_ptr ((actual, _), actual_typ) formal_opt astate =
    let fold_on_reachable_from_arg astate f =
      let reachable_from_arg =
        AbductiveDomain.reachable_addresses_from (Seq.return actual) astate0 `Post
      in
      AbstractValue.Set.fold f reachable_from_arg astate
    in
    (* We should not havoc when the corresponding formal is a pointer to const *)
    match should_havoc actual_typ (Option.map ~f:snd formal_opt) with
    | `ShouldHavoc ->
        is_pure := false ;
        (* this will deallocate anything reachable from the [actual] and havoc the values pointed to
           by [actual] *)
        let astate =
          AbductiveDomain.apply_unknown_effect hist actual astate
          (* record the [UnknownEffect] attribute so callers of the current procedure can apply the
             above effects too in calling contexts where more is reachable from [actual] than here *)
          |> AddressAttributes.add_all actual (Attributes.singleton (UnknownEffect (reason, hist)))
        in
        if
          Option.exists callee_pname_opt ~f:(fun p ->
              Procname.is_constructor p
              || Option.exists (IRAttributes.load p) ~f:(fun attrs ->
                     attrs.ProcAttributes.is_cpp_copy_assignment )
              || Procname.is_destructor p )
        then astate
        else
          let is_const_version_available =
            Option.exists formal_opt ~f:(fun (formal, _) -> Pvar.is_this formal)
            && Option.exists callee_pname_opt ~f:(is_const_version_available tenv)
          in
          if is_const_version_available then astate
          else
            (* record the [WrittenTo] attribute for all reachable values
               starting from actual argument so that we don't assume
               that they are not modified in the unnecessary copy analysis. *)
            let call_trace = Trace.Immediate {location= call_loc; history= hist} in
            let written_attrs = Attributes.singleton (WrittenTo (timestamp, call_trace)) in
            fold_on_reachable_from_arg astate (fun reachable_actual acc ->
                if
                  Formula.is_known_non_pointer astate.AbductiveDomain.path_condition
                    reachable_actual
                then
                  (* not add [WrittenTo] for the non-pointer value, because primitive constant value
                     is immutable, i.e. cannot be modified. *)
                  acc
                else AddressAttributes.add_all reachable_actual written_attrs acc )
    | `DoNotHavoc ->
        astate
    | `ShouldOnlyHavocResources ->
        let astate =
          AddressAttributes.add_all actual
            (Attributes.singleton (UnknownEffect (reason, hist)))
            astate
        in
        let some_resource_found, astate =
          fold_on_reachable_from_arg (false, astate)
            (fun reachable_actual (some_resource_found, astate) ->
              let some_resource_found =
                some_resource_found
                || AddressAttributes.get_allocation_attr reachable_actual astate
                   |> Option.exists ~f:(fun (attr, _) ->
                          Attribute.is_hack_resource attr || Attribute.is_python_resource attr )
              in
              (some_resource_found, AddressAttributes.remove_allocation_attr reachable_actual astate) )
        in
        if some_resource_found then Stats.incr_pulse_unknown_calls_on_hack_resource () ;
        astate
  in
  let add_skipped_proc astate =
    let** astate, f =
      match reason with
      | Call _ | Model _ | ModelName _ ->
          Sat (Ok (astate, None))
      | SkippedKnownCall proc_name ->
          Sat (Ok (astate, Some (PulseFormula.Procname proc_name)))
      | SkippedUnknownCall e ->
          let++ astate, (v, _) = PulseOperations.eval path Read call_loc e astate in
          (astate, Some (PulseFormula.Unknown v))
    in
    let++ astate =
      match f with
      | Some f when !is_pure || force_pure ->
          let ret_val =
            match (Option.bind formals_opt ~f:List.last, List.last actuals) with
            | Some (pvar, _), Some ((return_param_val, _), _)
              when Mangled.is_return_param (Pvar.get_name pvar) ->
                return_param_val
            | _ ->
                ret_val
          in
          PulseArithmetic.and_equal (AbstractValueOperand ret_val)
            (FunctionApplicationOperand
               {f; actuals= List.map ~f:(fun ((actual_val, _hist), _typ) -> actual_val) actuals} )
            astate
      | _ ->
          Sat (Ok astate)
    in
    match reason with
    | SkippedKnownCall proc_name ->
        AbductiveDomain.add_skipped_call proc_name
          (Trace.Immediate {location= call_loc; history= ValueHistory.epoch})
          astate
    | _ ->
        astate
  in
  let havoc_actuals_without_typ_info astate =
    List.fold actuals ~init:astate ~f:(fun astate actual_typ ->
        havoc_actual_if_ptr actual_typ None astate )
  in
  L.d_printfln ~color:Orange "skipping unknown procedure %a" (Pp.option Procname.pp)
    callee_pname_opt ;
  ( match (actuals, formals_opt) with
  | actual_typ :: _, _ when Option.exists callee_pname_opt ~f:Procname.is_constructor ->
      (* when the callee is an unknown constructor, havoc the first arg (the constructed object)
         only *)
      let formal_opt = Option.bind formals_opt ~f:List.hd in
      havoc_actual_if_ptr actual_typ formal_opt astate
  | _, None ->
      havoc_actuals_without_typ_info astate
  | _, Some formals -> (
      let actuals = trim_actuals_if_var_arg callee_pname_opt ~actuals ~formals in
      match
        List.fold2 actuals formals ~init:astate ~f:(fun astate actual_typ formal ->
            havoc_actual_if_ptr actual_typ (Some formal) astate )
      with
      | Unequal_lengths ->
          print_arity_mismatch_message callee_pname_opt ~formals ~actuals ;
          havoc_actuals_without_typ_info astate
      | Ok result ->
          result ) )
  |> add_skipped_proc


let apply_callee ({InterproceduralAnalysis.tenv; proc_desc} as analysis_data)
    ({PathContext.timestamp} as path) callee_proc_name call_loc call_flags callee_exec_state ~ret
    ~captured_formals ~captured_actuals ~formals ~actuals astate =
  let open ExecutionDomain in
  let copy_to_caller_return_variable astate return_val_opt =
    (* Copies the return value of the callee into the return register of the caller.
        We use this function when the callee throws an exception.
        If the write_deref fails, or if the callee return value does not exist,
        we simply return the original abstract state unchanged. *)
    match return_val_opt with
    | Some return_val_hist ->
        let caller_return_var : Pvar.t = Procdesc.get_ret_var proc_desc in
        let (astate, caller_return_val_hist) : AbductiveDomain.t * (AbstractValue.t * ValueHistory.t)
            =
          PulseOperations.eval_var path call_loc caller_return_var astate
        in
        let+ (astate : AbductiveDomain.t) =
          L.d_printfln "called copy to caller return ref=%a, obj=%a" AbstractValue.pp
            (fst caller_return_val_hist) AbstractValue.pp (fst return_val_hist) ;
          PulseOperations.write_deref path call_loc ~ref:caller_return_val_hist ~obj:return_val_hist
            astate
        in
        ExceptionRaised astate
    | None ->
        Ok (ExceptionRaised astate)
  in
  let map_call_result callee_summary ~f =
    (* Clean up the summary before application to improve taint traces. When the calee is a taint
         sink itself with kinds K, we remove all MustNotBeTainted attributes matching K. The reason is
         if the callee itself calls other functions that are taint sinks with kind in K, the trace
         will keep going from callee to callee as long as the tainted value flows through the call
         chain. This leads to very long and confusing traces.

       TODO(arr): ideally, we'd scrub the summary of the callee once when we create a summary in
       AbductiveDomain rather than on every application. Unfortunately, as is this creates a
       dependency cycle between modules and should be dealt with separately. *)
    let callee_summary =
      if Config.pulse_taint_short_traces then
        let kinds =
          PulseTaintItemMatcher.procedure_matching_kinds tenv callee_proc_name None
            TaintConfig.sink_procedure_matchers
        in
        AbductiveDomain.Summary.remove_all_must_not_be_tainted ~kinds callee_summary
      else callee_summary
    in
    (* In order to apply summary specialisation, we call blocks or function pointers with the closure as the first argument,
       but when we want to call the actual code of the block or function, we need to remove the closure argument again. *)
    let actuals =
      if call_flags.CallFlags.cf_is_objc_block then
        match actuals with _ :: rest -> rest | [] -> []
      else actuals
    in
    let sat_unsat, contradiction =
      PulseInterproc.apply_summary analysis_data path ~callee_proc_name call_loc ~callee_summary
        ~captured_formals ~captured_actuals ~formals
        ~actuals:(trim_actuals_if_var_arg (Some callee_proc_name) ~actuals ~formals)
        astate
    in
    let sat_unsat =
      let** post, return_val_opt, subst, hist_map = sat_unsat in
      let post =
        match return_val_opt with
        | Some return_val_hist ->
            PulseOperations.write_id (fst ret) return_val_hist post
        | None ->
            PulseOperations.havoc_id (fst ret)
              (ValueHistory.singleton
                 (Call
                    { f= Call callee_proc_name
                    ; location= call_loc
                    ; in_call= ValueHistory.epoch
                    ; timestamp } ) )
              post
      in
      f return_val_opt (subst, hist_map) post
    in
    (sat_unsat, contradiction)
  in
  match callee_exec_state with
  | ContinueProgram astate ->
      map_call_result astate ~f:(fun _return_val_opt _subst astate ->
          Sat (Ok (ContinueProgram astate)) )
  | ExceptionRaised astate ->
      (* If the callee throws, then store the return value of the callee (the exception object)
         in the return variable of the caller (using [copy_to_caller_return_variable])
         ready to be accessed by the exception handler. *)
      map_call_result astate ~f:(fun return_val_opt _subst astate ->
          Sat (copy_to_caller_return_variable astate return_val_opt) )
  | InfiniteProgram astate
  | AbortProgram astate
  | ExitProgram astate
  | LatentAbortProgram {astate}
  | LatentSpecializedTypeIssue {astate}
  | LatentInvalidAccess {astate} ->
      map_call_result astate ~f:(fun _return_val_opt (subst, hist_map) astate_post_call ->
          let** astate_summary =
            let open SatUnsat.Import in
            AbductiveDomain.Summary.of_post
              (Procdesc.get_attributes proc_desc)
              call_loc astate_post_call
            >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
            >>| AccessResult.with_summary
          in
          match callee_exec_state with
          | ContinueProgram _ | ExceptionRaised _ ->
              assert false
          | AbortProgram _ | InfiniteProgram _ ->
              (* bypass the current errors to avoid compounding issues *)
              Sat (Ok (AbortProgram astate_summary))
          | ExitProgram _ ->
              Sat (Ok (ExitProgram astate_summary))
          | LatentAbortProgram {latent_issue} -> (
              let open SatUnsat.Import in
              let latent_issue =
                LatentIssue.add_call (Call callee_proc_name, call_loc) (subst, hist_map)
                  astate_post_call latent_issue
              in
              let diagnostic = LatentIssue.to_diagnostic latent_issue in
              match LatentIssue.should_report astate_summary diagnostic with
              | `DelayReport latent_issue ->
                  L.d_printfln ~color:Orange "issue is still latent, recording a LatentAbortProgram" ;
                  Sat (Ok (LatentAbortProgram {astate= astate_summary; latent_issue}))
              | `ReportNow ->
                  L.d_printfln ~color:Red "issue is now manifest, emitting an error" ;
                  Sat
                    (AccessResult.of_error_f path
                       (WithSummary
                          (ReportableError {diagnostic; astate= astate_post_call}, astate_summary)
                       )
                       ~f:(fun _ ->
                         L.die InternalError
                           "LatentAbortProgram cannot be applied to non-fatal errors" ) ) )
          | LatentSpecializedTypeIssue {specialized_type; trace} ->
              let trace =
                Trace.ViaCall
                  { f= Call callee_proc_name
                  ; location= call_loc
                  ; history= ValueHistory.epoch
                  ; in_call= trace }
              in
              (* The decision to report or further propagate the issue is done
                 at summary creation time based on the current specialization *)
              Sat (Ok (LatentSpecializedTypeIssue {astate= astate_summary; specialized_type; trace}))
          | LatentInvalidAccess
              { address= address_callee
              ; must_be_valid= callee_access_trace, must_be_valid_reason
              ; calling_context } -> (
            match
              let open IOption.Let_syntax in
              let* addr = DecompilerExpr.abstract_value_of_expr address_callee in
              AbstractValue.Map.find_opt addr subst
            with
            | None ->
                (* the address became unreachable so the bug can never be reached; drop it *)
                let reason () =
                  F.asprintf
                    "%a seems no longer reachable, dropping the latent invalid access altogether"
                    DecompilerExpr.pp address_callee
                in
                Unsat {reason; source= __POS__}
            | Some (invalid_address, default_caller_history) -> (
                let access_trace =
                  Trace.add_call (Call callee_proc_name) call_loc hist_map ~default_caller_history
                    callee_access_trace
                in
                let calling_context =
                  (CallEvent.Call callee_proc_name, call_loc) :: calling_context
                in
                match
                  AbductiveDomain.find_post_cell_opt invalid_address astate_post_call
                  |> Option.bind ~f:(fun (_, attrs) -> Attributes.get_invalid attrs)
                with
                | None ->
                    (* still no proof that the address is invalid *)
                    let address_caller = Decompiler.find invalid_address astate_post_call in
                    L.d_printfln ~color:Orange
                      "%a in the callee is %a in the caller which is not known to be invalid, \
                       keeping the latent invalid access"
                      DecompilerExpr.pp address_callee DecompilerExpr.pp address_caller ;
                    Sat
                      (Ok
                         (LatentInvalidAccess
                            { astate= astate_summary
                            ; address= address_caller
                            ; must_be_valid= (access_trace, must_be_valid_reason)
                            ; calling_context } ) )
                | Some (invalidation, invalidation_trace) ->
                    let address_caller = Decompiler.find invalid_address astate_post_call in
                    L.d_printfln ~color:Red
                      "%a in the callee is %a in the caller which invalid, reporting the latent \
                       invalid access as manifest"
                      DecompilerExpr.pp address_callee DecompilerExpr.pp address_caller ;
                    Sat
                      (FatalError
                         ( WithSummary
                             ( ReportableError
                                 { diagnostic=
                                     AccessToInvalidAddress
                                       { calling_context
                                       ; invalid_address= address_caller
                                       ; invalidation
                                       ; invalidation_trace
                                       ; access_trace
                                       ; must_be_valid_reason }
                                 ; astate= astate_post_call }
                             , astate_summary )
                         , [] ) ) ) ) )


let call_aux disjunct_limit ({InterproceduralAnalysis.tenv} as analysis_data) path call_loc
    callee_pname ret actuals call_kind call_flags (callee_proc_attrs : ProcAttributes.t)
    exec_states_callee non_disj_callee (astate_caller : AbductiveDomain.t) non_disj_caller =
  let formals =
    List.map callee_proc_attrs.formals ~f:(fun (mangled, typ, _) ->
        (Pvar.mk mangled callee_pname, typ) )
  in
  let captured_formals = callee_proc_attrs.captured in
  let ( let<**> ) = bind_sat_result (non_disj_caller, None) in
  let<**> astate, captured_actuals =
    PulseOperations.get_captured_actuals callee_pname path call_loc call_kind ~captured_formals
      ~actuals astate_caller
  in
  let captured_formals =
    List.map captured_formals ~f:(fun {CapturedVar.pvar; typ} -> (pvar, typ))
  in
  let should_keep_at_most_one_disjunct =
    Option.exists Config.pulse_cut_to_one_path_procedures_pattern ~f:(fun regex ->
        Str.string_match regex (Procname.to_string callee_pname) 0 )
  in
  if should_keep_at_most_one_disjunct then
    L.d_printfln "Will keep at most one disjunct because %a is in block list" Procname.pp
      callee_pname ;
  (* we propagate transitive accesses from callee to caller using *)
  let skip_transitive_accesses = PulseTransitiveAccessChecker.should_skip_call tenv callee_pname in
  let non_disj =
    NonDisjDomain.apply_summary ~callee_pname ~call_loc ~skip_transitive_accesses non_disj_caller
      non_disj_callee
  in
  (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary, and to the
     over-approximate pre/post if we are currently executing the over-approximate part of the
     current state *)
  let exec_states_callee =
    if path.PathContext.is_non_disj then
      match NonDisjDomain.Summary.get_pre_post non_disj_callee with
      | Bottom ->
          exec_states_callee
      | NonBottom astate_over_approx ->
          exec_states_callee @ [ContinueProgram astate_over_approx]
    else exec_states_callee
  in
  let posts, contradiction =
    List.fold ~init:([], None) exec_states_callee
      ~f:(fun (posts, contradiction) callee_exec_state ->
        if
          (not path.PathContext.is_non_disj)
          && ( Option.exists disjunct_limit ~f:(fun limit -> List.length posts >= limit)
             || (should_keep_at_most_one_disjunct && not (List.is_empty posts)) )
        then (
          L.d_printfln "disjunct limit reached, stop applying callee pre/posts" ;
          (posts, contradiction) )
        else (
          (* apply one pre/post spec, check for timeouts in-between each pre/post spec from the callee
              *)
          Timer.check_timeout () ;
          match
            apply_callee analysis_data path callee_pname call_loc call_flags callee_exec_state
              ~captured_formals ~captured_actuals ~formals ~actuals ~ret astate
          with
          | Unsat unsat_info, new_contradiction ->
              SatUnsat.log_unsat unsat_info ;
              (* couldn't apply pre/post pair *)
              (posts, PulseInterproc.merge_contradictions contradiction new_contradiction)
          | Sat post, new_contradiction ->
              (post :: posts, PulseInterproc.merge_contradictions contradiction new_contradiction) ) )
  in
  (* We always apply the non-disj part of the summary from the current state, even if it we are
     inside a disjunct and not the non-disj part of the current state. We then put the result
     directly into the non-disjunctive part of the state, which will be joined with the results from
     other disjuncts and whatever over-approximate state was already there. *)
  let non_disj =
    match NonDisjDomain.Summary.get_pre_post non_disj_callee with
    | NonBottom callee_astate_over_approx
      when not path.PathContext.is_non_disj (* already executed above in this case *) ->
        let (non_disj_caller : _ AbstractDomain.Types.bottom_lifted) =
          match
            apply_callee analysis_data path callee_pname call_loc call_flags
              (ContinueProgram callee_astate_over_approx) ~captured_formals ~captured_actuals
              ~formals ~actuals ~ret astate
          with
          | Sat (Ok (ContinueProgram post)), _new_contradiction ->
              NonBottom (post, path)
          | Sat (Recoverable (ContinueProgram post, _)), _new_contradiction ->
              (* TODO: report errors *)
              NonBottom (post, path)
          | Sat (FatalError _), _ ->
              (* TODO: report errors, recover after errors *)
              Bottom
          | _, _new_contradiction ->
              Bottom
        in
        NonDisjDomain.join_to_astate non_disj_caller non_disj
    | _ ->
        non_disj
  in
  (posts, (non_disj, contradiction))


let call_aux_unknown limit ({InterproceduralAnalysis.tenv} as analysis_data) path call_loc
    callee_pname ~ret ~actuals ~formals_opt call_kind call_flags (astate : AbductiveDomain.t)
    non_disj_caller =
  let arg_values = List.map actuals ~f:(fun ((value, _), _) -> value) in
  let ( let<**> ) = bind_sat_result (non_disj_caller, None) in
  let<**> astate_unknown =
    PulseOperations.conservatively_initialize_args arg_values astate
    |> unknown_call tenv path call_loc (SkippedKnownCall callee_pname) (Some callee_pname) ~ret
         ~actuals ~formals_opt
  in
  if Config.pulse_log_unknown_calls then
    StatsLogging.log_message_with_location ~label:"unmodeled_function_operation_pulse"
      ~loc:(F.asprintf "%a" Location.pp_file_pos call_loc)
      ~message:
        (Format.asprintf "Unmodeled Function[Pulse] : %a" Procname.pp_without_templates callee_pname) ;
  Option.iter Config.pulse_log_unknown_calls_sampled ~f:(fun sample_rate ->
      StatsLogging.log_message_with_location_sampled
        ~label:(lazy "unmodeled_function_operation_pulse")
        ~loc:(lazy (F.asprintf "%a" Location.pp_file_pos call_loc))
        ~message:
          ( lazy
            (Format.asprintf "Unmodeled Function[Pulse] : %a" Procname.pp_without_templates
               callee_pname ) )
        ~sample_rate ) ;
  if Procname.is_objc_instance_method callee_pname then
    (* a special case for objc nil messaging *)
    let unknown_objc_nil_messaging astate_unknown proc_name proc_attrs =
      let result_unknown, non_disj =
        ( (let<++> astate_unknown =
             L.d_printfln "Appending positive self to state" ;
             PulseSummary.append_objc_actual_self_positive proc_name proc_attrs (List.hd actuals)
               astate_unknown
           in
           astate_unknown )
        , non_disj_caller )
      in
      L.d_printfln "@\nMaking and applying Objective-C nil messaging summary@\n" ;
      let result_unknown_nil, (non_disj_nil, contradiction) =
        PulseSummary.mk_objc_nil_messaging_summary tenv proc_attrs
        |> Option.value_map
             ~default:([], (NonDisjDomain.bottom, None))
             ~f:(fun nil_summary ->
               call_aux limit analysis_data path call_loc callee_pname ret actuals call_kind
                 call_flags proc_attrs [nil_summary] NonDisjDomain.Summary.bottom astate
                 non_disj_caller )
      in
      ( result_unknown @ result_unknown_nil
      , (NonDisjDomain.join non_disj non_disj_nil, contradiction) )
    in
    IRAttributes.load callee_pname
    |> Option.value_map
         ~default:([Ok (ContinueProgram astate_unknown)], (non_disj_caller, None))
         ~f:(unknown_objc_nil_messaging astate_unknown callee_pname)
  else ([Ok (ContinueProgram astate_unknown)], (non_disj_caller, None))


let add_need_dynamic_type_specialization needs execution_states =
  let update_astate astate =
    AbstractValue.Set.fold
      (fun addr astate -> AbductiveDomain.add_need_dynamic_type_specialization addr astate)
      needs astate
  in
  let update_summary summary =
    AbstractValue.Set.fold
      (fun addr summary -> AbductiveDomain.Summary.add_need_dynamic_type_specialization addr summary)
      needs summary
  in
  List.map execution_states
    ~f:
      (PulseResult.map ~f:(function
        | ExecutionDomain.ExceptionRaised astate ->
            ExceptionRaised (update_astate astate)
        | ContinueProgram astate ->
           ContinueProgram (update_astate astate)
        | InfiniteProgram astate ->
           InfiniteProgram (update_astate astate)
        | ExitProgram summary ->
            ExitProgram (update_summary summary)
        | AbortProgram summary ->
            AbortProgram (update_summary summary)
        | LatentAbortProgram latent_abort_program ->
            let astate = update_summary latent_abort_program.astate in
            LatentAbortProgram {latent_abort_program with astate}
        | LatentInvalidAccess latent_invalid_access ->
            let astate = update_summary latent_invalid_access.astate in
            LatentInvalidAccess {latent_invalid_access with astate}
        | LatentSpecializedTypeIssue latent_specialized_type_issue ->
            let astate = update_summary latent_specialized_type_issue.astate in
            LatentSpecializedTypeIssue {latent_specialized_type_issue with astate} ) )


let maybe_dynamic_type_specialization_is_needed already_specialized contradiction astate =
  let make_dynamic_type_specialization =
    (* we try to find a dynamic type name for each pvar in the [pvars] set in order to devirtualize all
       calls in the callee (and its own callees). If we do not find a dynamic type, we record the
       address in a set [need_specialization_from_caller] in order to propagate the need to the caller
       state *)
    let open IOption.Let_syntax in
    if Option.is_none contradiction then L.d_printfln "no dynamic type specialization needed" ;
    let* contradiction in
    let+ needed_heap_paths = PulseInterproc.is_dynamic_type_needed_contradiction contradiction in
    Specialization.HeapPath.Map.fold
      (fun heap_path addr (dyntypes_map, need_specialization_from_caller) ->
        let ( let** ) opt f =
          Option.value_map opt ~f
            ~default:(dyntypes_map, AbstractValue.Set.add addr need_specialization_from_caller)
        in
        let** dynamic_type_data = PulseArithmetic.get_dynamic_type addr astate in
        let** dynamic_type_name = Typ.name dynamic_type_data.Formula.typ in
        let dyntypes_map =
          if Specialization.HeapPath.Map.mem heap_path already_specialized then dyntypes_map
          else Specialization.HeapPath.Map.add heap_path dynamic_type_name dyntypes_map
        in
        (dyntypes_map, need_specialization_from_caller) )
      needed_heap_paths
      (Specialization.HeapPath.Map.empty, AbstractValue.Set.empty)
  in
  match make_dynamic_type_specialization with
  | Some (dyntypes_map, need_specialization_from_caller)
    when (not (AbstractValue.Set.is_empty need_specialization_from_caller))
         || not (Specialization.HeapPath.Map.is_empty dyntypes_map) ->
      let dyntypes_map =
        Specialization.HeapPath.Map.fold
          (fun heap_path type_name dyntypes_map ->
            Specialization.HeapPath.Map.add heap_path type_name dyntypes_map )
          already_specialized dyntypes_map
      in
      `NeedSpecialization (dyntypes_map, need_specialization_from_caller)
  | _ ->
      `UseCurrentSummary


let on_recursive_call ({InterproceduralAnalysis.proc_desc} as analysis_data) call_loc
    (call_flags : CallFlags.t) callee_pname ~actuals ~formals_opt astate =
  let actuals_values = List.map actuals ~f:(fun ((actual, _), _) -> actual) in
  if Procname.equal callee_pname (Procdesc.get_proc_name proc_desc) then (
    ( match formals_opt with
    | Some formals when List.length formals <> List.length actuals ->
        print_arity_mismatch_message ~extra_call_prefix:" recursive" (Some callee_pname) ~formals
          ~actuals
    | _ when Procname.is_hack_xinit callee_pname ->
        L.d_printfln "Suppressing recursive call report for non-user-visible function %a"
          Procname.pp callee_pname
    | _ when call_flags.cf_is_objc_getter_setter ->
        (* objc custom getters and setters generate recursive calls to themselves in the clang AST
           that end up here too; discard any possible cycle coming from them *)
        L.d_printfln "Suppressing recursive call to ObjC getter/setter %a" Procname.pp callee_pname
    | _ ->
        if AbductiveDomain.has_reachable_in_inner_pre_heap actuals_values astate then
          L.d_printfln
            "heap progress made before recursive call to %a; unlikely to be an infinite recursion, \
             suppressing report"
            Procname.pp callee_pname
        else
          let is_call_with_same_values =
            AbductiveDomain.are_same_values_as_pre_formals proc_desc actuals_values astate
          in
          PulseReport.report analysis_data ~is_suppressed:false ~latent:false
            (MutualRecursionCycle
               { cycle= PulseMutualRecursion.mk call_loc callee_pname actuals_values
               ; location= call_loc
               ; is_call_with_same_values } ) ) ;
    astate )
  else if
    AbductiveDomain.has_reachable_in_inner_pre_heap
      (List.map actuals ~f:(fun ((actual, _), _) -> actual))
      astate
  then (
    L.d_printfln
      "heap progress made before recursive call to %a; unlikely to be an infinite recursion, not \
       recording the cycle"
      Procname.pp callee_pname ;
    astate )
  else AbductiveDomain.add_recursive_call call_loc callee_pname actuals_values astate


let check_uninit_method ({InterproceduralAnalysis.tenv} as analysis_data) call_loc callee_pname
    actuals astate =
  let is_this v =
    (* We ignore the method call of `static::foo`, since it can be safe depending on call context.
       When `static::` is resolved, the first actual is `$this` in general. *)
    match Decompiler.find v astate with
    | SourceExpr ((PVar pvar, [Dereference]), _) ->
        Pvar.is_this pvar
    | _ ->
        false
  in
  let is_type_constant typ =
    (* TODO: For now, the checker handles type constant only, which means it cannot do
       inter-procedural reasoning where type variable is used.  We will work on this soon. *)
    (not (Typ.Name.Hack.is_HH_classname typ))
    && Tenv.mem_supers tenv typ ~f:(fun typ _ -> Typ.Name.Hack.is_HH_classname typ)
  in
  let has_method typ =
    match Tenv.lookup tenv typ with
    | None | Some {Struct.dummy= true} ->
        (* When unknown, we do not report the issue with assuming there is a method initialized. *)
        true
    | Some {Struct.methods} ->
        let callee_name = Procname.get_method callee_pname in
        List.exists methods ~f:(fun method_name ->
            String.equal callee_name (Procname.get_method method_name) )
  in
  let skip_special_pname pname =
    not (Procname.is_hack_construct pname || Procname.is_hack_xinit pname)
  in
  if skip_special_pname callee_pname then
    match actuals with
    | ((first_actual, history), _) :: _ when not (is_this first_actual) ->
        Option.iter (Procname.get_class_type_name callee_pname) ~f:(fun typ ->
            if is_type_constant typ && not (has_method typ) then
              PulseReport.report analysis_data ~is_suppressed:false ~latent:false
                (UninitMethod {callee= callee_pname; history; location= call_loc}) )
    | _ ->
        ()


let call ?disjunct_limit ({InterproceduralAnalysis.analyze_dependency} as analysis_data) path
    call_loc ?unresolved_reason callee_pname ~ret ~actuals ~formals_opt call_kind call_flags
    (astate : AbductiveDomain.t) non_disj_caller =
  let has_continue_program results =
    let f one_result =
      match one_result with
      | Ok (ContinueProgram _astate) | Recoverable (ContinueProgram _astate, _) ->
          true
      | _ ->
          false
    in
    List.exists results ~f
  in
  let call_as_unknown () =
    let results, (non_disj, contradiction) =
      call_aux_unknown disjunct_limit analysis_data path call_loc callee_pname ~ret ~actuals
        ~formals_opt call_kind call_flags astate non_disj_caller
    in
    if Option.is_some contradiction then (
      L.debug Analysis Verbose
        "@[<v>@[Failed to honor --pulse-force-continue, because attempting to treat the procedure \
         as unknown caused a contradiction: %a@]@;\
         @]"
        Procname.pp callee_pname ;
      ([], non_disj) )
    else (results, non_disj)
  in
  let call_specialized specialization
      {PulseSummary.pre_post_list= exec_states; non_disj= non_disj_callee} astate =
    let results, (non_disj, contradiction) =
      call_aux disjunct_limit analysis_data path call_loc callee_pname ret actuals call_kind
        call_flags
        (IRAttributes.load_exn callee_pname)
        exec_states non_disj_callee astate non_disj_caller
    in
    let non_disj =
      NonDisjDomain.add_specialized_direct_callee callee_pname specialization call_loc non_disj
    in
    (results, non_disj, contradiction)
  in
  let rec iter_call ~max_iteration ~nth_iteration ~is_pulse_specialization_limit_not_reached
      ?(specialization = Specialization.Pulse.bottom) already_given summary astate =
    let res, non_disj, contradiction = call_specialized specialization summary astate in
    let needs_aliasing_specialization =
      match (res, contradiction) with
      | [], Some (Aliasing _) ->
          `AliasSpecializationImpossible
      | [], Some (AliasingWithAllAliases aliases) ->
          `AliasSpeciationWith aliases
      | _, _ ->
          `NoAliasSpecializationRequired
    in
    let request_specialization specialization =
      let open IResult.Let_syntax in
      let+ (specialized_summary : PulseSummary.t) =
        analyze_dependency ~specialization:(Pulse specialization) callee_pname
      in
      let is_limit_not_reached =
        Specialization.Pulse.is_pulse_specialization_limit_not_reached
          specialized_summary.specialized
      in
      match Specialization.Pulse.Map.find_opt specialization specialized_summary.specialized with
      | None ->
          L.internal_error "ondemand engine did not return the expected specialized summary@\n" ;
          (* we use the non-specialized summary instead *)
          (specialized_summary.main, is_limit_not_reached)
      | Some pre_posts ->
          (pre_posts, is_limit_not_reached)
    in
    let case_if_specialization_is_impossible res =
      ( res
      , summary
      , non_disj
      , contradiction
      , match needs_aliasing_specialization with
        | `AliasSpecializationImpossible | `AliasSpeciationWith _ ->
            `UnknownCall
        | `NoAliasSpecializationRequired ->
            `KnownCall )
    in
    if not is_pulse_specialization_limit_not_reached then case_if_specialization_is_impossible res
    else
      let more_specialization, ask_caller_of_caller_first, needs_from_caller =
        match needs_aliasing_specialization with
        | `AliasSpecializationImpossible ->
            L.d_printfln "Alias specialization of %a failed" Procname.pp callee_pname ;
            (`NoMoreSpecialization, false, AbstractValue.Set.empty)
        | `AliasSpeciationWith alias_specialization ->
            let specialization =
              {specialization with Specialization.Pulse.aliases= Some alias_specialization}
            in
            L.d_printfln "requesting alias specialization %a" Specialization.Pulse.pp specialization ;
            (`MoreSpecialization specialization, false, AbstractValue.Set.empty)
        | `NoAliasSpecializationRequired ->
            let already_specialized = specialization.Specialization.Pulse.dynamic_types in
            L.with_indent ~collapsible:true "checking dynamic type specialization" ~f:(fun () ->
                match
                  maybe_dynamic_type_specialization_is_needed already_specialized contradiction
                    astate
                with
                | `NeedSpecialization (dyntypes_map, needs_from_caller) ->
                    let specialization_is_fully_satisfied =
                      AbstractValue.Set.is_empty needs_from_caller
                    in
                    if not specialization_is_fully_satisfied then
                      L.d_printfln
                        "[specialization] not enough dyntypes information in the caller context. \
                         Missing = %a"
                        AbstractValue.Set.pp needs_from_caller ;
                    let specialization =
                      {specialization with Specialization.Pulse.dynamic_types= dyntypes_map}
                    in
                    ( `MoreSpecialization specialization
                    , (not specialization_is_fully_satisfied)
                      && not Config.pulse_specialization_partial
                    , needs_from_caller )
                | `UseCurrentSummary ->
                    L.d_printfln "abort, using current summary" ;
                    (`NoMoreSpecialization, false, AbstractValue.Set.empty) )
      in
      match more_specialization with
      | `NoMoreSpecialization ->
          (res, summary, non_disj, contradiction, `KnownCall)
      | `MoreSpecialization specialization
        when Specialization.Pulse.is_bottom specialization
             && AbstractValue.Set.is_empty needs_from_caller ->
          (res, summary, non_disj, contradiction, `KnownCall)
      | `MoreSpecialization specialization ->
          let has_already_be_given = Specialization.Pulse.Set.mem specialization already_given in
          if has_already_be_given then
            L.d_printfln "[specialization] we have already query the callee with specialization %a"
              Specialization.Pulse.pp specialization ;
          if nth_iteration >= max_iteration then
            L.d_printfln "[specialization] we have reached the maximum number of iteration" ;
          if
            nth_iteration >= max_iteration || has_already_be_given || ask_caller_of_caller_first
            || Specialization.Pulse.is_bottom specialization
          then
            case_if_specialization_is_impossible
              (add_need_dynamic_type_specialization needs_from_caller res)
          else (
            L.d_printfln "requesting specialized analysis using %sspecialization %a"
              (if not (AbstractValue.Set.is_empty needs_from_caller) then "partial " else "")
              Specialization.Pulse.pp specialization ;
            match request_specialization specialization with
            | Error MutualRecursionCycle ->
                let res =
                  List.map res ~f:(function result ->
                      (let+ exec_state = result in
                       match exec_state with
                       | ContinueProgram astate ->
                           ContinueProgram
                             (on_recursive_call analysis_data call_loc call_flags callee_pname
                                ~actuals ~formals_opt astate )
                       | exec_state ->
                           exec_state ) )
                in
                case_if_specialization_is_impossible res
            | Error (AnalysisFailed | InBlockList | UnknownProcedure) ->
                case_if_specialization_is_impossible res
            | Ok (summary, is_pulse_specialization_limit_not_reached) ->
                let already_given = Specialization.Pulse.Set.add specialization already_given in
                iter_call ~max_iteration ~nth_iteration:(nth_iteration + 1)
                  ~is_pulse_specialization_limit_not_reached ~specialization already_given summary
                  astate )
  in
  match analyze_dependency callee_pname with
  | Ok summary ->
      let is_pulse_specialization_limit_not_reached =
        Specialization.Pulse.is_pulse_specialization_limit_not_reached
          summary.PulseSummary.specialized
      in
      let max_iteration = Config.pulse_specialization_iteration_limit in
      let already_given = Specialization.Pulse.Set.empty in
      let res, summary_used, non_disj, contradiction, resolution_status =
        iter_call ~max_iteration ~nth_iteration:0 ~is_pulse_specialization_limit_not_reached
          already_given summary.PulseSummary.main astate
      in
      let has_continue_program =
        has_continue_program res || not (NonDisjDomain.astate_is_bottom non_disj)
      in
      if has_continue_program then GlobalForStats.node_is_not_stuck () ;
      let res, non_disj =
        if
          Config.pulse_force_continue
          && ( PulseNonDisjunctiveDomain.Summary.has_dropped_disjuncts summary_used.non_disj
             || List.is_empty summary_used.pre_post_list )
          && not has_continue_program
        then (
          (* When a function call does not have a post of type ContinueProgram, we may want to treat
             the call as unknown to make the analysis continue. This may introduce false positives but
             could uncover additional true positives too. *)
          L.d_printfln_escaped ~color:Orange
            "No disjuncts of type ContinueProgram, treating the call to %a as unknown" Procname.pp
            callee_pname ;
          let unknown_results, non_disj = call_as_unknown () in
          (unknown_results @ res, non_disj) )
        else (res, non_disj)
      in
      (res, non_disj, contradiction, resolution_status)
  | Error no_summary ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln_escaped "No spec found for %a: %a@\n" Procname.pp callee_pname
        AnalysisResult.pp_no_summary no_summary ;
      let astate =
        match no_summary with
        | MutualRecursionCycle ->
            on_recursive_call analysis_data call_loc call_flags callee_pname ~actuals ~formals_opt
              astate
        | AnalysisFailed | InBlockList | UnknownProcedure ->
            astate
      in
      ( match (unresolved_reason : Tenv.unresolved_reason option) with
      | Some
          ( ClassNameNotFound
          | CurryInfoNotFound
          | MaybeMissingDueToMissedCapture
          | MaybeMissingDueToIncompleteModel ) ->
          ()
      | None ->
          check_uninit_method analysis_data call_loc callee_pname actuals astate ) ;
      let res, (non_disj, contradiction) =
        call_aux_unknown disjunct_limit analysis_data path call_loc callee_pname ~ret ~actuals
          ~formals_opt call_kind call_flags astate non_disj_caller
      in
      (res, non_disj, contradiction, `UnknownCall)
