(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

type t = AbductiveDomain.t

let is_ptr_to_const formal_typ_opt =
  Option.exists formal_typ_opt ~f:(fun (formal_typ : Typ.t) ->
      match formal_typ.desc with Typ.Tptr (t, _) -> Typ.is_const t.quals | _ -> false )


let unknown_call ({PathContext.timestamp} as path) call_loc (reason : CallEvent.t) callee_pname_opt
    ~ret ~actuals ~formals_opt ({AbductiveDomain.post} as astate) =
  let hist =
    ValueHistory.singleton
      (Call {f= reason; location= call_loc; in_call= ValueHistory.epoch; timestamp})
  in
  let ret_val = AbstractValue.mk_fresh () in
  let astate = PulseOperations.write_id (fst ret) (ret_val, hist) astate in
  let astate = Decompiler.add_call_source ret_val reason actuals astate in
  (* set to [false] if we think the procedure called does not behave "functionally", i.e. return the
     same value for the same inputs *)
  let is_functional = ref true in
  let should_havoc actual_typ formal_typ_opt =
    let matches_iter =
      QualifiedCppName.Match.of_fuzzy_qual_names ["std::__wrap_iter"; "__gnu_cxx::__normal_iterator"]
    in
    match actual_typ.Typ.desc with
    | Typ.Tstruct (Typ.CppClass {name})
      when QualifiedCppName.Match.match_qualifiers matches_iter name ->
        `ShouldHavoc
    | Tptr _ when not (is_ptr_to_const formal_typ_opt) ->
        AbductiveDomain.should_havoc_if_unknown ()
    | _ ->
        `DoNotHavoc
  in
  let havoc_actual_if_ptr ((actual, _), actual_typ) formal_typ_opt astate =
    let fold_on_reachable_from_arg astate f =
      let reachable_from_arg =
        BaseDomain.reachable_addresses_from (Caml.List.to_seq [actual]) (post :> BaseDomain.t)
      in
      AbstractValue.Set.fold f reachable_from_arg astate
    in
    (* We should not havoc when the corresponding formal is a pointer to const *)
    match should_havoc actual_typ formal_typ_opt with
    | `ShouldHavoc ->
        is_functional := false ;
        (* this will deallocate anything reachable from the [actual] and havoc the values pointed to
           by [actual] *)
        let astate =
          AbductiveDomain.apply_unknown_effect hist actual astate
          (* record the [UnknownEffect] attribute so callers of the current procedure can apply the
             above effects too in calling contexts where more is reachable from [actual] than here *)
          |> AddressAttributes.add_attrs actual
               (Attributes.singleton (UnknownEffect (reason, hist)))
        in
        if
          Option.exists callee_pname_opt ~f:(fun p ->
              Procname.is_constructor p || Procname.is_copy_assignment p )
        then astate
        else
          (* record the [WrittenTo] attribute for all reachable values
             starting from actual argument so that we don't assume
             that they are not modified in the unnecessary copy analysis. *)
          let call_trace = Trace.Immediate {location= call_loc; history= hist} in
          let written_attrs = Attributes.singleton (WrittenTo call_trace) in
          fold_on_reachable_from_arg astate (fun reachable_actual ->
              AddressAttributes.add_attrs reachable_actual written_attrs )
    | `DoNotHavoc ->
        astate
    | `ShouldOnlyHavocResources ->
        let astate =
          AddressAttributes.add_attrs actual
            (Attributes.singleton (UnknownEffect (reason, hist)))
            astate
        in
        fold_on_reachable_from_arg astate (fun reachable_actual ->
            AddressAttributes.remove_allocation_attr reachable_actual )
  in
  let add_skipped_proc astate =
    let* astate, f =
      match reason with
      | Call _ | Model _ ->
          Ok (astate, None)
      | SkippedKnownCall proc_name ->
          Ok (astate, Some (PulseFormula.Procname proc_name))
      | SkippedUnknownCall e ->
          let+ astate, (v, _) = PulseOperations.eval path Read call_loc e astate in
          (astate, Some (PulseFormula.Unknown v))
    in
    let+ astate =
      match f with
      | Some f when !is_functional ->
          PulseArithmetic.and_equal (AbstractValueOperand ret_val)
            (FunctionApplicationOperand
               {f; actuals= List.map ~f:(fun ((actual_val, _hist), _typ) -> actual_val) actuals} )
            astate
      | _ ->
          Ok astate
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
  L.d_printfln "skipping unknown procedure" ;
  ( match formals_opt with
  | None ->
      havoc_actuals_without_typ_info astate
  | Some formals -> (
    match
      List.fold2 actuals formals ~init:astate ~f:(fun astate actual_typ (_, formal_typ) ->
          havoc_actual_if_ptr actual_typ (Some formal_typ) astate )
    with
    | Unequal_lengths ->
        L.d_printfln "ERROR: formals have length %d but actuals have length %d"
          (List.length formals) (List.length actuals) ;
        havoc_actuals_without_typ_info astate
    | Ok result ->
        result ) )
  |> add_skipped_proc


let apply_callee tenv ({PathContext.timestamp} as path) ~caller_proc_desc callee_pname call_loc
    callee_exec_state ~ret ~captured_formals ~captured_actuals ~formals ~actuals astate =
  let open ExecutionDomain in
  let ( let* ) x f =
    SatUnsat.bind
      (fun result ->
        PulseResult.map result ~f |> PulseResult.map ~f:SatUnsat.sat |> PulseResult.of_some
        |> SatUnsat.of_option |> SatUnsat.map PulseResult.join )
      x
  in
  let map_call_result ~is_isl_error_prepost callee_prepost ~f =
    let sat_unsat, contradiction =
      PulseInterproc.apply_prepost path ~is_isl_error_prepost callee_pname call_loc ~callee_prepost
        ~captured_formals ~captured_actuals ~formals ~actuals astate
    in
    let sat_unsat =
      let* post, return_val_opt, subst = sat_unsat in
      let post =
        match return_val_opt with
        | Some return_val_hist ->
            PulseOperations.write_id (fst ret) return_val_hist post
        | None ->
            PulseOperations.havoc_id (fst ret)
              (ValueHistory.singleton
                 (Call
                    { f= Call callee_pname
                    ; location= call_loc
                    ; in_call= ValueHistory.epoch
                    ; timestamp } ) )
              post
      in
      f subst post
    in
    (sat_unsat, contradiction)
  in
  match callee_exec_state with
  | ContinueProgram astate ->
      map_call_result ~is_isl_error_prepost:false astate ~f:(fun _subst astate ->
          Sat (Ok (ContinueProgram astate)) )
  | ExceptionRaised astate ->
      map_call_result ~is_isl_error_prepost:false astate ~f:(fun _subst astate ->
          Sat (Ok (ExceptionRaised astate)) )
  | AbortProgram astate
  | ExitProgram astate
  | LatentAbortProgram {astate}
  | LatentInvalidAccess {astate} ->
      map_call_result ~is_isl_error_prepost:false
        (astate :> AbductiveDomain.t)
        ~f:(fun subst astate_post_call ->
          let* (astate_summary : AbductiveDomain.summary) =
            let open SatUnsat.Import in
            AbductiveDomain.summary_of_post tenv
              (Procdesc.get_proc_name caller_proc_desc)
              (Procdesc.get_attributes caller_proc_desc)
              call_loc astate_post_call
            >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
            >>| AccessResult.of_summary
          in
          match callee_exec_state with
          | ContinueProgram _ | ExceptionRaised _ | ISLLatentMemoryError _ ->
              assert false
          | AbortProgram _ ->
              (* bypass the current errors to avoid compounding issues *)
              Sat (Ok (AbortProgram astate_summary))
          | ExitProgram _ ->
              Sat (Ok (ExitProgram astate_summary))
          | LatentAbortProgram {latent_issue} -> (
              let open SatUnsat.Import in
              let latent_issue =
                LatentIssue.add_call (Call callee_pname, call_loc) subst astate_post_call
                  latent_issue
              in
              let diagnostic = LatentIssue.to_diagnostic latent_issue in
              match LatentIssue.should_report astate_summary diagnostic with
              | `DelayReport latent_issue ->
                  L.d_printfln ~color:Orange "issue is still latent, recording a LatentAbortProgram" ;
                  Sat (Ok (LatentAbortProgram {astate= astate_summary; latent_issue}))
              | `ReportNow ->
                  L.d_printfln ~color:Red "issue is now manifest, emitting an error" ;
                  Sat
                    (AccessResult.of_error_f
                       (Summary (ReportableErrorSummary {diagnostic; astate= astate_summary}))
                       ~f:(fun _ ->
                         L.die InternalError
                           "LatentAbortProgram cannot be applied to non-fatal errors" ) )
              | `ISLDelay astate ->
                  Sat (FatalError (Summary (ISLErrorSummary {astate}), [])) )
          | LatentInvalidAccess
              { address= address_callee
              ; must_be_valid= callee_access_trace, must_be_valid_reason
              ; calling_context } -> (
            match
              let open IOption.Let_syntax in
              let* addr = Decompiler.abstract_value_of_expr address_callee in
              AbstractValue.Map.find_opt addr subst
            with
            | None ->
                (* the address became unreachable so the bug can never be reached; drop it *)
                L.d_printfln ~color:Orange
                  "%a seems no longer reachable, dropping the latent invalid access altogether"
                  Decompiler.pp_expr address_callee ;
                Unsat
            | Some (invalid_address, caller_history) -> (
                let access_trace =
                  Trace.ViaCall
                    { in_call= callee_access_trace
                    ; f= Call callee_pname
                    ; location= call_loc
                    ; history= caller_history }
                in
                let calling_context = (CallEvent.Call callee_pname, call_loc) :: calling_context in
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
                      Decompiler.pp_expr address_callee Decompiler.pp_expr address_caller ;
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
                      Decompiler.pp_expr address_callee Decompiler.pp_expr address_caller ;
                    Sat
                      (FatalError
                         ( Summary
                             (ReportableErrorSummary
                                { diagnostic=
                                    AccessToInvalidAddress
                                      { calling_context
                                      ; invalid_address= address_caller
                                      ; invalidation
                                      ; invalidation_trace
                                      ; access_trace
                                      ; must_be_valid_reason }
                                ; astate= astate_summary } )
                         , [] ) ) ) ) )
  | ISLLatentMemoryError astate ->
      map_call_result ~is_isl_error_prepost:true
        (astate :> AbductiveDomain.t)
        ~f:(fun _subst astate ->
          let open SatUnsat.Import in
          AbductiveDomain.summary_of_post tenv
            (Procdesc.get_proc_name caller_proc_desc)
            (Procdesc.get_attributes caller_proc_desc)
            call_loc astate
          >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
          >>| AccessResult.of_summary
          >>| PulseResult.map ~f:(fun astate_summary -> ISLLatentMemoryError astate_summary) )


let conservatively_initialize_args arg_values ({AbductiveDomain.post} as astate) =
  let reachable_values =
    BaseDomain.reachable_addresses_from (Caml.List.to_seq arg_values) (post :> BaseDomain.t)
  in
  AbstractValue.Set.fold AbductiveDomain.initialize reachable_values astate


let ( let<*> ) x f =
  match (x : _ PulseResult.t) with
  | FatalError _ as err ->
      ([err], None)
  | Ok y ->
      f y
  | Recoverable (y, errors) ->
      let res, contradiction = f y in
      let res = List.map res ~f:(fun result -> PulseResult.append_errors errors result) in
      (res, contradiction)


let call_aux tenv path caller_proc_desc call_loc callee_pname ret actuals call_kind
    (callee_proc_attrs : ProcAttributes.t) exec_states (astate : AbductiveDomain.t) =
  let formals =
    List.map callee_proc_attrs.formals ~f:(fun (mangled, typ, _) ->
        (Pvar.mk mangled callee_pname |> Var.of_pvar, typ) )
  in
  let captured_formals =
    List.map callee_proc_attrs.captured ~f:(fun {CapturedVar.pvar; capture_mode; typ} ->
        (Var.of_pvar pvar, capture_mode, typ) )
  in
  let<*> astate, captured_actuals =
    PulseOperations.get_captured_actuals callee_pname path call_loc ~captured_formals ~call_kind
      ~actuals astate
  in
  let captured_formals = List.map captured_formals ~f:(fun (var, _, typ) -> (var, typ)) in
  let should_keep_at_most_one_disjunct =
    Option.exists Config.pulse_cut_to_one_path_procedures_pattern ~f:(fun regex ->
        Str.string_match regex (Procname.to_string callee_pname) 0 )
  in
  if should_keep_at_most_one_disjunct then
    L.d_printfln "Will keep at most one disjunct because %a is in block list" Procname.pp
      callee_pname ;
  (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
  List.fold ~init:([], None) exec_states ~f:(fun (posts, contradiction) callee_exec_state ->
      if should_keep_at_most_one_disjunct && not (List.is_empty posts) then (posts, contradiction)
      else
        let merge_contradictions contradiction1 contradiction2 =
          match (contradiction1, contradiction2) with
          | None, contradiction
          | contradiction, None
          | (Some (PulseInterproc.Aliasing _) as contradiction), _
          | _, (Some (PulseInterproc.Aliasing _) as contradiction)
          | contradiction, _ ->
              contradiction
        in
        (* apply all pre/post specs *)
        match
          apply_callee tenv path ~caller_proc_desc callee_pname call_loc callee_exec_state
            ~captured_formals ~captured_actuals ~formals ~actuals ~ret astate
        with
        | Unsat, new_contradiction ->
            (* couldn't apply pre/post pair *)
            (posts, merge_contradictions contradiction new_contradiction)
        | Sat post, new_contradiction ->
            (post :: posts, merge_contradictions contradiction new_contradiction) )


let call tenv path ~caller_proc_desc ~(callee_data : (Procdesc.t * PulseSummary.t) option) call_loc
    callee_pname ~ret ~actuals ~formals_opt ~call_kind (astate : AbductiveDomain.t) =
  (* a special case for objc nil messaging *)
  let unknown_objc_nil_messaging astate_unknown proc_name proc_attrs =
    let result_unknown =
      let<+> astate_unknown =
        PulseObjectiveCSummary.append_objc_actual_self_positive proc_name (List.hd actuals)
          astate_unknown
      in
      astate_unknown
    in
    let result_unknown_nil, contradiction =
      PulseObjectiveCSummary.mk_nil_messaging_summary tenv proc_name proc_attrs
      |> Option.value_map ~default:([], None) ~f:(fun nil_summary ->
             call_aux tenv path caller_proc_desc call_loc callee_pname ret actuals call_kind
               proc_attrs [nil_summary] astate )
    in
    (result_unknown @ result_unknown_nil, contradiction)
  in
  match callee_data with
  | Some (callee_proc_desc, exec_states) ->
      call_aux tenv path caller_proc_desc call_loc callee_pname ret actuals call_kind
        (Procdesc.get_attributes callee_proc_desc)
        (exec_states :> ExecutionDomain.t list)
        astate
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Procname.pp callee_pname ;
      let arg_values = List.map actuals ~f:(fun ((value, _), _) -> value) in
      let<*> astate_unknown =
        conservatively_initialize_args arg_values astate
        |> unknown_call path call_loc (SkippedKnownCall callee_pname) (Some callee_pname) ~ret
             ~actuals ~formals_opt
      in
      ScubaLogging.pulse_log_message ~label:"unmodeled_function_operation_pulse"
        ~message:
          (Format.asprintf "Unmodeled Function[Pulse] : %a" Procname.pp_without_templates
             callee_pname ) ;
      IRAttributes.load callee_pname
      |> Option.value_map
           ~default:([Ok (ContinueProgram astate_unknown)], None)
           ~f:(unknown_objc_nil_messaging astate_unknown callee_pname)
