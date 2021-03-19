(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import

type t = AbductiveDomain.t

let is_ptr_to_const formal_typ_opt =
  Option.value_map formal_typ_opt ~default:false ~f:(fun (formal_typ : Typ.t) ->
      match formal_typ.desc with Typ.Tptr (t, _) -> Typ.is_const t.quals | _ -> false )


let unknown_call tenv call_loc reason ~ret ~actuals ~formals_opt astate =
  let event = ValueHistory.Call {f= reason; location= call_loc; in_call= []} in
  let havoc_ret (ret, _) astate = PulseOperations.havoc_id ret [event] astate in
  let rec havoc_fields ((_, history) as addr) typ astate =
    match typ.Typ.desc with
    | Tstruct struct_name -> (
      match Tenv.lookup tenv struct_name with
      | Some {fields} ->
          List.fold fields ~init:astate ~f:(fun acc (field, field_typ, _) ->
              let fresh_value = AbstractValue.mk_fresh () in
              Memory.add_edge addr (FieldAccess field) (fresh_value, [event]) call_loc acc
              |> havoc_fields (fresh_value, history) field_typ )
      | None ->
          astate )
    | _ ->
        astate
  in
  let havoc_actual_if_ptr (actual, actual_typ) formal_typ_opt astate =
    (* We should not havoc when the corresponding formal is a
       pointer to const *)
    match actual_typ.Typ.desc with
    | Tptr (typ, _)
      when (not (Language.curr_language_is Java)) && not (is_ptr_to_const formal_typ_opt) ->
        (* HACK: write through the pointer even if it is invalid (except in Java). This is to avoid raising issues when
           havoc'ing pointer parameters (which normally causes a [check_valid] call. *)
        let fresh_value = AbstractValue.mk_fresh () in
        Memory.add_edge actual Dereference (fresh_value, [event]) call_loc astate
        |> havoc_fields actual typ
    | _ ->
        astate
  in
  let add_skipped_proc astate =
    match reason with
    | CallEvent.SkippedKnownCall proc_name ->
        AbductiveDomain.add_skipped_call proc_name
          (Trace.Immediate {location= call_loc; history= []})
          astate
    | _ ->
        astate
  in
  let havoc_actuals_without_typ_info astate =
    List.fold actuals
      ~f:(fun astate actual_typ -> havoc_actual_if_ptr actual_typ None astate)
      ~init:astate
  in
  L.d_printfln "skipping unknown procedure@." ;
  ( match formals_opt with
  | None ->
      havoc_actuals_without_typ_info astate
  | Some formals -> (
    match
      List.fold2 actuals formals
        ~f:(fun astate actual_typ (_, formal_typ) ->
          havoc_actual_if_ptr actual_typ (Some formal_typ) astate )
        ~init:astate
    with
    | Unequal_lengths ->
        L.d_printfln "ERROR: formals have length %d but actuals have length %d"
          (List.length formals) (List.length actuals) ;
        havoc_actuals_without_typ_info astate
    | Ok result ->
        result ) )
  |> havoc_ret ret |> add_skipped_proc


let apply_callee tenv ~caller_proc_desc callee_pname call_loc callee_exec_state ~ret
    ~captured_vars_with_actuals ~formals ~actuals astate =
  let map_call_result ~is_isl_error_prepost callee_prepost ~f =
    match
      PulseInterproc.apply_prepost ~is_isl_error_prepost callee_pname call_loc ~callee_prepost
        ~captured_vars_with_actuals ~formals ~actuals astate
    with
    | (Sat (Error _) | Unsat) as path_result ->
        path_result
    | Sat (Ok (post, return_val_opt, subst)) ->
        let event = ValueHistory.Call {f= Call callee_pname; location= call_loc; in_call= []} in
        let post =
          match return_val_opt with
          | Some (return_val, return_hist) ->
              PulseOperations.write_id (fst ret) (return_val, event :: return_hist) post
          | None ->
              PulseOperations.havoc_id (fst ret) [event] post
        in
        f subst post
  in
  let open ExecutionDomain in
  let open SatUnsat.Import in
  match callee_exec_state with
  | ContinueProgram astate ->
      map_call_result ~is_isl_error_prepost:false astate ~f:(fun _subst astate ->
          Sat (Ok (ContinueProgram astate)) )
  | AbortProgram astate
  | ExitProgram astate
  | LatentAbortProgram {astate}
  | LatentInvalidAccess {astate} ->
      map_call_result ~is_isl_error_prepost:false
        (astate :> AbductiveDomain.t)
        ~f:(fun subst astate ->
          let* astate_summary_result =
            AbductiveDomain.summary_of_post tenv caller_proc_desc astate
            >>| AccessResult.of_abductive_result
          in
          match astate_summary_result with
          | Error _ as error ->
              Sat error
          | Ok (astate_summary : AbductiveDomain.summary) -> (
            match callee_exec_state with
            | ContinueProgram _ | ISLLatentMemoryError _ ->
                assert false
            | AbortProgram _ ->
                Sat (Ok (AbortProgram astate_summary))
            | ExitProgram _ ->
                Sat (Ok (ExitProgram astate_summary))
            | LatentAbortProgram {latent_issue} -> (
                let latent_issue =
                  LatentIssue.add_call (CallEvent.Call callee_pname, call_loc) latent_issue
                in
                let diagnostic = LatentIssue.to_diagnostic latent_issue in
                match LatentIssue.should_report astate_summary diagnostic with
                | `DelayReport latent_issue ->
                    Sat (Ok (LatentAbortProgram {astate= astate_summary; latent_issue}))
                | `ReportNow ->
                    Sat
                      (Error
                         (ReportableError {diagnostic; astate= (astate_summary :> AbductiveDomain.t)}))
                | `ISLDelay astate ->
                    Sat (Error (ISLError (astate :> AbductiveDomain.t))) )
            | LatentInvalidAccess {address= address_callee; must_be_valid; calling_context} -> (
              match AbstractValue.Map.find_opt address_callee subst with
              | None ->
                  (* the address became unreachable so the bug can never be reached; drop it *)
                  Unsat
              | Some (address, _history) -> (
                  let calling_context =
                    (CallEvent.Call callee_pname, call_loc) :: calling_context
                  in
                  match
                    AbductiveDomain.find_post_cell_opt address (astate_summary :> AbductiveDomain.t)
                    |> Option.bind ~f:(fun (_, attrs) -> Attributes.get_invalid attrs)
                  with
                  | None ->
                      (* still no proof that the address is invalid *)
                      Sat
                        (Ok
                           (LatentInvalidAccess
                              {astate= astate_summary; address; must_be_valid; calling_context}))
                  | Some (invalidation, invalidation_trace) ->
                      Sat
                        (Error
                           (ReportableError
                              { diagnostic=
                                  AccessToInvalidAddress
                                    { calling_context
                                    ; invalidation
                                    ; invalidation_trace
                                    ; access_trace= must_be_valid }
                              ; astate= (astate_summary :> AbductiveDomain.t) })) ) ) ) )
  | ISLLatentMemoryError astate ->
      map_call_result ~is_isl_error_prepost:true
        (astate :> AbductiveDomain.t)
        ~f:(fun _subst astate ->
          AbductiveDomain.summary_of_post tenv caller_proc_desc astate
          >>| AccessResult.of_abductive_result
          >>| Result.map ~f:(fun astate_summary -> ISLLatentMemoryError astate_summary) )


let conservatively_initialize_args arg_values ({AbductiveDomain.post} as astate) =
  let reachable_values = BaseDomain.reachable_addresses_from arg_values (post :> BaseDomain.t) in
  AbstractValue.Set.fold AbductiveDomain.initialize reachable_values astate


let call tenv ~caller_proc_desc ~(callee_data : (Procdesc.t * PulseSummary.t) option) call_loc
    callee_pname ~ret ~actuals ~formals_opt (astate : AbductiveDomain.t) =
  let get_arg_values () = List.map actuals ~f:(fun ((value, _), _) -> value) in
  match callee_data with
  | Some (callee_proc_desc, exec_states) ->
      let formals =
        Procdesc.get_formals callee_proc_desc
        |> List.map ~f:(fun (mangled, _) -> Pvar.mk mangled callee_pname |> Var.of_pvar)
      in
      let captured_vars =
        Procdesc.get_captured callee_proc_desc
        |> List.map ~f:(fun {CapturedVar.name; capture_mode} ->
               let pvar = Pvar.mk name callee_pname in
               (Var.of_pvar pvar, capture_mode) )
      in
      let<*> astate, captured_vars_with_actuals =
        match actuals with
        | (actual_closure, _) :: _
          when not (Procname.is_objc_block callee_pname || List.is_empty captured_vars) ->
            (* Assumption: the first parameter will be a closure *)
            PulseOperations.get_captured_actuals call_loc ~captured_vars ~actual_closure astate
        | _ ->
            Ok (astate, [])
      in
      let should_keep_at_most_one_disjunct =
        Option.exists Config.pulse_cut_to_one_path_procedures_pattern ~f:(fun regex ->
            Str.string_match regex (Procname.to_string callee_pname) 0 )
      in
      if should_keep_at_most_one_disjunct then
        L.d_printfln "Will keep at most one disjunct because %a is in blacklist" Procname.pp
          callee_pname ;
      (* call {!AbductiveDomain.PrePost.apply} on each pre/post pair in the summary. *)
      List.fold ~init:[]
        (exec_states :> ExecutionDomain.t list)
        ~f:(fun posts callee_exec_state ->
          if should_keep_at_most_one_disjunct && not (List.is_empty posts) then posts
          else
            (* apply all pre/post specs *)
            match
              apply_callee tenv ~caller_proc_desc callee_pname call_loc callee_exec_state
                ~captured_vars_with_actuals ~formals ~actuals ~ret astate
            with
            | Unsat ->
                (* couldn't apply pre/post pair *)
                posts
            | Sat post ->
                post :: posts )
  | None ->
      (* no spec found for some reason (unknown function, ...) *)
      L.d_printfln "No spec found for %a@\n" Procname.pp callee_pname ;
      let astate =
        conservatively_initialize_args (get_arg_values ()) astate
        |> unknown_call tenv call_loc (SkippedKnownCall callee_pname) ~ret ~actuals ~formals_opt
      in
      [Ok (ContinueProgram astate)]
