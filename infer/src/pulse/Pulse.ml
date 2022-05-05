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
open PulseOperations.Import

(** raised when we detect that pulse is using too much memory to stop the analysis of the current
    procedure *)
exception AboutToOOM

let report_topl_errors proc_desc err_log summary =
  let f = function
    | ContinueProgram astate ->
        PulseTopl.report_errors proc_desc err_log (AbductiveDomain.Topl.get astate)
    | _ ->
        ()
  in
  List.iter ~f summary


let report_unnecessary_copies proc_desc err_log non_disj_astate =
  PulseNonDisjunctiveDomain.get_copied non_disj_astate
  |> List.iter ~f:(fun (var, location) ->
         let var_name = Format.asprintf "%a" Var.pp var in
         let diagnostic = Diagnostic.UnnecessaryCopy {variable= var; location} in
         PulseReport.report
           ~is_suppressed:
             ( String.is_substring var_name ~substring:"copy"
             || String.is_substring var_name ~substring:"Copy" )
           ~latent:false proc_desc err_log diagnostic )


let heap_size () = (Gc.quick_stat ()).heap_words

module PulseTransferFunctions = struct
  module CFG = ProcCfg.Exceptional
  module DisjDomain = AbstractDomain.PairDisjunct (ExecutionDomain) (PathContext)
  module NonDisjDomain = NonDisjDomain

  type analysis_data = PulseSummary.t InterproceduralAnalysis.t

  let get_pvar_formals pname =
    IRAttributes.load pname |> Option.map ~f:ProcAttributes.get_pvar_formals


  let need_specialization astates =
    List.exists astates ~f:(fun res ->
        match PulseResult.ok res with
        | Some
            ( ContinueProgram {AbductiveDomain.need_specialization}
            | ExceptionRaised {AbductiveDomain.need_specialization} ) ->
            need_specialization
        | Some
            ( ExitProgram astate
            | AbortProgram astate
            | LatentAbortProgram {astate}
            | LatentInvalidAccess {astate}
            | ISLLatentMemoryError astate ) ->
            (astate :> AbductiveDomain.t).need_specialization
        | None ->
            false )


  let reset_need_specialization needed_specialization astates =
    if needed_specialization then
      List.map astates ~f:(fun res ->
          PulseResult.map res ~f:(function
            | ExceptionRaised astate ->
                let astate = AbductiveDomain.set_need_specialization astate in
                ExceptionRaised astate
            | ContinueProgram astate ->
                let astate = AbductiveDomain.set_need_specialization astate in
                ContinueProgram astate
            | ExitProgram astate ->
                let astate = AbductiveDomain.summary_with_need_specialization astate in
                ExitProgram astate
            | AbortProgram astate ->
                let astate = AbductiveDomain.summary_with_need_specialization astate in
                AbortProgram astate
            | LatentAbortProgram latent_abort_program ->
                let astate =
                  AbductiveDomain.summary_with_need_specialization latent_abort_program.astate
                in
                LatentAbortProgram {latent_abort_program with astate}
            | LatentInvalidAccess latent_invalid_access ->
                let astate =
                  AbductiveDomain.summary_with_need_specialization latent_invalid_access.astate
                in
                LatentInvalidAccess {latent_invalid_access with astate}
            | ISLLatentMemoryError astate ->
                let astate = AbductiveDomain.summary_with_need_specialization astate in
                ISLLatentMemoryError astate ) )
    else astates


  let interprocedural_call
      ({InterproceduralAnalysis.analyze_dependency; tenv; proc_desc} as analysis_data) path ret
      callee_pname call_exp func_args call_loc (flags : CallFlags.t) astate =
    let actuals =
      List.map func_args ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
          (arg_payload, typ) )
    in
    match callee_pname with
    | Some callee_pname when not Config.pulse_intraprocedural_only ->
        let formals_opt = get_pvar_formals callee_pname in
        let callee_data = analyze_dependency callee_pname in
        let call_kind_of call_exp =
          match call_exp with
          | Exp.Closure {captured_vars} ->
              `Closure captured_vars
          | Exp.Var id ->
              `Var id
          | _ ->
              `ResolvedProcname
        in
        (* [needed_specialization] = current function already needs specialization before
           the upcoming call (i.e. we did not have enough information to sufficiently
           specialize a callee). *)
        let needed_specialization = astate.AbductiveDomain.need_specialization in
        (* [astate.need_specialization] is false when entering the call. This is to
           detect calls that need specialization. The value will be set back to true
           (if it was) in the end by [reset_need_specialization] *)
        let astate = AbductiveDomain.unset_need_specialization astate in
        let call_kind = call_kind_of call_exp in
        let maybe_res =
          PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data call_loc
            callee_pname ~ret ~actuals ~formals_opt ~call_kind astate
        in
        let res =
          if (not needed_specialization) && need_specialization maybe_res then (
            L.d_printfln "Trying to specialize %a" Exp.pp call_exp ;
            match
              PulseBlockSpecialization.make_specialized_call_exp analysis_data func_args
                callee_pname call_kind path call_loc astate
            with
            | Some (callee_pname, call_exp, astate) ->
                L.d_printfln "Succesfully specialized %a@\n" Exp.pp call_exp ;
                let formals_opt = get_pvar_formals callee_pname in
                let callee_data = analyze_dependency callee_pname in
                PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data call_loc
                  callee_pname ~ret ~actuals ~formals_opt ~call_kind:(call_kind_of call_exp) astate
            | None ->
                L.d_printfln "Failed to specialize %a@\n" Exp.pp call_exp ;
                maybe_res )
          else maybe_res
        in
        ( reset_need_specialization needed_specialization res
        , if Option.is_none callee_data then `UnknownCall else `KnownCall )
    | _ ->
        (* dereference call expression to catch nil issues *)
        ( (let<*> astate, _ =
             if flags.cf_is_objc_block then
               (* We are on an unknown block call, meaning that the block was defined
                  outside the current function and was either passed by the caller
                  as an argument or retrieved from an object. We do not handle blocks
                  inside objects yet so we assume we are in the former case. In this
                  case, we tell the caller that we are missing some information by
                  setting [need_specialization] in the resulting state and the caller
                  will then try to specialize the current function with its available
                  information. *)
               let astate = AbductiveDomain.set_need_specialization astate in
               PulseOperations.eval_deref path ~must_be_valid_reason:BlockCall call_loc call_exp
                 astate
             else PulseOperations.eval_deref path call_loc call_exp astate
           in
           L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
           let astate =
             let arg_values = List.map actuals ~f:(fun ((value, _), _) -> value) in
             PulseCallOperations.conservatively_initialize_args arg_values astate
           in
           let<+> astate =
             PulseCallOperations.unknown_call path call_loc (SkippedUnknownCall call_exp)
               callee_pname ~ret ~actuals ~formals_opt:None astate
           in
           astate )
        , `UnknownCall )


  (** has an object just gone out of scope? *)
  let get_out_of_scope_object callee_pname actuals (flags : CallFlags.t) =
    (* injected destructors are precisely inserted where an object goes out of scope *)
    if flags.cf_injected_destructor then
      match (callee_pname, actuals) with
      | Some (Procname.ObjC_Cpp pname), [(Exp.Lvar pvar, typ)]
        when Pvar.is_local pvar && not (Procname.ObjC_Cpp.is_inner_destructor pname) ->
          (* ignore inner destructors, only trigger out of scope on the final destructor call *)
          Some (pvar, typ)
      | _ ->
          None
    else None


  (** [out_of_scope_access_expr] has just gone out of scope and in now invalid *)
  let exec_object_out_of_scope path call_loc (pvar, typ) exec_state =
    match (exec_state : ExecutionDomain.t) with
    | ContinueProgram astate | ExceptionRaised astate ->
        let gone_out_of_scope = Invalidation.GoneOutOfScope (pvar, typ) in
        let* astate, out_of_scope_base =
          PulseOperations.eval path NoAccess call_loc (Exp.Lvar pvar) astate
        in
        (* invalidate [&x] *)
        PulseOperations.invalidate path
          (StackAddress (Var.of_pvar pvar, ValueHistory.epoch))
          call_loc gone_out_of_scope out_of_scope_base astate
        >>| ExecutionDomain.continue
    | ISLLatentMemoryError _
    | AbortProgram _
    | ExitProgram _
    | LatentAbortProgram _
    | LatentInvalidAccess _ ->
        Ok exec_state


  let topl_small_step loc procname arguments (return, _typ) exec_state_res =
    let arguments =
      List.map arguments ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload} -> fst arg_payload)
    in
    let return = Var.of_id return in
    let do_astate astate =
      let return = Option.map ~f:fst (Stack.find_opt return astate) in
      let topl_event = PulseTopl.Call {return; arguments; procname} in
      let keep = AbductiveDomain.get_reachable astate in
      AbductiveDomain.Topl.small_step loc ~keep topl_event astate
    in
    let do_one_exec_state (exec_state : ExecutionDomain.t) : ExecutionDomain.t =
      match exec_state with
      | ContinueProgram astate ->
          ContinueProgram (do_astate astate)
      | ISLLatentMemoryError _
      | AbortProgram _
      | LatentAbortProgram _
      | ExitProgram _
      | ExceptionRaised _
      | LatentInvalidAccess _ ->
          exec_state
    in
    List.map ~f:(PulseResult.map ~f:do_one_exec_state) exec_state_res


  let topl_store_step path loc ~lhs ~rhs:_ astate =
    match (lhs : Exp.t) with
    | Lindex (arr, index) ->
        (let open PulseResult.Let_syntax in
        let* _astate, (aw_array, _history) = PulseOperations.eval path Read loc arr astate in
        let+ _astate, (aw_index, _history) = PulseOperations.eval path Read loc index astate in
        let topl_event = PulseTopl.ArrayWrite {aw_array; aw_index} in
        let keep = AbductiveDomain.get_reachable astate in
        AbductiveDomain.Topl.small_step loc ~keep topl_event astate)
        |> PulseResult.ok (* don't emit Topl event if evals fail *) |> Option.value ~default:astate
    | _ ->
        astate


  let dispatch_call ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path ret
      call_exp actuals call_loc flags astate =
    let<*> astate, callee_pname = PulseOperations.eval_proc_name path call_loc call_exp astate in
    (* special case for objc dispatch models *)
    let callee_pname, call_exp, actuals =
      match callee_pname with
      | Some callee_pname when ObjCDispatchModels.is_model callee_pname -> (
        match ObjCDispatchModels.get_dispatch_closure_opt actuals with
        | Some (block_name, closure_exp, args) ->
            (Some block_name, closure_exp, args)
        | None ->
            (Some callee_pname, call_exp, actuals) )
      | _ ->
          (callee_pname, call_exp, actuals)
    in
    (* evaluate all actuals *)
    let<*> astate, rev_func_args =
      PulseResult.list_fold actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let+ astate, actual_evaled = PulseOperations.eval path Read call_loc actual_exp astate in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    let func_args = List.rev rev_func_args in
    let model =
      match callee_pname with
      | Some callee_pname ->
          PulseModels.dispatch tenv callee_pname func_args
          |> Option.map ~f:(fun model -> (model, callee_pname))
      | None ->
          (* unresolved function pointer, etc.: skip *)
          None
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let exec_states_res, call_was_unknown =
      match model with
      | Some (model, callee_procname) ->
          L.d_printfln "Found model for call@\n" ;
          let astate =
            let arg_values =
              List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= value, _} ->
                  value )
            in
            PulseCallOperations.conservatively_initialize_args arg_values astate
          in
          (model {analysis_data; path; callee_procname; location= call_loc; ret} astate, `KnownCall)
      | None ->
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let r =
            interprocedural_call analysis_data path ret callee_pname call_exp func_args call_loc
              flags astate
          in
          PerfEvent.(log (fun logger -> log_end_event logger ())) ;
          r
    in
    let exec_states_res =
      let one_state exec_state_res =
        let* exec_state = exec_state_res in
        match exec_state with
        | ContinueProgram astate ->
            let call_event =
              match callee_pname with
              | None ->
                  Either.First call_exp
              | Some proc_name ->
                  Either.Second proc_name
            in
            let call_was_unknown =
              match call_was_unknown with `UnknownCall -> true | `KnownCall -> false
            in
            let+ astate =
              PulseTaintOperations.call tenv path call_loc ret ~call_was_unknown call_event
                func_args astate
            in
            ContinueProgram astate
        | ( ExceptionRaised _
          | ExitProgram _
          | AbortProgram _
          | LatentAbortProgram _
          | LatentInvalidAccess _
          | ISLLatentMemoryError _ ) as exec_state ->
            Ok exec_state
      in
      List.map exec_states_res ~f:one_state
    in
    let exec_states_res =
      if Topl.is_active () then
        match callee_pname with
        | Some callee_pname ->
            topl_small_step call_loc callee_pname func_args ret exec_states_res
        | None ->
            (* skip, as above for non-topl *) exec_states_res
      else exec_states_res
    in
    let exec_states_res =
      match get_out_of_scope_object callee_pname actuals flags with
      | Some pvar_typ ->
          L.d_printfln "%a is going out of scope" Pvar.pp_value (fst pvar_typ) ;
          List.map exec_states_res ~f:(fun exec_state ->
              exec_state >>= exec_object_out_of_scope path call_loc pvar_typ )
      | None ->
          exec_states_res
    in
    if Option.exists callee_pname ~f:IRAttributes.is_no_return then
      List.filter_map exec_states_res ~f:(fun exec_state_res ->
          (let+ exec_state = exec_state_res in
           PulseSummary.force_exit_program tenv proc_desc err_log call_loc exec_state
           |> SatUnsat.map (fun (exec_state : ExecutionDomain.summary) ->
                  (exec_state :> ExecutionDomain.t) )
           |> SatUnsat.sat )
          |> PulseResult.of_some )
    else exec_states_res


  (* [get_dealloc_from_dynamic_types vars_types loc] returns a dealloc procname and vars and
     type needed to execute a call to dealloc for the given variables for which the dynamic type
     is an Objective-C class. *)
  let get_dealloc_from_dynamic_types dynamic_types_unreachable =
    let get_dealloc (var, typ) =
      Typ.name typ
      |> Option.bind ~f:(fun name ->
             let cls_typ = Typ.mk (Typ.Tstruct name) in
             match Var.get_ident var with
             | Some id when Typ.is_objc_class cls_typ ->
                 let ret_id = Ident.create_fresh Ident.knormal in
                 let dealloc = Procname.make_objc_dealloc name in
                 let typ = Typ.mk_ptr cls_typ in
                 Some (ret_id, id, typ, dealloc)
             | _ ->
                 None )
    in
    List.filter_map ~f:get_dealloc dynamic_types_unreachable


  (* Count strong references reachable from the stack for each RefCounted
     object in memory and set that count to their respective
     __infer_mode_reference_count field by calling the __objc_set_ref_count
     builtin *)
  let set_ref_counts astate location path
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) =
    let find_var_opt astate addr =
      Stack.fold
        (fun var (var_addr, _) var_opt ->
          if AbstractValue.equal addr var_addr then Some var else var_opt )
        astate None
    in
    let ref_counts = PulseRefCounting.count_references tenv astate in
    AbstractValue.Map.fold
      (fun addr count (astates, ret_vars) ->
        let ret_vars = ref ret_vars in
        let astates =
          List.concat_map astates ~f:(fun astate ->
              match astate with
              | ISLLatentMemoryError _
              | AbortProgram _
              | ExceptionRaised _
              | ExitProgram _
              | LatentAbortProgram _
              | LatentInvalidAccess _ ->
                  [astate]
              | ContinueProgram astate as default_astate ->
                  let astates : ExecutionDomain.t list option =
                    let open IOption.Let_syntax in
                    let* self_var = find_var_opt astate addr in
                    let+ self_typ =
                      let* attrs = AbductiveDomain.AddressAttributes.find_opt addr astate in
                      Attributes.get_dynamic_type attrs
                    in
                    let ret_id = Ident.create_fresh Ident.knormal in
                    ret_vars := Var.of_id ret_id :: !ret_vars ;
                    let ret = (ret_id, StdTyp.void) in
                    let call_flags = CallFlags.default in
                    let call_exp = Exp.Const (Cfun BuiltinDecl.__objc_set_ref_count) in
                    let actuals =
                      [ (Var.to_exp self_var, self_typ)
                      ; (Exp.Const (Cint (IntLit.of_int count)), StdTyp.uint) ]
                    in
                    let call_instr = Sil.Call (ret, call_exp, actuals, location, call_flags) in
                    L.d_printfln ~color:Pp.Orange "@\nExecuting injected instr:%a@\n@."
                      (Sil.pp_instr Pp.text ~print_types:true)
                      call_instr ;
                    dispatch_call analysis_data path ret call_exp actuals location call_flags astate
                    |> PulseReport.report_exec_results tenv proc_desc err_log location
                  in
                  Option.value ~default:[default_astate] astates )
        in
        (astates, !ret_vars) )
      ref_counts
      ([ContinueProgram astate], [])


  (* In the case of variables that point to Objective-C classes for which we have a dynamic type, we
     add and execute calls to dealloc. The main advantage of adding this calls
     is that some memory could be freed in dealloc, and we would be reporting a leak on it if we
     didn't call it. *)
  let execute_injected_dealloc_calls
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path vars astate
      location =
    let used_ids = Stack.keys astate |> List.filter_map ~f:(fun var -> Var.get_ident var) in
    Ident.update_name_generator used_ids ;
    let call_dealloc (astate_list : ExecutionDomain.t list) (ret_id, id, typ, dealloc) =
      let ret = (ret_id, StdTyp.void) in
      let call_flags = CallFlags.default in
      let call_exp = Exp.Const (Cfun dealloc) in
      let actuals = [(Exp.Var id, typ)] in
      let call_instr = Sil.Call (ret, call_exp, actuals, location, call_flags) in
      L.d_printfln ~color:Pp.Orange "@\nExecuting injected instr:%a@\n@."
        (Sil.pp_instr Pp.text ~print_types:true)
        call_instr ;
      List.concat_map astate_list ~f:(fun (astate : ExecutionDomain.t) ->
          match astate with
          | ISLLatentMemoryError _
          | AbortProgram _
          | ExceptionRaised _
          | ExitProgram _
          | LatentAbortProgram _
          | LatentInvalidAccess _ ->
              [astate]
          | ContinueProgram astate ->
              dispatch_call analysis_data path ret call_exp actuals location call_flags astate
              |> PulseReport.report_exec_results tenv proc_desc err_log location )
    in
    let dynamic_types_unreachable =
      PulseOperations.get_dynamic_type_unreachable_values vars astate
    in
    let dealloc_data = get_dealloc_from_dynamic_types dynamic_types_unreachable in
    let ret_vars = List.map ~f:(fun (ret_id, _, _, _) -> Var.of_id ret_id) dealloc_data in
    L.d_printfln ~color:Pp.Orange
      "Executing injected call to dealloc for vars (%a) that are exiting the scope@."
      (Pp.seq ~sep:"," Var.pp) vars ;
    let astates = List.fold ~f:call_dealloc dealloc_data ~init:[ContinueProgram astate] in
    (astates, ret_vars)


  let remove_vars vars location astates =
    List.map astates ~f:(fun (exec_state : ExecutionDomain.t) ->
        match exec_state with
        | ISLLatentMemoryError _
        | AbortProgram _
        | ExitProgram _
        | LatentAbortProgram _
        | LatentInvalidAccess _ ->
            exec_state
        | ContinueProgram astate ->
            ContinueProgram (PulseOperations.remove_vars vars location astate)
        | ExceptionRaised astate ->
            ExceptionRaised (PulseOperations.remove_vars vars location astate) )


  let exit_scope vars location path astate astate_n
      ({InterproceduralAnalysis.proc_desc; tenv} as analysis_data) =
    if Procname.is_java (Procdesc.get_proc_name proc_desc) then
      (remove_vars vars location [ContinueProgram astate], path, astate_n)
    else
      (* Some RefCounted variables must not be removed at their ExitScope
         because they may still be referenced by someone and that reference may
         be destroyed in the future. In that case, we would miss the opportunity
         to properly dealloc the object if it were removed from the stack,
         leading to potential FP memory leaks *)
      let vars = PulseRefCounting.removable_vars tenv astate vars in
      (* Prepare objects in memory before calling any dealloc:
         - set the number of unique strong references accessible from the
          stack to each object's respective __infer_mode_reference_count
          field by calling the __objc_set_ref_count modelled function
         This needs to be done before any call to dealloc because dealloc's
         behavior depends on this ref count and one's dealloc may call
         another's. Consequently, they each need to be up to date beforehand.
         The return variables of the calls to __objc_set_ref_count must be
         removed *)
      let astates, ret_vars = set_ref_counts astate location path analysis_data in
      (* Here we add and execute calls to dealloc for Objective-C objects
         before removing the variables. The return variables of those calls
         must be removed as welll *)
      let astates, ret_vars =
        List.fold_left astates ~init:([], ret_vars)
          ~f:(fun ((acc_astates, acc_ret_vars) as acc) astate ->
            match astate with
            | ContinueProgram astate ->
                let astates, ret_vars =
                  execute_injected_dealloc_calls analysis_data path vars astate location
                in
                (astates @ acc_astates, ret_vars @ acc_ret_vars)
            | _ ->
                acc )
      in
      (* OPTIM: avoid re-allocating [vars] when [ret_vars] is empty
         (in particular if no ObjC objects are involved), but otherwise
         assume [ret_vars] is potentially larger than [vars] and so
         append [vars] to [ret_vars]. *)
      let vars_to_remove = if List.is_empty ret_vars then vars else List.rev_append vars ret_vars in
      ( remove_vars vars_to_remove location astates
      , path
      , PulseNonDisjunctiveOperations.mark_modified_copies vars astates astate_n )


  let and_is_int_if_integer_type typ v astate =
    if Typ.is_int typ then PulseArithmetic.and_is_int v astate else Ok astate


  let check_modified_before_dtor args call_exp astate astate_n =
    match ((call_exp : Exp.t), args) with
    | (Const (Cfun proc_name) | Closure {name= proc_name}), (Exp.Lvar pvar, _) :: _
      when Procname.is_destructor proc_name ->
        let var = Var.of_pvar pvar in
        PulseNonDisjunctiveOperations.mark_modified_copies_with [var] ~astate astate_n
        |> NonDisjDomain.checked_via_dtor var
    | _ ->
        astate_n


  let exec_instr_aux ({PathContext.timestamp} as path) (astate : ExecutionDomain.t)
      (astate_n : NonDisjDomain.t)
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) _cfg_node
      (instr : Sil.instr) : ExecutionDomain.t list * PathContext.t * NonDisjDomain.t =
    match astate with
    | AbortProgram _ | ISLLatentMemoryError _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
        ([astate], path, astate_n)
    (* an exception has been raised, we skip the other instructions until we enter in
       exception edge *)
    | ExceptionRaised _
    (* program already exited, simply propagate the exited state upwards  *)
    | ExitProgram _ ->
        ([astate], path, astate_n)
    | ContinueProgram astate -> (
      match instr with
      | Load {id= lhs_id; e= rhs_exp; loc; typ} ->
          (* [lhs_id := *rhs_exp] *)
          let deref_rhs astate =
            let results =
              if Config.pulse_isl then
                PulseOperations.eval_deref_isl path loc rhs_exp astate
                |> List.map ~f:(fun result ->
                       let* astate, rhs_addr_hist = result in
                       and_is_int_if_integer_type typ (fst rhs_addr_hist) astate
                       >>| PulseOperations.write_id lhs_id rhs_addr_hist )
              else
                [ (let* astate, rhs_addr_hist = PulseOperations.eval_deref path loc rhs_exp astate in
                   and_is_int_if_integer_type typ (fst rhs_addr_hist) astate
                   >>| PulseOperations.write_id lhs_id rhs_addr_hist ) ]
            in
            PulseReport.report_results tenv proc_desc err_log loc results
          in
          let set_global_astates =
            let is_global_constant pvar =
              Pvar.(is_global pvar && (is_const pvar || is_compile_constant pvar))
            in
            let is_global_func_pointer pvar =
              Pvar.is_global pvar && Typ.is_pointer_to_function typ
              && Config.pulse_inline_global_init_func_pointer
            in
            match rhs_exp with
            | Lvar pvar when is_global_constant pvar || is_global_func_pointer pvar -> (
              (* Inline initializers of global constants or globals function pointers when they are being used.
                 This addresses nullptr false positives by pruning infeasable paths global_var != global_constant_value,
                 where global_constant_value is the value of global_var *)
              (* TODO: Initial global constants only once *)
              match Pvar.get_initializer_pname pvar with
              | Some proc_name ->
                  L.d_printfln_escaped "Found initializer for %a" (Pvar.pp Pp.text) pvar ;
                  let call_flags = CallFlags.default in
                  let ret_id_void = (Ident.create_fresh Ident.knormal, StdTyp.void) in
                  let no_error_states =
                    dispatch_call analysis_data path ret_id_void (Const (Cfun proc_name)) [] loc
                      call_flags astate
                    |> List.filter_map ~f:(function
                         | Ok (ContinueProgram astate) ->
                             Some astate
                         | _ ->
                             (* ignore errors in global initializers *)
                             None )
                  in
                  if List.is_empty no_error_states then [astate] else no_error_states
              | None ->
                  [astate] )
            | _ ->
                [astate]
          in
          (List.concat_map set_global_astates ~f:deref_rhs, path, astate_n)
      | Store {e1= lhs_exp; e2= rhs_exp; loc; typ} ->
          (* [*lhs_exp := rhs_exp] *)
          let event =
            match lhs_exp with
            | Lvar v when Pvar.is_return v ->
                ValueHistory.Returned (loc, timestamp)
            | _ ->
                ValueHistory.Assignment (loc, timestamp)
          in
          let result =
            let<*> astate, (rhs_addr, rhs_history) =
              PulseOperations.eval path NoAccess loc rhs_exp astate
            in
            let<*> is_structured, ls_astate_lhs_addr_hist =
              if Config.pulse_isl then
                PulseOperations.eval_structure_isl path Write loc lhs_exp astate
              else
                let+ astate, lhs_addr_hist = PulseOperations.eval path Write loc lhs_exp astate in
                (false, [Ok (astate, lhs_addr_hist)])
            in
            let hist = ValueHistory.sequence ~context:path.conditions event rhs_history in
            let write_function lhs_addr_hist astate =
              if is_structured then
                PulseOperations.write_deref_biad_isl path loc ~ref:lhs_addr_hist Dereference
                  ~obj:(rhs_addr, hist) astate
              else
                [ PulseOperations.write_deref path loc ~ref:lhs_addr_hist ~obj:(rhs_addr, hist)
                    astate ]
            in
            let astates =
              List.concat_map ls_astate_lhs_addr_hist ~f:(fun result ->
                  let<*> astate, lhs_addr_hist = result in
                  let<*> astate = and_is_int_if_integer_type typ rhs_addr astate in
                  write_function lhs_addr_hist astate )
            in
            let astates =
              if Topl.is_active () then
                List.map astates ~f:(fun result ->
                    let+ astate = result in
                    topl_store_step path loc ~lhs:lhs_exp ~rhs:rhs_exp astate )
              else astates
            in
            match lhs_exp with
            | Lvar pvar when Pvar.is_return pvar ->
                List.map astates ~f:(fun result ->
                    let* astate = result in
                    PulseOperations.check_address_escape loc proc_desc rhs_addr rhs_history astate )
            | _ ->
                astates
          in
          let astate_n = NonDisjDomain.set_captured_variables rhs_exp astate_n in
          (PulseReport.report_results tenv proc_desc err_log loc result, path, astate_n)
      | Call (ret, call_exp, actuals, loc, call_flags) ->
          let astate_n = check_modified_before_dtor actuals call_exp astate astate_n in
          let astates =
            dispatch_call analysis_data path ret call_exp actuals loc call_flags astate
            |> PulseReport.report_exec_results tenv proc_desc err_log loc
          in
          let astate_n, astates =
            PulseNonDisjunctiveOperations.add_copies path loc call_exp actuals astates astate_n
          in
          (astates, path, astate_n)
      | Prune (condition, loc, is_then_branch, if_kind) ->
          let prune_result = PulseOperations.prune path loc ~condition astate in
          let path =
            match PulseResult.ok prune_result with
            | None ->
                path
            | Some (_, hist) ->
                if Sil.is_terminated_if_kind if_kind then
                  let hist =
                    ValueHistory.sequence
                      (ConditionPassed {if_kind; is_then_branch; location= loc; timestamp})
                      hist
                  in
                  {path with conditions= hist :: path.conditions}
                else path
          in
          let results =
            let<*> astate, _ = prune_result in
            if PulseArithmetic.is_unsat_cheap astate then
              (* [condition] is known to be unsatisfiable: prune path *)
              []
            else
              (* [condition] is true or unknown value: go into the branch *)
              [Ok (ContinueProgram astate)]
          in
          (PulseReport.report_exec_results tenv proc_desc err_log loc results, path, astate_n)
      | Metadata EndBranches ->
          (* We assume that terminated conditions are well-parenthesised, hence an [EndBranches]
             instruction terminates the most recently seen terminated conditional. The empty case
             shouldn't happen but let's not crash by the fault of possible errors in frontends. *)
          let path = {path with conditions= List.tl path.conditions |> Option.value ~default:[]} in
          ([ContinueProgram astate], path, astate_n)
      | Metadata (ExitScope (vars, location)) ->
          exit_scope vars location path astate astate_n analysis_data
      | Metadata (VariableLifetimeBegins (pvar, typ, location)) when not (Pvar.is_global pvar) ->
          ( [ PulseOperations.realloc_pvar tenv path pvar typ location astate
              |> ExecutionDomain.continue ]
          , path
          , astate_n )
      | Metadata
          ( Abstract _
          | CatchEntry _
          | Nullify _
          | Skip
          | TryEntry _
          | TryExit _
          | VariableLifetimeBegins _ ) ->
          ([ContinueProgram astate], path, astate_n) )


  let exec_instr ((astate, path), astate_n) analysis_data cfg_node instr :
      DisjDomain.t list * NonDisjDomain.t =
    let heap_size = heap_size () in
    ( match Config.pulse_max_heap with
    | Some max_heap_size when heap_size > max_heap_size ->
        let pname = Procdesc.get_proc_name analysis_data.InterproceduralAnalysis.proc_desc in
        L.internal_error
          "OOM danger: heap size is %d words, more than the specified threshold of %d words. \
           Aborting the analysis of the procedure %a to avoid running out of memory.@\n"
          heap_size max_heap_size Procname.pp pname ;
        raise AboutToOOM
    | _ ->
        () ) ;
    let astates, path, astate_n =
      exec_instr_aux path astate astate_n analysis_data cfg_node instr
    in
    (* Sometimes instead of stopping on contradictions a false path condition is recorded
       instead. Prune these early here so they don't spuriously count towards the disjunct limit. *)
    ( List.filter_map astates ~f:(fun exec_state ->
          if ExecutionDomain.is_unsat_cheap exec_state then None
          else Some (exec_state, PathContext.post_exec_instr path) )
    , astate_n )


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module DisjunctiveAnalyzer =
  AbstractInterpreter.MakeDisjunctive
    (PulseTransferFunctions)
    (struct
      let join_policy = `UnderApproximateAfter Config.pulse_max_disjuncts

      let widen_policy = `UnderApproximateAfterNumIterations Config.pulse_widen_threshold
    end)

let with_html_debug_node node ~desc ~f =
  AnalysisCallbacks.html_debug_new_node_session node
    ~pp_name:(fun fmt -> F.pp_print_string fmt desc)
    ~f


let initial tenv proc_desc =
  let initial_astate =
    AbductiveDomain.mk_initial tenv proc_desc
    |> PulseObjectiveCSummary.initial_with_positive_self proc_desc
    |> PulseTaintOperations.taint_initial tenv proc_desc
  in
  [(ContinueProgram initial_astate, PathContext.initial)]


let should_analyze proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_id = Procname.to_unique_id proc_name in
  let f regex = not (Str.string_match regex proc_id 0) in
  Option.value_map Config.pulse_skip_procedures ~f ~default:true
  && not (Procdesc.is_too_big Pulse ~max_cfg_size:Config.pulse_max_cfg_size proc_desc)


let exit_function analysis_data location posts non_disj_astate =
  let astates, astate_n =
    List.fold_left posts ~init:([], non_disj_astate)
      ~f:(fun (acc_astates, astate_n) (exec_state, path) ->
        match exec_state with
        | ISLLatentMemoryError _
        | AbortProgram _
        | ExitProgram _
        | ExceptionRaised _
        | LatentAbortProgram _
        | LatentInvalidAccess _ ->
            (exec_state :: acc_astates, astate_n)
        | ContinueProgram astate ->
            let post = (astate.AbductiveDomain.post :> BaseDomain.t) in
            let vars =
              BaseStack.fold
                (fun var _ vars -> if Var.is_return var then vars else var :: vars)
                post.stack []
            in
            let astates, _, astate_n =
              PulseTransferFunctions.exit_scope vars location path astate astate_n analysis_data
            in
            (PulseTransferFunctions.remove_vars vars location astates @ acc_astates, astate_n) )
  in
  (List.rev astates, astate_n)


let analyze ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) =
  if should_analyze proc_desc then (
    AbstractValue.State.reset () ;
    PulseTopl.Debug.dropped_disjuncts_count := 0 ;
    let initial =
      with_html_debug_node (Procdesc.get_start_node proc_desc) ~desc:"initial state creation"
        ~f:(fun () -> (initial tenv proc_desc, NonDisjDomain.bottom))
    in
    match DisjunctiveAnalyzer.compute_post analysis_data ~initial proc_desc with
    | Some (posts, non_disj_astate) ->
        with_html_debug_node (Procdesc.get_exit_node proc_desc) ~desc:"pulse summary creation"
          ~f:(fun () ->
            let exit_location = Procdesc.get_exit_node proc_desc |> Procdesc.Node.get_loc in
            let posts, non_disj_astate =
              (* Do final cleanup at the end of procdesc
                 Forget path contexts on the way, we don't propagate them across functions *)
              exit_function analysis_data exit_location posts non_disj_astate
            in
            let objc_nil_summary = PulseObjectiveCSummary.mk_nil_messaging_summary tenv proc_desc in
            let summary =
              PulseSummary.of_posts tenv proc_desc err_log exit_location
                (Option.to_list objc_nil_summary @ posts)
            in
            report_topl_errors proc_desc err_log summary ;
            report_unnecessary_copies proc_desc err_log non_disj_astate ;
            if Config.trace_topl then
              L.debug Analysis Quiet "ToplTrace: dropped %d disjuncts in %a@\n"
                !PulseTopl.Debug.dropped_disjuncts_count
                Procname.pp_unique_id
                (Procdesc.get_proc_name proc_desc) ;
            if Config.pulse_scuba_logging then
              ScubaLogging.log_count ~label:"pulse_summary" ~value:(List.length summary) ;
            Stats.add_pulse_summaries_count (List.length summary) ;
            Some summary )
    | None ->
        None )
  else None


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  if should_analyze proc_desc then (
    try analyze analysis_data
    with AboutToOOM ->
      (* We trigger GC to avoid skipping the next procedure that will be analyzed. *)
      Gc.major () ;
      None )
  else None
