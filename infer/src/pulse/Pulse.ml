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


let is_not_implicit pname =
  not
    (Option.exists (IRAttributes.load pname) ~f:(fun attrs -> attrs.ProcAttributes.is_cpp_implicit))


let report_unnecessary_copies proc_desc err_log non_disj_astate =
  let pname = Procdesc.get_proc_name proc_desc in
  if is_not_implicit pname then
    PulseNonDisjunctiveDomain.get_copied non_disj_astate
    |> List.iter ~f:(fun (copied_into, source_typ, location, copied_location, from) ->
           let copy_name = Format.asprintf "%a" Attribute.CopiedInto.pp copied_into in
           let is_suppressed = PulseNonDisjunctiveOperations.has_copy_in copy_name in
           let diagnostic =
             Diagnostic.UnnecessaryCopy {copied_into; source_typ; location; copied_location; from}
           in
           PulseReport.report ~is_suppressed ~latent:false proc_desc err_log diagnostic )


let report_unnecessary_parameter_copies proc_desc err_log non_disj_astate =
  let pname = Procdesc.get_proc_name proc_desc in
  if is_not_implicit pname then
    PulseNonDisjunctiveDomain.get_const_refable_parameters non_disj_astate
    |> List.iter ~f:(fun (param, typ, location) ->
           let diagnostic =
             if Typ.is_shared_pointer typ then
               if NonDisjDomain.is_lifetime_extended param non_disj_astate then None
               else
                 let used_locations = NonDisjDomain.get_loaded_locations param non_disj_astate in
                 Some (Diagnostic.ReadonlySharedPtrParameter {param; typ; location; used_locations})
             else Some (Diagnostic.ConstRefableParameter {param; typ; location})
           in
           Option.iter diagnostic ~f:(fun diagnostic ->
               PulseReport.report ~is_suppressed:false ~latent:false proc_desc err_log diagnostic ) )


let heap_size () = (Gc.quick_stat ()).heap_words

module PulseTransferFunctions = struct
  module CFG = ProcCfg.ExceptionalNoSinkToExitEdge
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
            | LatentInvalidAccess {astate} ) ->
            AbductiveDomain.Summary.need_specialization astate
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
                let astate = AbductiveDomain.Summary.with_need_specialization astate in
                ExitProgram astate
            | AbortProgram astate ->
                let astate = AbductiveDomain.Summary.with_need_specialization astate in
                AbortProgram astate
            | LatentAbortProgram latent_abort_program ->
                let astate =
                  AbductiveDomain.Summary.with_need_specialization latent_abort_program.astate
                in
                LatentAbortProgram {latent_abort_program with astate}
            | LatentInvalidAccess latent_invalid_access ->
                let astate =
                  AbductiveDomain.Summary.with_need_specialization latent_invalid_access.astate
                in
                LatentInvalidAccess {latent_invalid_access with astate} ) )
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
        let maybe_call_with_alias callee_pname call_exp ((res, contradiction) as call_res) =
          if List.is_empty res then
            match contradiction with
            | Some (PulseInterproc.Aliasing _) -> (
                L.d_printfln "Trying to alias-specialize %a" Exp.pp call_exp ;
                match
                  PulseAliasSpecialization.make_specialized_call_exp callee_pname func_args
                    (call_kind_of call_exp) path call_loc astate
                with
                | Some (callee_pname, call_exp, astate) ->
                    L.d_printfln "Succesfully alias-specialized %a@\n" Exp.pp call_exp ;
                    let formals_opt = get_pvar_formals callee_pname in
                    let callee_data = analyze_dependency callee_pname in
                    let call_res =
                      PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data
                        call_loc callee_pname ~ret ~actuals ~formals_opt
                        ~call_kind:(call_kind_of call_exp) astate
                    in
                    (callee_pname, call_exp, call_res)
                | None ->
                    L.d_printfln "Failed to alias-specialize %a@\n" Exp.pp call_exp ;
                    (callee_pname, call_exp, call_res) )
            | _ ->
                (callee_pname, call_exp, call_res)
          else (callee_pname, call_exp, call_res)
        in
        let maybe_call_specialization callee_pname call_exp ((res, _) as call_res) =
          if (not needed_specialization) && need_specialization res then (
            L.d_printfln "Trying to closure-specialize %a" Exp.pp call_exp ;
            match
              PulseClosureSpecialization.make_specialized_call_exp analysis_data func_args
                callee_pname (call_kind_of call_exp) path call_loc astate
            with
            | Some (callee_pname, call_exp, astate) ->
                L.d_printfln "Succesfully closure-specialized %a@\n" Exp.pp call_exp ;
                let formals_opt = get_pvar_formals callee_pname in
                let callee_data = analyze_dependency callee_pname in
                PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data call_loc
                  callee_pname ~ret ~actuals ~formals_opt ~call_kind:(call_kind_of call_exp) astate
                |> maybe_call_with_alias callee_pname call_exp
            | None ->
                L.d_printfln "Failed to closure-specialize %a@\n" Exp.pp call_exp ;
                (callee_pname, call_exp, call_res) )
          else (callee_pname, call_exp, call_res)
        in
        let res, _contradiction =
          let callee_pname, call_exp, call_res =
            PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~callee_data call_loc
              callee_pname ~ret ~actuals ~formals_opt ~call_kind astate
            |> maybe_call_with_alias callee_pname call_exp
          in
          let _, _, call_res = maybe_call_specialization callee_pname call_exp call_res in
          call_res
        in
        ( reset_need_specialization needed_specialization res
        , if Option.is_none callee_data then `UnknownCall else `KnownCall )
    | _ ->
        (* dereference call expression to catch nil issues *)
        ( (let<**> astate, _ =
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
             else
               let astate =
                 if
                   (* this condition may need refining to check that the function comes
                      from the function's parameters or captured variables.
                      The function_pointer_specialization option is there to compensate
                      this / control the specialization's agressivity *)
                   Config.function_pointer_specialization
                   && Language.equal Language.Clang
                        (Procname.get_language (Procdesc.get_proc_name proc_desc))
                 then AbductiveDomain.set_need_specialization astate
                 else astate
               in
               PulseOperations.eval_deref path call_loc call_exp astate
           in
           L.d_printfln "Skipping indirect call %a@\n" Exp.pp call_exp ;
           let astate =
             let arg_values = List.map actuals ~f:(fun ((value, _), _) -> value) in
             PulseOperations.conservatively_initialize_args arg_values astate
           in
           let<++> astate =
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
        let+* astate, out_of_scope_base =
          PulseOperations.eval path NoAccess call_loc (Exp.Lvar pvar) astate
        in
        (* invalidate [&x] *)
        PulseOperations.invalidate path
          (StackAddress (Var.of_pvar pvar, ValueHistory.epoch))
          call_loc gone_out_of_scope out_of_scope_base astate
        >>| ExecutionDomain.continue
    | AbortProgram _ | ExitProgram _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
        Sat (Ok exec_state)


  let topl_small_step loc procname arguments (return, _typ) exec_state_res =
    let arguments =
      List.map arguments ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload} -> fst arg_payload)
    in
    let return = Var.of_id return in
    let do_astate astate =
      let return = Option.map ~f:fst (Stack.find_opt return astate) in
      let topl_event = PulseTopl.Call {return; arguments; procname} in
      AbductiveDomain.Topl.small_step loc topl_event astate
    in
    let do_one_exec_state (exec_state : ExecutionDomain.t) : ExecutionDomain.t =
      match exec_state with
      | ContinueProgram astate ->
          ContinueProgram (do_astate astate)
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
        (let** _astate, (aw_array, _history) = PulseOperations.eval path Read loc arr astate in
         let++ _astate, (aw_index, _history) = PulseOperations.eval path Read loc index astate in
         let topl_event = PulseTopl.ArrayWrite {aw_array; aw_index} in
         AbductiveDomain.Topl.small_step loc topl_event astate )
        |> PulseOperationResult.sat_ok
        |> (* don't emit Topl event if evals fail *) Option.value ~default:astate
    | _ ->
        astate


  (* assume that virtual calls are only made on instance methods where it makes sense, in which case
     the receiver is always the first argument if present *)
  let get_receiver _proc_name actuals =
    match actuals with receiver :: _ -> Some receiver | _ -> None


  let get_dynamic_type_name astate v =
    match AbductiveDomain.AddressAttributes.get_dynamic_type_source_file v astate with
    | Some ({desc= Tstruct name}, source_file_opt) ->
        Some (name, source_file_opt)
    | Some (t, _) ->
        L.d_printfln "dynamic type %a of %a is not a Tstruct" (Typ.pp_full Pp.text) t
          AbstractValue.pp v ;
        None
    | None ->
        L.d_printfln "no dynamic type found for %a" AbstractValue.pp v ;
        None


  let find_override tenv astate actuals proc_name proc_name_opt =
    let open IOption.Let_syntax in
    let* {ProcnameDispatcher.Call.FuncArg.arg_payload= receiver, _} =
      get_receiver proc_name actuals
    in
    let* dynamic_type_name, source_file_opt = get_dynamic_type_name astate receiver in
    let* type_name = Procname.get_class_type_name proc_name in
    if Typ.Name.equal type_name dynamic_type_name then proc_name_opt
    else
      let method_exists proc_name methods = List.mem ~equal:Procname.equal methods proc_name in
      (* if we have a source file then do the look up in the (local) tenv
         for that source file instead of in the tenv for the current file *)
      let tenv = Option.bind source_file_opt ~f:Tenv.load |> Option.value ~default:tenv in
      Tenv.resolve_method ~method_exists tenv dynamic_type_name proc_name


  let resolve_virtual_call tenv astate actuals proc_name_opt =
    Option.map proc_name_opt ~f:(fun proc_name ->
        match find_override tenv astate actuals proc_name proc_name_opt with
        | Some proc_name' ->
            L.d_printfln "Dynamic dispatch: %a resolved to %a" Procname.pp proc_name Procname.pp
              proc_name' ;
            proc_name'
        | None ->
            proc_name )


  type model_search_result =
    | DoliModel of Procname.t
    | OcamlModel of (PulseModelsImport.model * Procname.t)
    | NoModel

  let rec dispatch_call_eval_args
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path ret call_exp
      actuals func_args call_loc flags astate callee_pname =
    let callee_pname =
      if flags.CallFlags.cf_virtual then resolve_virtual_call tenv astate func_args callee_pname
      else callee_pname
    in
    let astate =
      match (callee_pname, func_args) with
      | Some callee_pname, [{ProcnameDispatcher.Call.FuncArg.arg_payload= arg, _}]
        when Procname.is_std_move callee_pname ->
          AddressAttributes.add_one arg StdMoved astate
      | _, _ ->
          astate
    in
    let model =
      match callee_pname with
      | Some callee_pname -> (
        match DoliToTextual.matcher callee_pname with
        | Some procname ->
            DoliModel procname
        | None ->
            PulseModels.dispatch tenv callee_pname func_args
            |> Option.value_map ~default:NoModel ~f:(fun model -> OcamlModel (model, callee_pname))
        )
      | None ->
          (* unresolved function pointer, etc.: skip *)
          NoModel
    in
    (* do interprocedural call then destroy objects going out of scope *)
    let exec_states_res, call_was_unknown =
      match model with
      | OcamlModel (model, callee_procname) ->
          L.d_printfln "Found ocaml model for call@\n" ;
          let astate =
            let arg_values =
              List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= value, _} ->
                  value )
            in
            PulseOperations.conservatively_initialize_args arg_values astate
          in
          ( model
              { analysis_data
              ; dispatch_call_eval_args
              ; path
              ; callee_procname
              ; location= call_loc
              ; ret }
              astate
          , `KnownCall )
      | DoliModel callee_pname ->
          L.d_printfln "Found doli model %a for call@\n" Procname.pp callee_pname ;
          PerfEvent.(log (fun logger -> log_begin_event logger ~name:"pulse interproc call" ())) ;
          let r =
            interprocedural_call analysis_data path ret (Some callee_pname) call_exp func_args
              call_loc flags astate
          in
          PerfEvent.(log (fun logger -> log_end_event logger ())) ;
          r
      | NoModel ->
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
          | LatentInvalidAccess _ ) as exec_state ->
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
          List.filter_map exec_states_res ~f:(fun exec_state ->
              exec_state >>>= exec_object_out_of_scope path call_loc pvar_typ |> SatUnsat.sat )
      | None ->
          exec_states_res
    in
    if Option.exists callee_pname ~f:IRAttributes.is_no_return then
      List.filter_map exec_states_res ~f:(fun exec_state_res ->
          (let+ exec_state = exec_state_res in
           PulseSummary.force_exit_program tenv proc_desc err_log call_loc exec_state
           |> SatUnsat.sat )
          |> PulseResult.of_some )
    else exec_states_res


  let dispatch_call analysis_data path ret call_exp actuals call_loc flags astate =
    let<**> astate, callee_pname = PulseOperations.eval_proc_name path call_loc call_exp astate in
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
    let<**> astate, rev_func_args =
      PulseOperationResult.list_fold actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let++ astate, actual_evaled = PulseOperations.eval path Read call_loc actual_exp astate in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    let func_args = List.rev rev_func_args in
    dispatch_call_eval_args analysis_data path ret call_exp actuals func_args call_loc flags astate
      callee_pname


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
                    let+ self_typ, _ =
                      let* attrs = AbductiveDomain.AddressAttributes.find_opt addr astate in
                      Attributes.get_dynamic_type_source_file attrs
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
      ref_counts ([ContinueProgram astate], [])


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
        | AbortProgram _ | ExitProgram _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
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
      , PulseNonDisjunctiveOperations.mark_modified_copies_and_parameters vars astates astate_n )


  let and_is_int_if_integer_type typ v astate =
    if Typ.is_int typ then PulseArithmetic.and_is_int v astate else Sat (Ok astate)


  let check_modified_before_dtor args call_exp astate astate_n =
    match ((call_exp : Exp.t), args) with
    | (Const (Cfun proc_name) | Closure {name= proc_name}), (Exp.Lvar pvar, _) :: _
      when Procname.is_destructor proc_name ->
        let var = Var.of_pvar pvar in
        PulseNonDisjunctiveOperations.mark_modified_copies_and_parameters_on_abductive [var] astate
          astate_n
        |> NonDisjDomain.checked_via_dtor var
    | _ ->
        astate_n


  let check_config_usage {InterproceduralAnalysis.proc_desc} loc exp astate =
    let pname = Procdesc.get_proc_name proc_desc in
    let trace = Trace.Immediate {location= loc; history= ValueHistory.epoch} in
    Sequence.fold (Exp.free_vars exp) ~init:(Ok astate) ~f:(fun acc var ->
        Option.value_map (PulseOperations.read_id var astate) ~default:acc ~f:(fun addr_hist ->
            let* acc in
            PulseOperations.check_used_as_branch_cond addr_hist ~pname_using_config:pname
              ~branch_location:loc ~location:loc trace acc ) )


  let set_global_astates path ({InterproceduralAnalysis.proc_desc} as analysis_data) exp typ loc
      astate =
    let is_global_constant pvar =
      Pvar.(is_global pvar && (is_const pvar || is_compile_constant pvar))
    in
    let is_global_func_pointer pvar =
      Pvar.is_global pvar && Typ.is_pointer_to_function typ
      && Config.pulse_inline_global_init_func_pointer
    in
    match (exp : Exp.t) with
    | Lvar pvar when is_global_constant pvar || is_global_func_pointer pvar -> (
      (* Inline initializers of global constants or globals function pointers when they are being used.
         This addresses nullptr false positives by pruning infeasable paths global_var != global_constant_value,
         where global_constant_value is the value of global_var *)
      (* TODO: Initial global constants only once *)
      match Pvar.get_initializer_pname pvar with
      | Some init_pname when not (Procname.equal (Procdesc.get_proc_name proc_desc) init_pname) ->
          L.d_printfln_escaped "Found initializer for %a" (Pvar.pp Pp.text) pvar ;
          let call_flags = CallFlags.default in
          let ret_id_void = (Ident.create_fresh Ident.knormal, StdTyp.void) in
          let no_error_states =
            dispatch_call analysis_data path ret_id_void (Const (Cfun init_pname)) [] loc call_flags
              astate
            |> List.filter_map ~f:(function
                 | Ok (ContinueProgram astate) ->
                     Some astate
                 | _ ->
                     (* ignore errors in global initializers *)
                     None )
          in
          if List.is_empty no_error_states then [astate] else no_error_states
      | _ ->
          [astate] )
    | _ ->
        [astate]


  let exec_instr_aux ({PathContext.timestamp} as path) (astate : ExecutionDomain.t)
      (astate_n : NonDisjDomain.t)
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) _cfg_node
      (instr : Sil.instr) : ExecutionDomain.t list * PathContext.t * NonDisjDomain.t =
    match astate with
    | AbortProgram _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
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
            (let** astate, rhs_addr_hist = PulseOperations.eval_deref path loc rhs_exp astate in
             and_is_int_if_integer_type typ (fst rhs_addr_hist) astate
             >>|| PulseOperations.write_id lhs_id rhs_addr_hist )
            |> SatUnsat.to_list
            |> PulseReport.report_results tenv proc_desc err_log loc
          in
          let astates = set_global_astates path analysis_data rhs_exp typ loc astate in
          let astate_n =
            match rhs_exp with
            | Lvar pvar ->
                NonDisjDomain.set_load loc timestamp lhs_id (Var.of_pvar pvar) astate_n
            | _ ->
                astate_n
          in
          (List.concat_map astates ~f:deref_rhs, path, astate_n)
      | Store {e1= lhs_exp; e2= rhs_exp; loc; typ} ->
          (* [*lhs_exp := rhs_exp] *)
          let event =
            match lhs_exp with
            | Lvar v when Pvar.is_return v ->
                ValueHistory.Returned (loc, timestamp)
            | _ ->
                ValueHistory.Assignment (loc, timestamp)
          in
          let astate_n =
            Exp.program_vars lhs_exp
            |> Sequence.fold ~init:astate_n ~f:(fun astate_n pvar ->
                   NonDisjDomain.set_store loc timestamp pvar astate_n )
          in
          let result =
            let<**> astate, (rhs_addr, rhs_history) =
              PulseOperations.eval path NoAccess loc rhs_exp astate
            in
            let<**> ls_astate_lhs_addr_hist =
              let++ astate, lhs_addr_hist = PulseOperations.eval path Write loc lhs_exp astate in
              [Ok (astate, lhs_addr_hist)]
            in
            let hist = ValueHistory.sequence ~context:path.conditions event rhs_history in
            let astates =
              List.concat_map ls_astate_lhs_addr_hist ~f:(fun result ->
                  let<*> astate, lhs_addr_hist = result in
                  let<**> astate = and_is_int_if_integer_type typ rhs_addr astate in
                  [ PulseOperations.write_deref path loc ~ref:lhs_addr_hist ~obj:(rhs_addr, hist)
                      astate ] )
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
            List.fold actuals ~init:[astate] ~f:(fun astates (exp, typ) ->
                List.concat_map astates ~f:(fun astate ->
                    set_global_astates path analysis_data exp typ loc astate ) )
          in
          let astates =
            List.concat_map astates ~f:(fun astate ->
                dispatch_call analysis_data path ret call_exp actuals loc call_flags astate )
            |> PulseReport.report_exec_results tenv proc_desc err_log loc
          in
          let astate_n, astates =
            PulseNonDisjunctiveOperations.call tenv proc_desc path loc ~call_exp ~actuals astates
              astate_n
          in
          let astate_n = NonDisjDomain.set_passed_to loc timestamp call_exp actuals astate_n in
          (astates, path, astate_n)
      | Prune (condition, loc, is_then_branch, if_kind) ->
          let prune_result =
            let=* astate = check_config_usage analysis_data loc condition astate in
            PulseOperations.prune path loc ~condition astate
          in
          let path =
            match PulseOperationResult.sat_ok prune_result with
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
            let<++> astate, _ = prune_result in
            astate
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
        (* If we'd not compact, then heap remains big, and we'll keep skipping procedures until
           the runtime decides to compact. *)
        Gc.compact () ;
        raise_notrace AboutToOOM
    | _ ->
        () ) ;
    let astates, path, astate_n =
      exec_instr_aux path astate astate_n analysis_data cfg_node instr
    in
    ( List.map astates ~f:(fun exec_state -> (exec_state, PathContext.post_exec_instr path))
    , astate_n )


  let pp_session_name _node fmt = F.pp_print_string fmt "Pulse"
end

module Out = struct
  let channel_ref = ref None

  let channel () =
    let output_dir = Filename.concat Config.results_dir "pulse" in
    Unix.mkdir_p output_dir ;
    match !channel_ref with
    | None ->
        let filename = Format.asprintf "pulse-summary-count-%a.txt" Pid.pp (Unix.getpid ()) in
        let channel = Filename.concat output_dir filename |> Out_channel.create in
        let close_channel () =
          Option.iter !channel_ref ~f:Out_channel.close_no_err ;
          channel_ref := None
        in
        Epilogues.register ~f:close_channel ~description:"close output channel for Pulse" ;
        channel_ref := Some channel ;
        channel
    | Some channel ->
        channel
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


let initial tenv proc_name proc_attrs =
  let initial_astate =
    AbductiveDomain.mk_initial tenv proc_name proc_attrs
    |> PulseSummary.initial_with_positive_self proc_name proc_attrs
    |> PulseTaintOperations.taint_initial tenv proc_name proc_attrs
  in
  [(ContinueProgram initial_astate, PathContext.initial)]


let should_analyze proc_desc =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_id = Procname.to_unique_id proc_name in
  let f regex = not (Str.string_match regex proc_id 0) in
  Option.value_map Config.pulse_skip_procedures ~f ~default:true
  && (not (Procdesc.is_kotlin proc_desc))
  && not (Procdesc.is_too_big Pulse ~max_cfg_size:Config.pulse_max_cfg_size proc_desc)


let exit_function analysis_data location posts non_disj_astate =
  let astates, astate_n =
    List.fold_left posts ~init:([], non_disj_astate)
      ~f:(fun (acc_astates, astate_n) (exec_state, path) ->
        match exec_state with
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
    let proc_name = Procdesc.get_proc_name proc_desc in
    let proc_attrs = Procdesc.get_attributes proc_desc in
    let initial_disjuncts = initial tenv proc_name proc_attrs in
    let initial_non_disj =
      PulseNonDisjunctiveOperations.init_const_refable_parameters proc_desc tenv
        (List.map initial_disjuncts ~f:fst)
        NonDisjDomain.bottom
    in
    let initial =
      with_html_debug_node (Procdesc.get_start_node proc_desc) ~desc:"initial state creation"
        ~f:(fun () -> (initial_disjuncts, initial_non_disj))
    in
    let exit_summaries_opt, exn_sink_summaries_opt =
      DisjunctiveAnalyzer.compute_post_including_exceptional analysis_data ~initial proc_desc
    in
    let process_postconditions node posts_opt ~convert_normal_to_exceptional =
      match posts_opt with
      | Some (posts, non_disj_astate) ->
          let node_loc = Procdesc.Node.get_loc node in
          let node_id = Procdesc.Node.get_id node in
          let posts, non_disj_astate =
            (* Do final cleanup at the end of procdesc
               Forget path contexts on the way, we don't propagate them across functions *)
            exit_function analysis_data node_loc posts non_disj_astate
          in
          let posts =
            if convert_normal_to_exceptional then
              List.map posts ~f:(fun edomain ->
                  match edomain with ContinueProgram x -> ExceptionRaised x | _ -> edomain )
            else posts
          in
          let summary = PulseSummary.of_posts tenv proc_desc err_log node_loc posts in
          let is_exit_node =
            Procdesc.Node.equal_id node_id (Procdesc.Node.get_id (Procdesc.get_exit_node proc_desc))
          in
          let summary =
            if is_exit_node then
              let objc_nil_summary =
                PulseSummary.mk_objc_nil_messaging_summary tenv proc_name proc_attrs
              in
              Option.to_list objc_nil_summary @ summary
            else summary
          in
          report_topl_errors proc_desc err_log summary ;
          report_unnecessary_copies proc_desc err_log non_disj_astate ;
          report_unnecessary_parameter_copies proc_desc err_log non_disj_astate ;
          summary
      | None ->
          []
    in
    let report_on_and_return_summaries (summary : ExecutionDomain.summary list) :
        ExecutionDomain.summary list option =
      if Config.trace_topl then
        L.debug Analysis Quiet "ToplTrace: dropped %d disjuncts in %a@\n"
          !PulseTopl.Debug.dropped_disjuncts_count
          Procname.pp_unique_id
          (Procdesc.get_proc_name proc_desc) ;
      if Config.pulse_scuba_logging then
        ScubaLogging.log_count ~label:"pulse_summary" ~value:(List.length summary) ;
      let summary_count = List.length summary in
      Stats.add_pulse_summaries_count summary_count ;
      ( if Config.pulse_log_summary_count then
        let name = F.asprintf "%a" Procname.pp_verbose proc_name in
        Printf.fprintf (Out.channel ()) "%s summaries: %d\n" name summary_count ) ;
      Some summary
    in
    let exn_sink_node_opt = Procdesc.get_exn_sink proc_desc in
    let summaries_at_exn_sink : ExecutionDomain.summary list =
      (* We extract postconditions from the exceptions sink. *)
      match exn_sink_node_opt with
      | Some esink_node ->
          with_html_debug_node esink_node ~desc:"pulse summary creation (for exception sink node)"
            ~f:(fun () ->
              process_postconditions esink_node exn_sink_summaries_opt
                ~convert_normal_to_exceptional:true )
      | None ->
          []
    in
    let exit_node = Procdesc.get_exit_node proc_desc in
    with_html_debug_node exit_node ~desc:"pulse summary creation" ~f:(fun () ->
        let summaries_for_exit =
          process_postconditions exit_node exit_summaries_opt ~convert_normal_to_exceptional:false
        in
        let exit_esink_summaries = summaries_for_exit @ summaries_at_exn_sink in
        report_on_and_return_summaries exit_esink_summaries ) )
  else None


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  if should_analyze proc_desc then (
    try analyze analysis_data
    with AboutToOOM ->
      (* We trigger GC to avoid skipping the next procedure that will be analyzed. *)
      Gc.major () ;
      None )
  else None
