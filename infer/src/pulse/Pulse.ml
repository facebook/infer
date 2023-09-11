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
        let pulse_is_manifest = PulseArithmetic.is_manifest astate in
        AbductiveDomain.Topl.report_errors proc_desc err_log ~pulse_is_manifest astate
    | _ ->
        ()
  in
  List.iter ~f summary


let is_hack_async pname =
  Option.exists (IRAttributes.load pname) ~f:(fun attrs -> attrs.ProcAttributes.is_hack_async)


let is_not_implicit_or_copy_ctor_assignment pname =
  not
    (Option.exists (IRAttributes.load pname) ~f:(fun attrs ->
         attrs.ProcAttributes.is_cpp_implicit || attrs.ProcAttributes.is_cpp_copy_ctor
         || attrs.ProcAttributes.is_cpp_copy_assignment ) )


let is_non_deleted_copy pname =
  (* TODO: Default is set to true for now because we can't get the attributes of library calls right now. *)
  Option.value_map ~default:true (IRAttributes.load pname) ~f:(fun attrs ->
      attrs.ProcAttributes.is_cpp_copy_ctor && not attrs.ProcAttributes.is_cpp_deleted )


let is_type_copiable tenv typ =
  match typ with
  | {Typ.desc= Tstruct name} | {Typ.desc= Tptr ({desc= Tstruct name}, _)} -> (
    match Tenv.lookup tenv name with
    | None | Some {dummy= true} ->
        true
    | Some {methods} ->
        List.exists ~f:is_non_deleted_copy methods )
  | _ ->
      true


let get_loc_instantiated pname =
  IRAttributes.load pname |> Option.bind ~f:ProcAttributes.get_loc_instantiated


let is_copy_cted_into_var from copied_into =
  Attribute.CopiedInto.is_copied_into_var copied_into
  &&
  match (from : Attribute.CopyOrigin.t) with
  | CopyInGetDefault | CopyCtor ->
      true
  | CopyAssignment | CopyToOptional ->
      false


let report_unnecessary_copies tenv proc_desc err_log non_disj_astate =
  let pname = Procdesc.get_proc_name proc_desc in
  if is_not_implicit_or_copy_ctor_assignment pname then
    PulseNonDisjunctiveDomain.get_copied
      ~ref_formals:(Procdesc.get_passed_by_ref_formals proc_desc)
      ~ptr_formals:(Procdesc.get_pointer_formals proc_desc)
      non_disj_astate
    |> List.iter ~f:(fun (copied_into, source_typ, source_opt, location, copied_location, from) ->
           let copy_name = Format.asprintf "%a" Attribute.CopiedInto.pp copied_into in
           let is_suppressed = PulseNonDisjunctiveOperations.has_copy_in copy_name in
           let location_instantiated = get_loc_instantiated pname in
           let diagnostic =
             Diagnostic.UnnecessaryCopy
               { copied_into
               ; source_typ
               ; source_opt
               ; location
               ; copied_location
               ; location_instantiated
               ; from }
           in
           if
             is_copy_cted_into_var from copied_into
             || Option.value_map ~default:true
                  ~f:(fun typ -> Typ.is_const_reference_on_source typ |> not)
                  source_typ
           then PulseReport.report tenv ~is_suppressed ~latent:false proc_desc err_log diagnostic )


let report_unnecessary_parameter_copies tenv proc_desc err_log non_disj_astate =
  let pname = Procdesc.get_proc_name proc_desc in
  if is_not_implicit_or_copy_ctor_assignment pname then
    PulseNonDisjunctiveDomain.get_const_refable_parameters non_disj_astate
    |> List.iter ~f:(fun (param, typ, location) ->
           if is_type_copiable tenv typ then
             let diagnostic =
               if Typ.is_shared_pointer typ then
                 if NonDisjDomain.is_lifetime_extended param non_disj_astate then None
                 else
                   let used_locations = NonDisjDomain.get_loaded_locations param non_disj_astate in
                   Some
                     (Diagnostic.ReadonlySharedPtrParameter {param; typ; location; used_locations})
               else Some (Diagnostic.ConstRefableParameter {param; typ; location})
             in
             Option.iter diagnostic ~f:(fun diagnostic ->
                 PulseReport.report tenv ~is_suppressed:false ~latent:false proc_desc err_log
                   diagnostic ) )


let heap_size () = (Gc.quick_stat ()).heap_words

module PulseTransferFunctions = struct
  module CFG = ProcCfg.ExceptionalNoSinkToExitEdge
  module DisjDomain = AbstractDomain.PairDisjunct (ExecutionDomain) (PathContext)
  module NonDisjDomain = NonDisjDomain

  type analysis_data = PulseSummary.t InterproceduralAnalysis.t

  let get_pvar_formals pname =
    IRAttributes.load pname |> Option.map ~f:ProcAttributes.get_pvar_formals


  let need_closure_specialization astates =
    List.exists astates ~f:(fun res ->
        match PulseResult.ok res with
        | Some
            ( ContinueProgram {AbductiveDomain.need_closure_specialization}
            | ExceptionRaised {AbductiveDomain.need_closure_specialization} ) ->
            need_closure_specialization
        | Some
            ( ExitProgram astate
            | AbortProgram astate
            | LatentAbortProgram {astate}
            | LatentInvalidAccess {astate} ) ->
            AbductiveDomain.Summary.need_closure_specialization astate
        | None ->
            false )


  let reset_need_closure_specialization needed_closure_specialization astates =
    if needed_closure_specialization then
      List.map astates ~f:(fun res ->
          PulseResult.map res ~f:(function
            | ExceptionRaised astate ->
                let astate = AbductiveDomain.set_need_closure_specialization astate in
                ExceptionRaised astate
            | ContinueProgram astate ->
                let astate = AbductiveDomain.set_need_closure_specialization astate in
                ContinueProgram astate
            | ExitProgram astate ->
                let astate = AbductiveDomain.Summary.with_need_closure_specialization astate in
                ExitProgram astate
            | AbortProgram astate ->
                let astate = AbductiveDomain.Summary.with_need_closure_specialization astate in
                AbortProgram astate
            | LatentAbortProgram latent_abort_program ->
                let astate =
                  AbductiveDomain.Summary.with_need_closure_specialization
                    latent_abort_program.astate
                in
                LatentAbortProgram {latent_abort_program with astate}
            | LatentInvalidAccess latent_invalid_access ->
                let astate =
                  AbductiveDomain.Summary.with_need_closure_specialization
                    latent_invalid_access.astate
                in
                LatentInvalidAccess {latent_invalid_access with astate} ) )
    else astates


  let interprocedural_call
      ({InterproceduralAnalysis.analyze_dependency; tenv; proc_desc} as analysis_data) path ret
      callee_pname call_exp func_args call_loc (flags : CallFlags.t) astate =
    let actuals =
      List.map func_args ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
          (ValueOrigin.addr_hist arg_payload, typ) )
    in
    let call_kind_of call_exp =
      match call_exp with
      | Exp.Closure {captured_vars} ->
          `Closure captured_vars
      | Exp.Var id ->
          `Var id
      | _ ->
          `ResolvedProcname
    in
    let eval_args_and_call callee_pname call_exp astate =
      let formals_opt = get_pvar_formals callee_pname in
      let call_kind = call_kind_of call_exp in
      PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~analyze_dependency call_loc
        callee_pname ~ret ~actuals ~formals_opt ~call_kind astate
    in
    match callee_pname with
    | Some callee_pname when not Config.pulse_intraprocedural_only ->
        (* [needed_closure_specialization] = current function already needs specialization before
           the upcoming call (i.e. we did not have enough information to sufficiently
           specialize a callee). *)
        let needed_closure_specialization = astate.AbductiveDomain.need_closure_specialization in
        (* [astate.need_closure_specialization] is false when entering the call. This is to
           detect calls that need specialization. The value will be set back to true
           (if it was) in the end by [reset_need_closure_specialization] *)
        let astate = AbductiveDomain.unset_need_closure_specialization astate in
        let maybe_call_specialization callee_pname call_exp ((res, _, _) as call_res) =
          if (not needed_closure_specialization) && need_closure_specialization res then (
            L.d_printfln "Trying to closure-specialize %a" Exp.pp call_exp ;
            match
              PulseClosureSpecialization.make_specialized_call_exp analysis_data
                (ValueOrigin.addr_hist_args func_args)
                callee_pname (call_kind_of call_exp) path call_loc astate
            with
            | Some (callee_pname, call_exp, astate) ->
                L.d_printfln "Succesfully closure-specialized %a@\n" Exp.pp call_exp ;
                let call_res = eval_args_and_call callee_pname call_exp astate in
                (callee_pname, call_exp, call_res)
            | None ->
                L.d_printfln "Failed to closure-specialize %a@\n" Exp.pp call_exp ;
                (callee_pname, call_exp, call_res) )
          else (callee_pname, call_exp, call_res)
        in
        let _, _, (res, _, is_known_call) =
          eval_args_and_call callee_pname call_exp astate
          |> maybe_call_specialization callee_pname call_exp
        in
        (reset_need_closure_specialization needed_closure_specialization res, is_known_call)
    | _ ->
        (* dereference call expression to catch nil issues *)
        ( (let<**> astate, _ =
             if flags.cf_is_objc_block then
               (* We are on an unknown block call, meaning that the block was defined
                  outside the current function and was either passed by the caller
                  as an argument or retrieved from an object. We do not handle blocks
                  inside objects yet so we assume we are in the former case. In this
                  case, we tell the caller that we are missing some information by
                  setting [need_closure_specialization] in the resulting state and the caller
                  will then try to specialize the current function with its available
                  information. *)
               let astate = AbductiveDomain.set_need_closure_specialization astate in
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
                 then AbductiveDomain.set_need_closure_specialization astate
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
             PulseCallOperations.unknown_call tenv path call_loc (SkippedUnknownCall call_exp)
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
        let++ astate, out_of_scope_base =
          PulseOperations.eval path NoAccess call_loc (Exp.Lvar pvar) astate
        in
        (* invalidate [&x] *)
        PulseOperations.invalidate path
          (StackAddress (Var.of_pvar pvar, ValueHistory.epoch))
          call_loc gone_out_of_scope out_of_scope_base astate
        |> ExecutionDomain.continue
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


  let find_override exe_env tenv astate receiver proc_name proc_name_opt =
    let tenv_resolve_method tenv type_name proc_name =
      let method_exists proc_name methods = List.mem ~equal:Procname.equal methods proc_name in
      Tenv.resolve_method ~method_exists tenv type_name proc_name
    in
    let open IOption.Let_syntax in
    match get_dynamic_type_name astate receiver with
    | Some (dynamic_type_name, _) when Typ.Name.Hack.is_generated_curry dynamic_type_name ->
        (* this is a ...$static_...$curry'.__invoke() call to a Hack function reference *)
        let+ class_name, function_name = Typ.Name.Hack.extract_curry_info dynamic_type_name in
        let arity = Procname.get_hack_arity proc_name in
        let proc_name = Procname.make_hack ~class_name:(Some class_name) ~function_name ~arity in
        L.d_printfln "function pointer on %a detected" Procname.pp proc_name ;
        (* TODO (dpichardie): we need to modify the first argument because this is not the expected class object *)
        (Tenv.MethodInfo.mk_class proc_name, `HackFunctionReference)
    | Some (dynamic_type_name, source_file_opt) ->
        (* if we have a source file then do the look up in the (local) tenv
           for that source file instead of in the tenv for the current file *)
        let tenv =
          Option.bind source_file_opt ~f:(Exe_env.get_source_tenv exe_env)
          |> Option.value ~default:tenv
        in
        let+ proc_name = tenv_resolve_method tenv dynamic_type_name proc_name in
        (proc_name, `ExactDevirtualization)
    | None ->
        let+ proc_name =
          if Language.curr_language_is Hack || Language.curr_language_is Python then
            (* contrary to the Java frontend, the Hack and Python frontends do not perform
               static resolution so we have to do it here *)
            let* type_name = Procname.get_class_type_name proc_name in
            tenv_resolve_method tenv type_name proc_name
          else Option.map ~f:Tenv.MethodInfo.mk_class proc_name_opt
        in
        (proc_name, `ApproxDevirtualization)


  let string_of_devirtualization_status = function
    | `ExactDevirtualization ->
        "exactly"
    | `HackFunctionReference ->
        "exactly (through a Hack function reference)"
    | `ApproxDevirtualization ->
        "approximately"


  let resolve_virtual_call exe_env tenv astate receiver proc_name_opt =
    Option.map proc_name_opt ~f:(fun proc_name ->
        match find_override exe_env tenv astate receiver proc_name proc_name_opt with
        | Some (info, devirtualization_status) ->
            let proc_name' = Tenv.MethodInfo.get_procname info in
            L.d_printfln "Dynamic dispatch: %a %s resolved to %a" Procname.pp_verbose proc_name
              (string_of_devirtualization_status devirtualization_status)
              Procname.pp_verbose proc_name' ;
            (info, devirtualization_status)
        | None ->
            (Tenv.MethodInfo.mk_class proc_name, `ApproxDevirtualization) )


  let need_dynamic_type_specialization astate receiver_addr =
    AbductiveDomain.add_need_dynamic_type_specialization receiver_addr astate


  (* Hack static methods can be overriden so we need class hierarchy walkup *)
  let resolve_hack_static_method path loc astate tenv proc_name opt_callee_pname =
    let resolve_method tenv type_name proc_name =
      let equal pname1 pname2 =
        String.equal (Procname.get_method pname1) (Procname.get_method pname2)
      in
      let is_already_resolved proc_name =
        (* these methods will never be found in Tenv but we can assume they exist *)
        Option.exists (Procname.get_class_type_name proc_name) ~f:(fun class_name ->
            List.mem ~equal:Typ.Name.equal
              [ TextualSil.hack_mixed_type_name
              ; TextualSil.hack_mixed_static_companion_type_name
              ; TextualSil.hack_builtins_type_name
              ; TextualSil.hack_root_type_name ]
              class_name )
      in
      let method_exists proc_name methods = List.mem ~equal methods proc_name in
      if is_already_resolved proc_name then (
        L.d_printfln "always_implemented %a" Procname.pp proc_name ;
        Some (Tenv.MethodInfo.mk_class proc_name) )
      else Tenv.resolve_method ~method_exists tenv type_name proc_name
    in
    (* In a Hack trait, try to replace [__self__$static] with the static class name where the
       [use] of the trait was located. This information is stored in the additional [self]
       argument hackc added to the trait. *)
    let resolve_self_in_trait astate static_class_name =
      let mangled = Mangled.from_string "self" in
      (* pvar is &self, we need to dereference it to access its dynamic type *)
      let pvar = Pvar.mk mangled proc_name in
      let astate, value = PulseOperations.eval_var path loc pvar astate in
      match
        PulseOperations.eval_access path Read loc value Dereference astate |> PulseResult.ok
      with
      | None ->
          (Some static_class_name, astate)
      | Some (astate, (value, _)) -> (
        match AbductiveDomain.AddressAttributes.get_dynamic_type value astate with
        | None ->
            (* No information is available from the [self] argument at this time, we need to
               wait for specialization *)
            (None, need_dynamic_type_specialization astate value)
        | Some typ ->
            (Typ.name typ, astate) )
    in
    (* If we spot a call on [__parent__$static], we push further and get the parent of
       [__self__$static] *)
    let resolve_parent_in_trait astate static_class_name =
      let self_ty_name, astate = resolve_self_in_trait astate static_class_name in
      (* Now that we have the [self] type, locate its parent *)
      let parent = Option.bind self_ty_name ~f:(Tenv.get_parent tenv) in
      (parent, astate)
    in
    let trait_resolution astate static_class_name =
      let maybe_origin = Typ.Name.Hack.static_companion_origin static_class_name |> Typ.Name.name in
      if String.equal "__self__" maybe_origin then resolve_self_in_trait astate static_class_name
      else if String.equal "__parent__" maybe_origin then
        resolve_parent_in_trait astate static_class_name
      else (Some static_class_name, astate)
    in
    (* Similar to IOption.Let_syntax but threading [astate] along the way *)
    let ( let* ) (opt, env) f = match opt with None -> (None, env) | Some v -> f (v, env) in
    let* callee_pname, astate = (opt_callee_pname, astate) in
    match Procname.get_class_type_name callee_pname with
    | None ->
        (Some (Tenv.MethodInfo.mk_class callee_pname), astate)
    | Some static_class_name ->
        let* static_class_name, astate = trait_resolution astate static_class_name in
        L.d_printfln "hack static dispatch from %a in class name %a" Procname.pp callee_pname
          Typ.Name.pp static_class_name ;
        (resolve_method tenv static_class_name callee_pname, astate)


  let improve_receiver_static_type astate receiver proc_name_opt =
    if Language.curr_language_is Hack || Language.curr_language_is Python then
      let open IOption.Let_syntax in
      let* proc_name = proc_name_opt in
      match AbductiveDomain.AddressAttributes.get_static_type receiver astate with
      | Some typ_name ->
          let improved_proc_name = Procname.replace_class proc_name typ_name in
          L.d_printfln "Propagating declared type to improve callee name: %a replaced by %a"
            Procname.pp proc_name Procname.pp improved_proc_name ;
          Some improved_proc_name
      | _ ->
          proc_name_opt
    else proc_name_opt


  type model_search_result =
    | DoliModel of Procname.t
    | OcamlModel of (PulseModelsImport.model * Procname.t)
    | NoModel

  (* When Hack traits are involved, we need to compute and pass an additional argument that is a
     token to find the right class name for [self].

     [hackc] adds [self] argument at the end of the signature. *)
  let add_self_for_hack_traits path location astate method_info func_args =
    let hack_kind = Option.bind method_info ~f:Tenv.MethodInfo.get_hack_kind in
    match hack_kind with
    | Some (IsTrait {used}) ->
        let exp, arg_payload, astate =
          let arg_payload, astate =
            PulseModelsHack.get_static_companion ~model_desc:"add_self_for_hack_traits" path
              location used astate
          in
          let self_id = Ident.create_fresh Ident.kprimed in
          let astate = PulseOperations.write_id self_id arg_payload astate in
          (Exp.Var self_id, arg_payload, astate)
        in
        let static_used = Typ.Name.Hack.static_companion used in
        let typ = Typ.mk_struct static_used |> Typ.mk_ptr in
        let self =
          {PulseAliasSpecialization.FuncArg.exp; typ; arg_payload= ValueOrigin.Unknown arg_payload}
        in
        (astate, func_args @ [self])
    | Some IsClass | None ->
        (astate, func_args)


  let modify_receiver_if_hack_function_reference path location astate func_args =
    let open IOption.Let_syntax in
    ( match func_args with
    | ({ProcnameDispatcher.Call.FuncArg.arg_payload= value} as arg) :: args ->
        let function_addr_hist = ValueOrigin.addr_hist value in
        let* dynamic_type_name, _ = function_addr_hist |> fst |> get_dynamic_type_name astate in
        if Typ.Name.Hack.is_generated_curry dynamic_type_name then
          let this_field = Fieldname.make dynamic_type_name "this" in
          let+ astate, class_object =
            PulseOperations.eval_deref_access path Read location function_addr_hist
              (FieldAccess this_field) astate
            |> PulseResult.ok
          in
          (astate, {arg with arg_payload= ValueOrigin.unknown class_object} :: args)
        else None
    | [] ->
        None )
    |> Option.value ~default:(astate, func_args)


  let lookup_virtual_method_info {InterproceduralAnalysis.tenv; proc_desc; exe_env} path func_args
      call_loc astate callee_pname default_info =
    match get_receiver callee_pname func_args with
    | None ->
        L.internal_error "No receiver on virtual call" ;
        (default_info, astate)
    | Some {ProcnameDispatcher.Call.FuncArg.arg_payload= receiver} -> (
      match
        improve_receiver_static_type astate (ValueOrigin.value receiver) callee_pname
        |> resolve_virtual_call exe_env tenv astate (ValueOrigin.value receiver)
      with
      | Some (info, `HackFunctionReference) ->
          let caller_proc_name = Procdesc.get_proc_name proc_desc in
          let proc_name = Tenv.MethodInfo.get_procname info in
          let info, astate =
            match
              resolve_hack_static_method path call_loc astate tenv caller_proc_name (Some proc_name)
            with
            | Some info, astate ->
                (info, astate)
            | None, _ ->
                (info, astate)
          in
          (Some info, astate)
      | Some (info, `ExactDevirtualization) ->
          (Some info, astate)
      | Some (info, `ApproxDevirtualization) ->
          (Some info, need_dynamic_type_specialization astate (ValueOrigin.value receiver))
      | None ->
          (None, astate) )


  (** Check whether a Python class was declared with an explicit constructor, or if it was left
      implicit. *)
  let is_python_class_with_implicit_init tenv class_name =
    let is_python_init procname =
      match Procname.python_classify procname with
      | None ->
          false
      | Some kind -> (
        match (kind : Procname.Python.kind) with Init _ -> true | Fun _ | Other -> false )
    in
    Tenv.lookup tenv (PythonClass class_name)
    |> Option.for_all ~f:(fun {Struct.methods} -> not (List.exists methods ~f:is_python_init))


  (* If a Python class doesn't define a [__init__] method, we have to take over and provide a valid
     implementation. Consider:

     # file1.py
     class A:
     def __init__(self, x):
       self.x = x

     # file2.py
     import file1.A

     class B(A):
     pass # implicit constructor/__init__

     # file3.py
     b = B(42)

     When looking for [B] to analyze [B(42)], we won't find its definition, but if there is a
     class with the same name, we can take over and:
     - allocate a value of type [B]
     - use normal method resolution to find [A.__init__] and call it on this new value

     TODO(vsiles) if a genuine toplevel function is named like a class (e.g.  [B] in this example),
     we should not do anything as it will shadow the class constructor. But
     this is only true if the definition is *after* the class, and this might
     be tricky to get right. For now, let's do it all the time
  *)
  let dispatch_python_constructor dispatch_call_eval_args
      ({InterproceduralAnalysis.tenv} as analysis_data) path ret actuals func_args call_loc astate
      callee_pname class_name default_info =
    let type_name = Typ.PythonClass class_name in
    let model_data =
      { PulseModelsImport.analysis_data
      ; dispatch_call_eval_args
      ; path
      ; callee_procname= callee_pname
      ; location= call_loc
      ; ret }
    in
    let size =
      Exp.Sizeof
        {typ= Typ.mk_struct type_name; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
    in
    (* Since the constructor is implicit, we first have to allocate a value of the correct type. *)
    match
      PulseModelsImport.Basic.alloc_no_leak_not_null ~desc:"Implicit Python constructor allocation"
        (Some size) ~initialize:false model_data astate
      |> PulseOperationResult.sat_ok
    with
    | None ->
        (default_info, ret, actuals, func_args, astate)
    | Some astate ->
        (* Now that the newly allocated value is bound to [ret] we have to:
           - update the function arguments / actuals to insert [ret] as the first (self) argument.
           - create a new id to bind the return value of [__init__] (which will always be None)
        *)
        let astate, self = PulseOperations.eval_ident (fst ret) astate in
        (* Also register its static type *)
        let astate =
          AbductiveDomain.AddressAttributes.add_static_type tenv type_name (fst self) astate
        in
        let self_origin = ValueOrigin.OnStack {var= Var.of_id (fst ret); addr_hist= self} in
        let ret_exp, ret_ty = ret in
        (* Actual arguments updated *)
        let self = (Exp.Var ret_exp, ret_ty) in
        let actuals = self :: actuals in
        (* Formal arguments updated *)
        let self =
          {ProcnameDispatcher.Call.FuncArg.arg_payload= self_origin; exp= fst self; typ= snd self}
        in
        let func_args = self :: func_args in
        (* Creating a new binding for the return value of [__init__].
           We never read back the return value of [__init__] as it is [None].
           However some constructors seem to return a closure (when
           inheritance is involved). Not sure we will ever do that, just
           leaving a note in case we need this in the future *)
        let new_ret = Ident.create_fresh Ident.kprimed in
        let new_ret_typ =
          let name = PythonClassName.make "PyNone" in
          let name = Typ.PythonClass name in
          let t = Typ.mk_struct name in
          Typ.mk_ptr t
        in
        let new_ret = (new_ret, new_ret_typ) in
        (* Now, we need to resume method resolution looking for [__init__], not the original
           constructor, so we'll provide a new [callee_pname] too *)
        let callee_pname = Procname.mk_python_init callee_pname in
        (* We know [__init__] will be in a parent class, so let virtual lookup do its job *)
        let method_info, astate =
          lookup_virtual_method_info analysis_data path func_args call_loc astate
            (Some callee_pname) default_info
        in
        (method_info, new_ret, actuals, func_args, astate)


  (* If a Python constructor is missing, we have to correctly dispatch calls to the constructor,
     but also to the [__init__] methods. *)
  let dispatch_python_constructor_and_init dispatch_call_eval_args
      ({InterproceduralAnalysis.tenv} as analysis_data) path ret actuals func_args call_loc astate
      callee_pname default_info =
    let return info astate = (info, ret, actuals, func_args, astate) in
    match Option.bind ~f:Procname.python_classify callee_pname with
    | Some (Init class_name) ->
        (* This case might happen when there is an explicit call (like [super().__init__()])
           to a parent class which didn't have an explicit constructor *)
        let has_implicit_constructor = is_python_class_with_implicit_init tenv class_name in
        if not has_implicit_constructor then return default_info astate
        else
          let info, astate =
            lookup_virtual_method_info analysis_data path func_args call_loc astate callee_pname
              default_info
          in
          return info astate
    | Some (Fun class_name) ->
        let has_implicit_constructor = is_python_class_with_implicit_init tenv class_name in
        if not has_implicit_constructor then return default_info astate
        else
          let callee_pname = Option.value_exn callee_pname in
          dispatch_python_constructor dispatch_call_eval_args analysis_data path ret actuals
            func_args call_loc astate callee_pname class_name default_info
    | Some Other | None ->
        return default_info astate


  let rec dispatch_call_eval_args
      ({InterproceduralAnalysis.tenv; proc_desc; err_log} as analysis_data) path ret call_exp
      actuals func_args call_loc flags astate callee_pname =
    let method_info, ret, actuals, func_args, astate =
      let default_info = Option.map ~f:Tenv.MethodInfo.mk_class callee_pname in
      if flags.CallFlags.cf_virtual then
        let method_info, astate =
          lookup_virtual_method_info analysis_data path func_args call_loc astate callee_pname
            default_info
        in
        (method_info, ret, actuals, func_args, astate)
      else if Language.curr_language_is Hack then
        (* In Hack, a static method can be inherited.
           TODO(vsiles) this is the case for Python too, update this when the time is right *)
        let proc_name = Procdesc.get_proc_name proc_desc in
        let info, astate =
          resolve_hack_static_method path call_loc astate tenv proc_name callee_pname
        in
        (* Don't drop the initial [callee_pname]: even though we couldn't refine it, we can still
           use it to match against taint configs and such. *)
        (Option.first_some info default_info, ret, actuals, func_args, astate)
      else if Language.curr_language_is Python then
        (* In Python, we need to be careful about implicit class constructors *)
        dispatch_python_constructor_and_init dispatch_call_eval_args analysis_data path ret actuals
          func_args call_loc astate callee_pname default_info
      else (default_info, ret, actuals, func_args, astate)
    in
    let callee_pname = Option.map ~f:Tenv.MethodInfo.get_procname method_info in
    let astate, func_args = add_self_for_hack_traits path call_loc astate method_info func_args in
    let astate, func_args =
      modify_receiver_if_hack_function_reference path call_loc astate func_args
    in
    let astate =
      match (callee_pname, func_args) with
      | Some callee_pname, [{ProcnameDispatcher.Call.FuncArg.arg_payload= arg}]
        when Procname.is_std_move callee_pname ->
          AddressAttributes.add_one (ValueOrigin.value arg) StdMoved astate
      | _, _ ->
          astate
    in
    let astate =
      List.fold func_args ~init:astate
        ~f:(fun acc {ProcnameDispatcher.Call.FuncArg.arg_payload= arg; exp} ->
          match exp with
          | Cast (typ, _) when Typ.is_rvalue_reference typ ->
              AddressAttributes.add_one (ValueOrigin.value arg) StdMoved acc
          | _ ->
              acc )
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
              List.map func_args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= value} ->
                  ValueOrigin.value value )
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
              let astate_after_call =
                (* TODO: move to PulseModelsHack *)
                match callee_pname with
                | Some proc_name when is_hack_async proc_name -> (
                    L.d_printfln "did return from asynccall of %a, ret=%a" Procname.pp proc_name
                      Ident.pp (fst ret) ;
                    match PulseOperations.read_id (fst ret) astate with
                    | None ->
                        L.d_printfln "couldn't find ret in state" ;
                        astate
                    | Some (rv, _) ->
                        L.d_printfln "return value %a" AbstractValue.pp rv ;
                        PulseOperations.allocate Attribute.HackAsync call_loc rv astate )
                | _ ->
                    astate
              in
              PulseTaintOperations.call tenv path call_loc ret ~call_was_unknown call_event
                func_args astate_after_call
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
            topl_small_step call_loc callee_pname
              (ValueOrigin.addr_hist_args func_args)
              ret exec_states_res
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


  let eval_function_call_args path call_exp actuals call_loc astate =
    let** astate, callee_pname = PulseOperations.eval_proc_name path call_loc call_exp astate in
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
    let++ astate, rev_actuals =
      PulseOperationResult.list_fold actuals ~init:(astate, [])
        ~f:(fun (astate, rev_func_args) (actual_exp, actual_typ) ->
          let++ astate, actual_evaled =
            PulseOperations.eval_to_value_origin path Read call_loc actual_exp astate
          in
          ( astate
          , ProcnameDispatcher.Call.FuncArg.
              {exp= actual_exp; arg_payload= actual_evaled; typ= actual_typ}
            :: rev_func_args ) )
    in
    (astate, call_exp, callee_pname, List.rev rev_actuals)


  let dispatch_call analysis_data path ret call_exp actuals call_loc flags astate =
    let<**> astate, call_exp, callee_pname, func_args =
      eval_function_call_args path call_exp actuals call_loc astate
    in
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
    List.filter_map astates ~f:(fun (exec_state : ExecutionDomain.t) ->
        match exec_state with
        | AbortProgram _ | ExitProgram _ | LatentAbortProgram _ | LatentInvalidAccess _ ->
            Some exec_state
        | ContinueProgram astate -> (
          match PulseOperations.remove_vars vars location astate with
          | Sat astate ->
              Some (ContinueProgram astate)
          | Unsat ->
              None )
        | ExceptionRaised astate -> (
          match PulseOperations.remove_vars vars location astate with
          | Sat astate ->
              Some (ExceptionRaised astate)
          | Unsat ->
              None ) )


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
      ({InterproceduralAnalysis.tenv; proc_desc; err_log; exe_env} as analysis_data) _cfg_node
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
          let model_opt = PulseLoadInstrModels.dispatch ~load:rhs_exp in
          let deref_rhs astate =
            (let** astate, rhs_addr_hist =
               match model_opt with
               | None ->
                   (* no model found: evaluate the expression as normal *)
                   PulseOperations.eval_deref path loc rhs_exp astate
               | Some model ->
                   (* we are loading from something modelled; apply the model *)
                   model {path; location= loc} astate
             in
             let rhs_addr, _ = rhs_addr_hist in
             and_is_int_if_integer_type typ rhs_addr astate
             >>|| PulseOperations.hack_python_propagates_type_on_load tenv path loc rhs_exp rhs_addr
             >>|| PulseOperations.write_id lhs_id rhs_addr_hist )
            |> SatUnsat.to_list
            |> PulseReport.report_results tenv proc_desc err_log loc
          in
          let astates =
            (* call the initializer for certain globals to populate their values, unless we already
               have a model for it *)
            if Option.is_some model_opt then [astate]
            else set_global_astates path analysis_data rhs_exp typ loc astate
          in
          let astate_n =
            match rhs_exp with
            | Lvar pvar ->
                NonDisjDomain.set_load loc timestamp lhs_id (Var.of_pvar pvar) astate_n
            | _ ->
                astate_n
          in
          let astates, path, non_disj = (List.concat_map astates ~f:deref_rhs, path, astate_n) in
          let astates =
            let procname = Procdesc.get_proc_name proc_desc in
            List.concat_map astates ~f:(fun astate ->
                match astate with
                | ContinueProgram astate ->
                    let astates =
                      [ PulseTaintOperations.load procname tenv path loc ~lhs:(lhs_id, typ)
                          ~rhs:rhs_exp astate ]
                    in
                    PulseReport.report_results tenv proc_desc err_log loc astates
                | _ ->
                    [astate] )
          in
          (astates, path, non_disj)
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
            let** astate, rhs_value_origin =
              PulseOperations.eval_to_value_origin path NoAccess loc rhs_exp astate
            in
            let rhs_addr, rhs_history = ValueOrigin.addr_hist rhs_value_origin in
            let** astate, lhs_addr_hist = PulseOperations.eval path Write loc lhs_exp astate in
            let hist = ValueHistory.sequence ~context:path.conditions event rhs_history in
            let** astate = and_is_int_if_integer_type typ rhs_addr astate in
            let=* astate =
              PulseTaintOperations.store tenv path loc ~lhs:lhs_exp
                ~rhs:(rhs_exp, rhs_value_origin, typ) astate
            in
            let=+ astate =
              PulseOperations.write_deref path loc ~ref:lhs_addr_hist ~obj:(rhs_addr, hist) astate
            in
            let astate =
              if Topl.is_active () then topl_store_step path loc ~lhs:lhs_exp ~rhs:rhs_exp astate
              else astate
            in
            match lhs_exp with
            | Lvar pvar when Pvar.is_return pvar ->
                PulseOperations.check_address_escape loc proc_desc rhs_addr rhs_history astate
            | _ ->
                Ok astate
          in
          let astate_n = NonDisjDomain.set_captured_variables rhs_exp astate_n in
          let results = SatUnsat.to_list result in
          (PulseReport.report_results tenv proc_desc err_log loc results, path, astate_n)
      | Call (ret, call_exp, actuals, loc, call_flags) ->
          let astate_n = check_modified_before_dtor actuals call_exp astate astate_n in
          let astates =
            List.fold actuals ~init:[astate] ~f:(fun astates (exp, typ) ->
                List.concat_map astates ~f:(fun astate ->
                    set_global_astates path analysis_data exp typ loc astate ) )
          in
          (* [astates_before] are the states after we evaluate args but before we apply the callee. This is needed for PulseNonDisjunctiveOperations to determine whether we are copying from something pointed to by [this].  *)
          let astates, astates_before =
            let astates_before = ref [] in
            let res =
              List.concat_map astates ~f:(fun astate ->
                  let results_and_before_state =
                    let++ astate, call_exp, callee_pname, func_args =
                      eval_function_call_args path call_exp actuals loc astate
                    in
                    (* stash the intermediate "before" [astate] here because the result monad does
                       not accept more complicated types than lists of states (we need a pair of the
                       before astate and the list of results) *)
                    astates_before := astate :: !astates_before ;
                    dispatch_call_eval_args analysis_data path ret call_exp actuals func_args loc
                      call_flags astate callee_pname
                  in
                  let<**> r = results_and_before_state in
                  r )
            in
            let astates_before = !astates_before in
            (PulseReport.report_exec_results tenv proc_desc err_log loc res, astates_before)
          in
          let astate_n, astates =
            let pname = Procdesc.get_proc_name proc_desc in
            let integer_type_widths = Exe_env.get_integer_type_widths exe_env pname in
            PulseNonDisjunctiveOperations.call integer_type_widths tenv proc_desc path loc ~call_exp
              ~actuals ~astates_before astates astate_n
          in
          let astate_n = NonDisjDomain.set_passed_to loc timestamp call_exp actuals astate_n in
          let astate_n =
            List.fold actuals ~init:astate_n ~f:(fun astate_n (exp, _) ->
                NonDisjDomain.set_captured_variables exp astate_n )
          in
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
      | Metadata (VariableLifetimeBegins {pvar; typ; loc; is_cpp_structured_binding})
        when not (Pvar.is_global pvar) ->
          ( [ PulseOperations.realloc_pvar tenv path
                ~set_uninitialized:(not is_cpp_structured_binding) pvar typ loc astate
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

let summary_count_channel =
  lazy
    (let output_dir = Filename.concat Config.results_dir "pulse" in
     Unix.mkdir_p output_dir ;
     let filename = Format.asprintf "pulse-summary-count-%a.txt" Pid.pp (Unix.getpid ()) in
     let channel = Filename.concat output_dir filename |> Out_channel.create in
     let close_channel () = Out_channel.close_no_err channel in
     Epilogues.register ~f:close_channel ~description:"close summary_count_channel for Pulse" ;
     channel )


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


let initial tenv proc_attrs specialization =
  let initial_astate =
    AbductiveDomain.mk_initial tenv proc_attrs specialization
    |> PulseSummary.initial_with_positive_self proc_attrs
    |> PulseTaintOperations.taint_initial tenv proc_attrs
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
        | AbortProgram _
        | ExitProgram _
        | ExceptionRaised _
        | LatentAbortProgram _
        | LatentInvalidAccess _ ->
            (exec_state :: acc_astates, astate_n)
        | ContinueProgram astate ->
            let vars =
              Stack.fold
                (fun var _ vars -> if Var.is_return var then vars else var :: vars)
                astate []
            in
            let astates, _, astate_n =
              PulseTransferFunctions.exit_scope vars location path astate astate_n analysis_data
            in
            (PulseTransferFunctions.remove_vars vars location astates @ acc_astates, astate_n) )
  in
  (List.rev astates, astate_n)


let log_summary_count proc_name summary =
  let counts =
    let summary_kinds = List.map ~f:ExecutionDomain.to_name summary in
    let map =
      let incr_or_one val_opt = match val_opt with Some v -> v + 1 | None -> 1 in
      let update acc s = String.Map.update acc s ~f:incr_or_one in
      List.fold summary_kinds ~init:String.Map.empty ~f:update
    in
    let alist = List.map ~f:(fun (s, i) -> (s, `Int i)) (String.Map.to_alist map) in
    let pname = F.asprintf "%a" Procname.pp_verbose proc_name in
    `Assoc (("procname", `String pname) :: alist)
  in
  Yojson.Basic.to_channel (Lazy.force summary_count_channel) counts ;
  Out_channel.output_char (Lazy.force summary_count_channel) '\n'


let analyze specialization
    ({InterproceduralAnalysis.tenv; proc_desc; err_log; exe_env} as analysis_data) =
  let proc_name = Procdesc.get_proc_name proc_desc in
  let proc_attrs = Procdesc.get_attributes proc_desc in
  let integer_type_widths = Exe_env.get_integer_type_widths exe_env proc_name in
  let initial =
    with_html_debug_node (Procdesc.get_start_node proc_desc) ~desc:"initial state creation"
      ~f:(fun () ->
        let initial_disjuncts = initial tenv proc_attrs specialization in
        let initial_non_disj =
          PulseNonDisjunctiveOperations.init_const_refable_parameters proc_desc integer_type_widths
            tenv
            (List.map initial_disjuncts ~f:fst)
            NonDisjDomain.bottom
        in
        (initial_disjuncts, initial_non_disj) )
  in
  let invariant_map = DisjunctiveAnalyzer.exec_pdesc analysis_data ~initial proc_desc in
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
            let objc_nil_summary = PulseSummary.mk_objc_nil_messaging_summary tenv proc_attrs in
            Option.to_list objc_nil_summary @ summary
          else summary
        in
        report_topl_errors proc_desc err_log summary ;
        report_unnecessary_copies tenv proc_desc err_log non_disj_astate ;
        report_unnecessary_parameter_copies tenv proc_desc err_log non_disj_astate ;
        summary
    | None ->
        []
  in
  let report_on_and_return_summaries (summary : ExecutionDomain.summary list) :
      ExecutionDomain.summary list option =
    if Config.trace_topl then
      L.debug Analysis Quiet "ToplTrace: dropped %d disjuncts in %a@\n"
        (PulseTopl.Debug.get_dropped_disjuncts_count ())
        Procname.pp_unique_id
        (Procdesc.get_proc_name proc_desc) ;
    let summary_count = List.length summary in
    if Config.pulse_scuba_logging then
      ScubaLogging.log_count ~label:"pulse_summary" ~value:summary_count ;
    Stats.add_pulse_summaries_count summary_count ;
    if Config.pulse_log_summary_count then log_summary_count proc_name summary ;
    (* needed to record the stats corresponding to the metadata *)
    DisjunctiveAnalyzer.get_cfg_metadata () |> ignore ;
    Some summary
  in
  let exn_sink_node_opt = Procdesc.get_exn_sink proc_desc in
  let summaries_at_exn_sink : ExecutionDomain.summary list =
    (* We extract postconditions from the exceptions sink. *)
    match exn_sink_node_opt with
    | Some esink_node ->
        with_html_debug_node esink_node ~desc:"pulse summary creation (for exception sink node)"
          ~f:(fun () ->
            process_postconditions ~convert_normal_to_exceptional:true esink_node
              (DisjunctiveAnalyzer.extract_post (Procdesc.Node.get_id esink_node) invariant_map) )
    | None ->
        []
  in
  let exit_node = Procdesc.get_exit_node proc_desc in
  with_html_debug_node exit_node ~desc:"pulse summary creation" ~f:(fun () ->
      let summaries_for_exit =
        process_postconditions ~convert_normal_to_exceptional:false exit_node
          (DisjunctiveAnalyzer.extract_post (Procdesc.Node.get_id exit_node) invariant_map)
      in
      let exit_esink_summaries = summaries_for_exit @ summaries_at_exn_sink in
      report_on_and_return_summaries exit_esink_summaries )


let checker ?specialization ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let open IOption.Let_syntax in
  if should_analyze proc_desc then (
    try
      match specialization with
      | None ->
          let+ pre_post_list = analyze None analysis_data in
          {PulseSummary.main= pre_post_list; specialized= Specialization.Pulse.Map.empty}
      | Some (current_summary, Specialization.Pulse specialization) ->
          let+ pre_post_list = analyze (Some specialization) analysis_data in
          let specialized =
            Specialization.Pulse.Map.add specialization pre_post_list
              current_summary.PulseSummary.specialized
          in
          {current_summary with PulseSummary.specialized}
    with AboutToOOM ->
      (* We trigger GC to avoid skipping the next procedure that will be analyzed. *)
      Gc.major () ;
      None )
  else None


let is_already_specialized (Pulse specialization : Specialization.t) (summary : PulseSummary.t) =
  Specialization.Pulse.Map.mem specialization summary.specialized
