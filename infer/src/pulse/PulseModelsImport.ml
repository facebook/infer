(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module IRAttributes = Attributes
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import

type arg_payload = ValueOrigin.t

type model_data =
  { analysis_data: PulseSummary.t InterproceduralAnalysis.t
  ; dispatch_call_eval_args:
         PulseSummary.t InterproceduralAnalysis.t
      -> PathContext.t
      -> Ident.t * Typ.t
      -> Exp.t
      -> ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t list
      -> Location.t
      -> CallFlags.t
      -> AbductiveDomain.t
      -> NonDisjDomain.t
      -> Procname.t option
      -> ExecutionDomain.t AccessResult.t list * NonDisjDomain.t
  ; path: PathContext.t
  ; callee_procname: Procname.t
  ; location: Location.t
  ; ret: Ident.t * Typ.t }

type model_no_non_disj = model_data -> AbductiveDomain.t -> ExecutionDomain.t AccessResult.t list

type model =
     model_data
  -> AbductiveDomain.t
  -> NonDisjDomain.t
  -> ExecutionDomain.t AccessResult.t list * NonDisjDomain.t

type matcher = (Tenv.t * Procname.t, model, arg_payload) ProcnameDispatcher.Call.matcher

let lift_model model data astate non_disj = (model data astate, non_disj)

let with_non_disj model = ProcnameDispatcher.Call.map_matcher ~f:lift_model model

module Hist = struct
  let mk_desc ?more desc =
    Option.value_map ~default:desc more ~f:(fun extra_info ->
        Printf.sprintf "%s %s" desc extra_info )


  let alloc_event {PathContext.timestamp} location ?more model_desc =
    let desc = mk_desc ?more model_desc in
    ValueHistory.Allocation {f= Model desc; location; timestamp}


  let call_event {PathContext.timestamp} ?(in_call = ValueHistory.epoch) location ?more model_desc =
    let desc = mk_desc ?more model_desc in
    ValueHistory.Call {f= Model desc; location; in_call; timestamp}


  let add_event path event hist =
    ValueHistory.sequence ~context:path.PathContext.conditions event hist


  let single_event path event = add_event path event ValueHistory.epoch

  let add_call path ?(in_call = ValueHistory.epoch) location model_desc ?more hist =
    add_event path (call_event path ~in_call location ?more model_desc) hist


  let single_call path ?(in_call = ValueHistory.epoch) location ?more model_desc =
    add_call path ~in_call location model_desc ?more ValueHistory.epoch


  let single_alloc path location ?more model_desc =
    alloc_event path location ?more model_desc |> single_event path


  let binop path bop hist1 hist2 =
    ValueHistory.in_context path.PathContext.conditions (ValueHistory.binary_op bop hist1 hist2)
end

module Basic = struct
  let continue astate = ContinueProgram astate

  let ok_continue post = [Ok (ContinueProgram post)]

  let map_continue astate_result =
    let open PulseResult.Let_syntax in
    let+ astate = astate_result in
    ContinueProgram astate


  let shallow_copy_value path location event ret_id dest_pointer_hist src_value_hist astate =
    let<*> astate, obj_copy = PulseOperations.shallow_copy path location src_value_hist astate in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:dest_pointer_hist
        ~obj:(fst obj_copy, Hist.add_event path event (snd obj_copy))
        astate
    in
    PulseOperations.havoc_id ret_id (Hist.single_event path event) astate


  let shallow_copy path location event ret_id dest_pointer_hist src_pointer_hist astate =
    let<*> astate, obj =
      PulseOperations.eval_access path Read location src_pointer_hist Dereference astate
    in
    shallow_copy_value path location event ret_id dest_pointer_hist obj astate


  let shallow_copy_model model_desc dest_pointer_hist src_pointer_hist : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location model_desc in
    shallow_copy path location event ret_id dest_pointer_hist src_pointer_hist astate


  let deep_copy path location ~value ~desc astate =
    let address = (AbstractValue.mk_fresh (), Hist.add_call path location desc (snd value)) in
    let=* astate, value_deref =
      PulseOperations.eval_access path Read location value Dereference astate
    in
    let=* astate = PulseOperations.write_deref path location ~ref:address ~obj:value_deref astate in
    let++ astate = PulseArithmetic.and_positive (fst address) astate in
    (astate, address)


  let alloc_value_address (typ : Typ.t) ~desc {analysis_data; path; location} astate =
    (* alloc an address for some object *)
    let value, hist = (AbstractValue.mk_fresh (), ValueHistory.epoch) in
    let astate, (value, hist) =
      PulseTaintOperations.taint_allocation analysis_data.tenv path location ~typ_desc:typ.desc
        ~alloc_desc:desc ~allocator:(Some CppNew) (value, hist) astate
    in
    let astate =
      if Typ.is_objc_class typ then
        PulseArithmetic.and_dynamic_type_is_unsafe value typ ~source_file:location.file location
          astate
      else PulseArithmetic.and_dynamic_type_is_unsafe value typ location astate
    in
    let++ astate = PulseArithmetic.and_positive value astate in
    (astate, (value, hist))


  let early_exit : model_no_non_disj =
   fun {analysis_data= {proc_desc}; location} astate ->
    let open SatUnsat.Import in
    match
      AbductiveDomain.Summary.of_post
        (Procdesc.get_proc_name proc_desc)
        (Procdesc.get_attributes proc_desc)
        location astate
      >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_summary_result
      >>| AccessResult.with_summary
    with
    | Unsat ->
        []
    | Sat (Ok astate) ->
        [Ok (ExitProgram astate)]
    | Sat (Recoverable (astate, errors)) ->
        [Recoverable (ExitProgram astate, errors)]
    | Sat (FatalError _ as err) ->
        [err]


  let return_int ~desc : Int64.t -> model_no_non_disj =
   fun i64 {path; location; ret= ret_id, _} astate ->
    let i = IntLit.of_int64 i64 in
    let astate, ret_addr = PulseArithmetic.absval_of_int astate i in
    PulseOperations.write_id ret_id (ret_addr, Hist.single_call path location desc) astate
    |> ok_continue


  let return_positive ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, Hist.single_call path location desc) in
    let<++> astate = PulseArithmetic.and_positive ret_addr astate in
    PulseOperations.write_id ret_id ret_value astate


  let return_unknown_size ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let<++> astate = PulseArithmetic.and_nonnegative ret_addr astate in
    PulseOperations.write_id ret_id (ret_addr, Hist.single_call path location desc) astate


  (** Pretend the function call is a call to an "unknown" function, i.e. a function for which we
      don't have the implementation. This triggers a bunch of heuristics, e.g. to havoc arguments we
      suspect are passed by reference. *)
  let unknown_call skip_reason args : model_no_non_disj =
   fun {path; callee_procname; analysis_data= {tenv}; location; ret} astate ->
    let actuals =
      List.map args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= actual; typ} ->
          (actual, typ) )
    in
    let formals_opt =
      IRAttributes.load callee_procname |> Option.map ~f:ProcAttributes.get_pvar_formals
    in
    let<++> astate =
      PulseCallOperations.unknown_call tenv path location (Model skip_reason) (Some callee_procname)
        ~ret ~actuals ~formals_opt astate
    in
    astate


  let id_first_arg_from_list ~desc args : model_no_non_disj =
   fun {path; callee_procname; location; ret= ret_id, _} astate ->
    match args with
    | _ :: arg :: _
      when Procname.is_objc_instance_method callee_procname || Procname.is_java callee_procname ->
        let arg_value, arg_history = arg.ProcnameDispatcher.Call.FuncArg.arg_payload in
        let ret_value = (arg_value, Hist.add_call path location desc arg_history) in
        PulseOperations.write_id ret_id ret_value astate |> ok_continue
    | arg :: _ when Procname.is_c callee_procname || Procname.is_objc_class_method callee_procname
      ->
        let arg_value, arg_history = arg.ProcnameDispatcher.Call.FuncArg.arg_payload in
        let ret_value = (arg_value, Hist.add_call path location desc arg_history) in
        PulseOperations.write_id ret_id ret_value astate |> ok_continue
    | _ ->
        ok_continue astate


  let return_this ~desc args : model_no_non_disj =
   fun {path; callee_procname; location; ret= ret_id, _} astate ->
    match args with
    | arg :: _
      when Procname.is_objc_instance_method callee_procname || Procname.is_java callee_procname ->
        let arg_value, arg_history = arg.ProcnameDispatcher.Call.FuncArg.arg_payload in
        let ret_value = (arg_value, Hist.add_call path location desc arg_history) in
        PulseOperations.write_id ret_id ret_value astate |> ok_continue
    | _ ->
        ok_continue astate


  let nondet ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    PulseOperations.havoc_id ret_id (Hist.single_call path location desc) astate |> ok_continue


  let skip : model_no_non_disj = fun _ astate -> ok_continue astate

  let id_first_arg ~desc (arg_value, arg_history) : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_value = (arg_value, Hist.add_call path location desc arg_history) in
    PulseOperations.write_id ret_id ret_value astate |> ok_continue


  let call_destructor ({ProcnameDispatcher.Call.FuncArg.arg_payload= _; exp; typ} as deleted_arg) :
      model =
   fun ({analysis_data; dispatch_call_eval_args; path; location; ret} : model_data) astate non_disj ->
    (* TODO: lookup dynamic type; currently not set in C++, should update model of [new] *)
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct class_name}, _) -> (
      match Tenv.find_cpp_destructor analysis_data.tenv class_name with
      | None ->
          Logging.d_printfln "No destructor found for class %a@\n" Typ.Name.pp class_name ;
          (ok_continue astate, non_disj)
      | Some destructor ->
          let callflags : CallFlags.t = CallFlags.default in
          dispatch_call_eval_args analysis_data path ret exp [deleted_arg] location callflags astate
            non_disj (Some destructor) )
    | _ ->
        Logging.d_printfln "Object being deleted is not a pointer to a class, got '%a' instead@\n"
          (Typ.pp_desc (Pp.html Black))
          typ.Typ.desc ;
        (ok_continue astate, non_disj)


  let match_args_of_single_proc comparator actuals (proc : Procname.t) : bool =
    (* Check if the formal arguments of the function 'proc' bind the actual arguments *)
    let formals = IRAttributes.load_formal_types proc in
    List.equal comparator formals actuals


  let match_args_of_procedures comparators actuals procedures =
    (* Check if there is any procedure in procedures where the formal arguments bind the actual arguments.
       It tries multiple-levels of comparators (e.g., strict, loose),
            and return the function found firstly that matches with a stricter comparator.*)
    List.find_map comparators ~f:(fun c ->
        List.find procedures ~f:(match_args_of_single_proc c actuals) )


  let call_constructor class_name actuals args exp
      {analysis_data; dispatch_call_eval_args; path; location; ret} astate non_disj =
    let candidates = Tenv.find_cpp_constructor analysis_data.tenv class_name in
    match match_args_of_procedures Typ.overloading_resolution actuals candidates with
    | Some constructor ->
        L.d_printfln_escaped "Constructor found: %a" Procname.pp_unique_id constructor ;
        dispatch_call_eval_args analysis_data path ret exp args location CallFlags.default astate
          non_disj (Some constructor)
    | None ->
        (* A constructor can be not found if it is not in captured data, e.g. standard library. *)
        L.d_printfln "Constructor not found" ;
        (astate |> ok_continue, non_disj)


  let free_or_delete operation invalidation
      ({ProcnameDispatcher.Call.FuncArg.arg_payload= deleted_access_path} as deleted_arg) : model =
   fun ({path; location} as model_data) astate non_disj ->
    let ((deleted_addr, deleted_hist) as deleted_access) =
      ValueOrigin.addr_hist deleted_access_path
    in
    (* NOTE: freeing 0 is a no-op so we introduce a case split *)
    let astates_alloc, non_disj =
      let addrs_to_invalidate =
        AbductiveDomain.reachable_addresses_from
          ~edge_filter:(function FieldAccess _ | ArrayAccess _ -> true | Dereference -> false)
          (Seq.return deleted_addr) astate `Post
      in
      let ( let<**> ) x f = bind_sat_result non_disj x f in
      let<**> astate = PulseArithmetic.prune_positive deleted_addr astate in
      let<**> astate =
        Sat (PulseOperations.check_addr_access path NoAccess location deleted_access astate)
      in
      let astates, non_disj =
        match operation with
        | `Free ->
            (ok_continue astate, non_disj)
        | `Delete ->
            call_destructor deleted_arg model_data astate non_disj
      in
      ( List.concat_map astates ~f:(fun exec_state_result ->
            let<*> exec_state = exec_state_result in
            match exec_state with
            | ContinueProgram astate ->
                AbstractValue.Set.fold
                  (fun addr astate ->
                    PulseOperations.invalidate path UntraceableAccess location invalidation
                      (addr, deleted_hist) astate )
                  addrs_to_invalidate astate
                |> ok_continue
            | ExceptionRaised _
            | ExitProgram _
            | AbortProgram _
            | LatentAbortProgram _
            | LatentInvalidAccess _
            | LatentSpecializedTypeIssue _ ->
                [Ok exec_state] )
      , non_disj )
    in
    let astate_zero =
      PulseArithmetic.prune_eq_zero deleted_addr astate
      >>|| ExecutionDomain.continue |> SatUnsat.to_list
    in
    (astate_zero @ astates_alloc, non_disj)


  let get_malloced_object_type (exp : Exp.t) =
    match exp with
    | BinOp (Mult _, Sizeof {typ}, _) | BinOp (Mult _, _, Sizeof {typ}) | Sizeof {typ} ->
        Some typ
    | _ ->
        None


  let set_uninitialized tenv path size_exp_opt location ret_value astate =
    (let open IOption.Let_syntax in
     let+ obj_typ = size_exp_opt >>= get_malloced_object_type in
     AddressAttributes.set_uninitialized tenv path (`Malloc ret_value) obj_typ location astate )
    |> Option.value ~default:astate


  let alloc_not_null_common ~initialize ?desc ~allocator size_exp_opt
      {analysis_data= {tenv}; location; path; callee_procname; ret= ret_id, ret_typ} astate =
    let desc = Option.value desc ~default:(Procname.to_string callee_procname) in
    let ret_addr, ret_alloc_hist =
      (AbstractValue.mk_fresh (), Hist.single_alloc path location desc)
    in
    let astate, (ret_addr, ret_alloc_hist) =
      let typ = if Typ.is_pointer ret_typ then Typ.strip_ptr ret_typ else ret_typ in
      PulseTaintOperations.taint_allocation tenv path location ~typ_desc:typ.desc ~alloc_desc:desc
        ~allocator (ret_addr, ret_alloc_hist) astate
    in
    let astate =
      match size_exp_opt with
      | Some (Exp.Sizeof {typ}) ->
          if Typ.is_objc_class typ then
            PulseArithmetic.and_dynamic_type_is_unsafe ret_addr typ ~source_file:location.file
              location astate
          else PulseArithmetic.and_dynamic_type_is_unsafe ret_addr typ location astate
      | _ ->
          (* The type expr is sometimes a Var expr in Java but this is not expected.
              This seems to be introduced by inline mechanism of Java synthetic methods during preanalysis *)
          astate
    in
    let astate = PulseOperations.write_id ret_id (ret_addr, ret_alloc_hist) astate in
    let astate =
      match allocator with
      | None ->
          astate
      | Some allocator ->
          PulseOperations.allocate allocator location ret_addr astate
    in
    let astate =
      if initialize then astate
      else set_uninitialized tenv path size_exp_opt location ret_addr astate
    in
    PulseArithmetic.and_positive ret_addr astate


  let alloc_not_null ?desc allocator size =
    alloc_not_null_common ?desc ~allocator:(Some allocator) size


  (** record in its history and dynamic type that the value was allocated but consider that it
      cannot generate leaks (eg it is handled by the language's garbage collector, or we don't want
      to report leaks for some reason) *)
  let alloc_no_leak_not_null ?desc size = alloc_not_null_common ?desc ~allocator:None size

  (* NOTE: starts from the [exp] representing the argument as there's some logic in
      {PulseOperations.prune} that works better if we know the SIL expression. This just means we're
      discarding some abstract values that were potentially created to evaluate [exp] when the model
      was called. *)
  let assert_ {ProcnameDispatcher.Call.FuncArg.exp= condition} : model_no_non_disj =
   fun {path; location} astate ->
    let<++> astate, _ = PulseOperations.prune path location ~condition astate in
    astate


  let matchers : matcher list =
    let open ProcnameDispatcher.Call in
    let match_regexp_opt r_opt (_tenv, proc_name) _ =
      Option.exists r_opt ~f:(fun r ->
          let s = Procname.to_string proc_name in
          Str.string_match r s 0 )
    in
    let match_taint_source flag (tenv, proc_name) _ =
      if flag then PulseTaintOperations.procedure_matches_source tenv proc_name else false
    in
    [ -"random" <>$$--> nondet ~desc:"random"
    ; -"assert" <>$ capt_arg $--> assert_
    ; +BuiltinDecl.(match_builtin objc_cpp_throw) <>--> early_exit
    ; +BuiltinDecl.(match_builtin __cast) <>$ capt_arg_payload $+...$--> id_first_arg ~desc:"cast"
    ; +BuiltinDecl.(match_builtin abort) <>--> early_exit
    ; +BuiltinDecl.(match_builtin exit) <>--> early_exit
    ; +BuiltinDecl.(match_builtin __infer_initializer_list)
      <>$ capt_arg_payload
      $+...$--> id_first_arg ~desc:"infer_init_list"
    ; +BuiltinDecl.(match_builtin __get_array_length) <>--> return_unknown_size ~desc:""
    ; +match_regexp_opt Config.pulse_model_return_nonnull
      &::.*--> return_positive ~desc:"modelled as returning not null due to configuration option"
    ; +match_regexp_opt Config.pulse_model_return_this
      &::.*++> return_this
                 ~desc:"modelled as returning `this` or `self` due to configuration option"
    ; +match_regexp_opt Config.pulse_model_return_first_arg
      &::.*++> id_first_arg_from_list
                 ~desc:"modelled as returning the first argument due to configuration option"
    ; +match_regexp_opt Config.pulse_model_skip_pattern
      &::.*++> unknown_call "modelled as skip due to configuration option"
    ; +match_taint_source Config.pulse_taint_skip_sources
      &::.*++> unknown_call "modelled as skip due to configuration option" ]
    |> List.map ~f:(fun matcher ->
           matcher
           |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
           |> with_non_disj )
end
