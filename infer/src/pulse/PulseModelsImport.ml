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

type arg_payload = AbstractValue.t * ValueHistory.t

type model_data =
  { analysis_data: PulseSummary.t InterproceduralAnalysis.t
  ; path: PathContext.t
  ; callee_procname: Procname.t
  ; location: Location.t
  ; ret: Ident.t * Typ.t }

type model = model_data -> AbductiveDomain.t -> ExecutionDomain.t AccessResult.t list

type matcher = (Tenv.t * Procname.t, model, arg_payload) ProcnameDispatcher.Call.matcher

module Hist = struct
  let mk_desc ?more desc =
    Option.value_map ~default:desc more ~f:(fun extra_info ->
        Printf.sprintf "%s %s" desc extra_info )


  let alloc_event {PathContext.timestamp} location ?more model_desc =
    let desc = mk_desc ?more model_desc in
    ValueHistory.Allocation {f= Model desc; location; timestamp}


  let call_event {PathContext.timestamp} location ?more model_desc =
    let desc = mk_desc ?more model_desc in
    ValueHistory.Call {f= Model desc; location; in_call= Epoch; timestamp}


  let add_event path event hist = PathContext.with_context path (Sequence (event, hist))

  let single_event path event = add_event path event Epoch

  let add_call path location model_desc ?more hist =
    add_event path (call_event path location ?more model_desc) hist


  let single_call path location ?more model_desc = add_call path location model_desc ?more Epoch

  let single_alloc path location ?more model_desc =
    alloc_event path location ?more model_desc |> single_event path


  let binop path bop hist1 hist2 = PathContext.with_context path (BinaryOp (bop, hist1, hist2))

  let hist path hist = PathContext.with_context path hist
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


  let shallow_copy_model model_desc dest_pointer_hist src_pointer_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location model_desc in
    shallow_copy path location event ret_id dest_pointer_hist src_pointer_hist astate


  let early_exit : model =
   fun {analysis_data= {tenv; proc_desc}; location} astate ->
    let open SatUnsat.Import in
    match
      ( AbductiveDomain.summary_of_post tenv proc_desc location astate
        >>| AccessResult.ignore_leaks >>| AccessResult.of_abductive_result
        :> (AbductiveDomain.summary, AbductiveDomain.t AccessResult.error) PulseResult.t SatUnsat.t
        )
    with
    | Unsat ->
        []
    | Sat (Ok astate) ->
        [Ok (ExitProgram astate)]
    | Sat (Recoverable (astate, errors)) ->
        [Recoverable (ExitProgram astate, errors)]
    | Sat (FatalError _ as err) ->
        [err]


  let return_int ~desc : Int64.t -> model =
   fun i64 {path; location; ret= ret_id, _} astate ->
    let i = IntLit.of_int64 i64 in
    let ret_addr = AbstractValue.Constants.get_int i in
    let<+> astate = PulseArithmetic.and_eq_int ret_addr i astate in
    PulseOperations.write_id ret_id (ret_addr, Hist.single_call path location desc) astate


  let return_positive ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, Hist.single_call path location desc) in
    let<+> astate = PulseArithmetic.and_positive ret_addr astate in
    PulseOperations.write_id ret_id ret_value astate


  let return_unknown_size ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let<+> astate = PulseArithmetic.and_nonnegative ret_addr astate in
    PulseOperations.write_id ret_id (ret_addr, Hist.single_call path location desc) astate


  (** Pretend the function call is a call to an "unknown" function, i.e. a function for which we
      don't have the implementation. This triggers a bunch of heuristics, e.g. to havoc arguments we
      suspect are passed by reference. *)
  let unknown_call skip_reason args : model =
   fun {path; callee_procname; location; ret} astate ->
    let actuals =
      List.map args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= actual; typ} ->
          (actual, typ) )
    in
    let formals_opt = IRAttributes.load callee_procname |> Option.map ~f:Pvar.get_pvar_formals in
    let<+> astate =
      PulseCallOperations.unknown_call path location (Model skip_reason) ~ret ~actuals ~formals_opt
        astate
    in
    astate


  let nondet ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    PulseOperations.havoc_id ret_id (Hist.single_call path location desc) astate |> ok_continue


  let id_first_arg ~desc (arg_value, arg_history) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_value = (arg_value, Hist.add_call path location desc arg_history) in
    PulseOperations.write_id ret_id ret_value astate |> ok_continue


  let call_destructor {ProcnameDispatcher.Call.FuncArg.arg_payload= deleted_access; typ} : model =
   fun {analysis_data= {tenv; proc_desc; analyze_dependency}; path; location; ret} astate ->
    (* TODO: lookup dynamic type; currently not set in C++, should update model of [new] *)
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct class_name}, _) -> (
      match Tenv.find_cpp_destructor tenv class_name with
      | None ->
          L.d_printfln "No destructor found for class %a@\n" Typ.Name.pp class_name ;
          ok_continue astate
      | Some destructor ->
          L.d_printfln "Found destructor for class %a@\n" Typ.Name.pp class_name ;
          PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc
            ~callee_data:(analyze_dependency destructor) location destructor ~ret
            ~actuals:[(deleted_access, typ)] ~formals_opt:None astate )
    | _ ->
        L.d_printfln "Object being deleted is not a pointer to a class, got '%a' instead@\n"
          (Typ.pp_desc (Pp.html Black))
          typ.Typ.desc ;
        ok_continue astate


  let free_or_delete operation invalidation
      ({ProcnameDispatcher.Call.FuncArg.arg_payload= deleted_access} as deleted_arg) : model =
   fun ({path; location} as model_data) astate ->
    (* NOTE: freeing 0 is a no-op so we introduce a case split *)
    let astates_alloc =
      let<*> astate = PulseArithmetic.and_positive (fst deleted_access) astate in
      let astates =
        match operation with
        | `Free ->
            ok_continue astate
        | `Delete ->
            call_destructor deleted_arg model_data astate
      in
      List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              if Config.pulse_isl then
                PulseOperations.invalidate_biad_isl path location invalidation deleted_access astate
                |> List.map ~f:(fun result ->
                       let+ astate = result in
                       ContinueProgram astate )
              else
                let<+> astate =
                  PulseOperations.invalidate path UntraceableAccess location invalidation
                    deleted_access astate
                in
                astate
          | ExceptionRaised _
          | ExitProgram _
          | AbortProgram _
          | LatentAbortProgram _
          | LatentInvalidAccess _
          | ISLLatentMemoryError _ ->
              [Ok exec_state] )
    in
    let astate_zero = PulseArithmetic.prune_eq_zero (fst deleted_access) astate |> map_continue in
    astate_zero :: astates_alloc


  let set_uninitialized tenv path size_exp_opt location ret_value astate =
    Option.value_map size_exp_opt ~default:astate ~f:(fun size_exp ->
        BufferOverrunModels.get_malloc_info_opt size_exp
        |> Option.value_map ~default:astate ~f:(fun (obj_typ, _, _, _) ->
               AbductiveDomain.set_uninitialized tenv path (`Malloc ret_value) obj_typ location
                 astate ) )


  let alloc_not_null_common ~initialize ?desc ~allocator size_exp_opt
      {analysis_data= {tenv}; location; path; callee_procname; ret= ret_id, _} astate =
    let ret_addr = AbstractValue.mk_fresh () in
    let desc = Option.value desc ~default:(Procname.to_string callee_procname) in
    let ret_alloc_hist = Hist.single_alloc path location desc in
    let astate =
      match size_exp_opt with
      | Some (Exp.Sizeof {typ}) ->
          PulseOperations.add_dynamic_type typ ret_addr astate
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
  let assert_ {ProcnameDispatcher.Call.FuncArg.exp= condition} : model =
   fun {path; location} astate ->
    let<+> astate, _ = PulseOperations.prune path location ~condition astate in
    astate


  let matchers : matcher list =
    let open ProcnameDispatcher.Call in
    let match_regexp_opt r_opt (_tenv, proc_name) _ =
      Option.exists r_opt ~f:(fun r ->
          let s = Procname.to_string proc_name in
          Str.string_match r s 0 )
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
    ; +match_regexp_opt Config.pulse_model_return_first_arg
      &::+ (fun _ _ -> true)
      <>$ capt_arg_payload
      $+...$--> id_first_arg
                  ~desc:"modelled as returning the first argument due to configuration option"
    ; +match_regexp_opt Config.pulse_model_skip_pattern
      &::.*++> unknown_call "modelled as skip due to configuration option" ]
end
