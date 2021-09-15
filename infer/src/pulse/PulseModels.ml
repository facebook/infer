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

let continue astate = ContinueProgram astate

let map_continue astate_result =
  let open IResult.Let_syntax in
  let+ astate = astate_result in
  continue astate


let ok_continue post = [Ok (ContinueProgram post)]

module Misc = struct
  let shallow_copy_value path location event ret_id dest_pointer_hist src_value_hist astate =
    let<*> astate, obj_copy = PulseOperations.shallow_copy path location src_value_hist astate in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:dest_pointer_hist
        ~obj:(fst obj_copy, event :: snd obj_copy)
        astate
    in
    PulseOperations.havoc_id ret_id [event] astate


  let shallow_copy path location event ret_id dest_pointer_hist src_pointer_hist astate =
    let<*> astate, obj =
      PulseOperations.eval_access path Read location src_pointer_hist Dereference astate
    in
    shallow_copy_value path location event ret_id dest_pointer_hist obj astate


  let shallow_copy_model model_desc dest_pointer_hist src_pointer_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    shallow_copy path location event ret_id dest_pointer_hist src_pointer_hist astate


  let early_exit : model =
   fun {analysis_data= {tenv; proc_desc}; location} astate ->
    let open SatUnsat.Import in
    match
      ( AbductiveDomain.summary_of_post tenv proc_desc location astate
        >>| AccessResult.ignore_memory_leaks >>| AccessResult.of_abductive_result
        :> (AbductiveDomain.summary, AbductiveDomain.t AccessResult.error) result SatUnsat.t )
    with
    | Unsat ->
        []
    | Sat (Ok astate) ->
        [Ok (ExitProgram astate)]
    | Sat (Error _ as error) ->
        [error]


  let return_int : Int64.t -> model =
   fun i64 {ret= ret_id, _} astate ->
    let i = IntLit.of_int64 i64 in
    let ret_addr = AbstractValue.Constants.get_int i in
    let<+> astate = PulseArithmetic.and_eq_int ret_addr i astate in
    PulseOperations.write_id ret_id (ret_addr, []) astate


  let return_positive ~desc : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, [event]) in
    let<+> astate = PulseArithmetic.and_positive ret_addr astate in
    PulseOperations.write_id ret_id ret_value astate


  let return_unknown_size : model =
   fun {ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let<+> astate = PulseArithmetic.and_nonnegative ret_addr astate in
    PulseOperations.write_id ret_id (ret_addr, []) astate


  (** Pretend the function call is a call to an "unknown" function, i.e. a function for which we
      don't have the implementation. This triggers a bunch of heuristics, e.g. to havoc arguments we
      suspect are passed by reference. *)
  let unknown_call skip_reason args : model =
   fun {analysis_data= {tenv}; path; callee_procname; location; ret} astate ->
    let actuals =
      List.map args ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= actual; typ} ->
          (actual, typ) )
    in
    let formals_opt = IRAttributes.load callee_procname |> Option.map ~f:Pvar.get_pvar_formals in
    let<+> astate =
      PulseCallOperations.unknown_call tenv path location (Model skip_reason) ~ret ~actuals
        ~formals_opt astate
    in
    astate


  (** don't actually do nothing, apply the heuristics for unknown calls (this may or may not be a
      good idea) *)
  let skip = unknown_call

  let nondet ~fn_name : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model fn_name; location; in_call= []} in
    PulseOperations.havoc_id ret_id [event] astate |> ok_continue


  let id_first_arg ~desc (arg_value, arg_history) : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ret_value = (arg_value, event :: arg_history) in
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
          | ExitProgram _
          | AbortProgram _
          | LatentAbortProgram _
          | LatentInvalidAccess _
          | ISLLatentMemoryError _ ->
              [Ok exec_state] )
    in
    let astate_zero = PulseArithmetic.prune_eq_zero (fst deleted_access) astate |> map_continue in
    astate_zero :: astates_alloc


  let set_uninitialized tenv size_exp_opt location ret_value astate =
    Option.value_map size_exp_opt ~default:astate ~f:(fun size_exp ->
        BufferOverrunModels.get_malloc_info_opt size_exp
        |> Option.value_map ~default:astate ~f:(fun (obj_typ, _, _, _) ->
               AbductiveDomain.set_uninitialized tenv (`Malloc ret_value) obj_typ location astate ) )


  let alloc_not_null_common ~initialize ?desc ~allocator size_exp_opt
      {analysis_data= {tenv}; location; callee_procname; ret= ret_id, _} astate =
    let ret_addr = AbstractValue.mk_fresh () in
    let desc = Option.value desc ~default:(Procname.to_string callee_procname) in
    let ret_alloc_hist = [ValueHistory.Allocation {f= Model desc; location}] in
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
          PulseOperations.allocate allocator location (ret_addr, []) astate
    in
    let astate =
      if initialize then astate else set_uninitialized tenv size_exp_opt location ret_addr astate
    in
    PulseArithmetic.and_positive ret_addr astate


  let alloc_not_null ?desc allocator size =
    alloc_not_null_common ?desc ~allocator:(Some allocator) size


  (** record in its history and dynamic type that the value was allocated but consider that it
      cannot generate leaks (eg it is handled by the language's garbage collector, or we don't want
      to report leaks for some reason) *)
  let alloc_no_leak_not_null ?desc size = alloc_not_null_common ?desc ~allocator:None size
end

module C = struct
  let free deleted_access : model = Misc.free_or_delete `Free CFree deleted_access

  let alloc_common allocator ~size_exp_opt : model =
   fun ({path; callee_procname; location; ret= ret_id, _} as model_data) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let astate_alloc =
      Misc.alloc_not_null allocator ~initialize:false size_exp_opt model_data astate >>| continue
    in
    let result_null =
      let ret_null_hist =
        [ValueHistory.Call {f= Model (Procname.to_string callee_procname); location; in_call= []}]
      in
      let ret_null_value = (ret_addr, ret_null_hist) in
      let+ astate_null =
        PulseOperations.write_id ret_id ret_null_value astate
        |> PulseArithmetic.and_eq_int ret_addr IntLit.zero
        >>= PulseOperations.invalidate path
              (StackAddress (Var.of_id ret_id, ret_null_hist))
              location (ConstantDereference IntLit.zero) ret_null_value
      in
      ContinueProgram astate_null
    in
    [astate_alloc; result_null]


  let alloc_not_null_common allocator ~size_exp_opt : model =
   fun model_data astate ->
    let<+> astate =
      Misc.alloc_not_null ~initialize:false allocator size_exp_opt model_data astate
    in
    astate


  let malloc size_exp = alloc_common CMalloc ~size_exp_opt:(Some size_exp)

  let malloc_not_null size_exp = alloc_not_null_common CMalloc ~size_exp_opt:(Some size_exp)

  let custom_malloc size_exp model_data astate =
    alloc_common (CustomMalloc model_data.callee_procname) ~size_exp_opt:(Some size_exp) model_data
      astate


  let custom_alloc model_data astate =
    alloc_common (CustomMalloc model_data.callee_procname) ~size_exp_opt:None model_data astate


  let custom_alloc_not_null model_data astate =
    alloc_not_null_common (CustomMalloc model_data.callee_procname) ~size_exp_opt:None model_data
      astate


  let realloc_common allocator pointer size : model =
   fun data astate ->
    free pointer data astate
    |> List.concat_map ~f:(fun result ->
           let<*> exec_state = result in
           match (exec_state : ExecutionDomain.t) with
           | ContinueProgram astate ->
               alloc_common allocator ~size_exp_opt:(Some size) data astate
           | ExitProgram _
           | AbortProgram _
           | LatentAbortProgram _
           | LatentInvalidAccess _
           | ISLLatentMemoryError _ ->
               [Ok exec_state] )


  let realloc = realloc_common CRealloc

  let custom_realloc pointer size data astate =
    realloc_common (CustomRealloc data.callee_procname) pointer size data astate
end

module ObjCCoreFoundation = struct
  let cf_bridging_release access : model =
   fun {ret= ret_id, _} astate ->
    let astate = PulseOperations.write_id ret_id access astate in
    PulseOperations.remove_allocation_attr (fst access) astate |> ok_continue
end

module ObjC = struct
  let dispatch_sync args : model =
   fun {path; analysis_data= {analyze_dependency; tenv; proc_desc}; location; ret} astate ->
    match List.last args with
    | None ->
        ok_continue astate
    | Some {ProcnameDispatcher.Call.FuncArg.arg_payload= lambda_ptr_hist} -> (
        let<*> astate, (lambda, _) =
          PulseOperations.eval_access path Read location lambda_ptr_hist Dereference astate
        in
        match AddressAttributes.get_closure_proc_name lambda astate with
        | None ->
            ok_continue astate
        | Some callee_proc_name ->
            PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc
              ~callee_data:(analyze_dependency callee_proc_name)
              location callee_proc_name ~ret ~actuals:[] ~formals_opt:None astate )


  let insertion_into_collection_key_and_value (value, value_hist) (key, key_hist) ~desc : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, _ =
      PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionValue Read
        location
        (value, event :: value_hist)
        Dereference astate
    in
    let<+> astate, _ =
      PulseOperations.eval_access path ~must_be_valid_reason:InsertionIntoCollectionKey Read
        location
        (key, event :: key_hist)
        Dereference astate
    in
    astate


  let insertion_into_collection_key_or_value (value, value_hist) ~value_kind ~desc : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let must_be_valid_reason =
      match value_kind with
      | `Key ->
          Invalidation.InsertionIntoCollectionKey
      | `Value ->
          Invalidation.InsertionIntoCollectionValue
    in
    let<+> astate, _ =
      PulseOperations.eval_access path ~must_be_valid_reason Read location
        (value, event :: value_hist)
        Dereference astate
    in
    astate


  let read_from_collection (key, key_hist) ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let astate_nil =
      let ret_val = AbstractValue.mk_fresh () in
      let<*> astate = PulseArithmetic.prune_eq_zero key astate in
      let<+> astate = PulseArithmetic.and_eq_int ret_val IntLit.zero astate in
      PulseOperations.write_id ret_id (ret_val, event :: key_hist) astate
    in
    let astate_not_nil =
      let<*> astate = PulseArithmetic.prune_positive key astate in
      let<+> astate, (ret_val, hist) =
        PulseOperations.eval_access path Read location (key, key_hist) Dereference astate
      in
      PulseOperations.write_id ret_id (ret_val, event :: hist) astate
    in
    List.rev_append astate_nil astate_not_nil


  (* NOTE: assume that this is always called with [freeWhenDone] being [YES] *)
  let init_with_bytes_free_when_done bytes : model =
   fun {ret= ret_id, _; callee_procname; location} astate ->
    let event = ValueHistory.Call {f= Call callee_procname; location; in_call= []} in
    PulseOperations.havoc_id ret_id [event] astate
    |> PulseOperations.remove_allocation_attr (fst bytes)
    |> ok_continue


  let alloc_no_fail size : model =
   fun model_data astate ->
    (* NOTE: technically this doesn't initialize the result but we haven't modelled initialization so
       assume the object is initialized after [init] for now *)
    let<+> astate =
      Misc.alloc_no_leak_not_null ~initialize:true ~desc:"alloc" (Some size) model_data astate
    in
    astate
end

module Optional = struct
  let internal_value = Fieldname.make PulseOperations.pulse_model_type "backing_value"

  let internal_value_access = HilExp.Access.FieldAccess internal_value

  let to_internal_value path mode location optional astate =
    PulseOperations.eval_access path mode location optional internal_value_access astate


  let to_internal_value_deref path mode location optional astate =
    let* astate, pointer = to_internal_value path Read location optional astate in
    PulseOperations.eval_access path mode location pointer Dereference astate


  let write_value path location this ~value ~desc astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let* astate, value_field = to_internal_value path Read location this astate in
    let value_hist = (fst value, event :: snd value) in
    let+ astate =
      PulseOperations.write_deref path location ~ref:value_field ~obj:value_hist astate
    in
    (astate, value_field, value_hist)


  let assign_value_fresh path location this ~desc astate =
    write_value path location this ~value:(AbstractValue.mk_fresh (), []) ~desc astate


  let assign_none this ~desc : model =
   fun {path; location} astate ->
    let<*> astate, pointer, value = assign_value_fresh path location this ~desc astate in
    let<*> astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
    let<+> astate =
      PulseOperations.invalidate path
        (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
        location OptionalEmpty value astate
    in
    astate


  let assign_value this _value ~desc : model =
   fun {path; location} astate ->
    (* TODO: call the copy constructor of a value *)
    let<*> astate, _, value = assign_value_fresh path location this ~desc astate in
    let<+> astate = PulseArithmetic.and_positive (fst value) astate in
    astate


  let assign_optional_value this init ~desc : model =
   fun {path; location} astate ->
    let<*> astate, value = to_internal_value_deref path Read location init astate in
    let<+> astate, _, _ = write_value path location this ~value ~desc astate in
    astate


  let emplace optional ~desc : model =
   fun {path; location} astate ->
    let<+> astate, _, _ = assign_value_fresh path location optional ~desc astate in
    astate


  let value optional ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, ((value_addr, value_hist) as value) =
      to_internal_value_deref path Write location optional astate
    in
    (* Check dereference to show an error at the callsite of `value()` *)
    let<*> astate, _ = PulseOperations.eval_access path Write location value Dereference astate in
    PulseOperations.write_id ret_id (value_addr, event :: value_hist) astate |> ok_continue


  let has_value optional ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, [ValueHistory.Call {f= Model desc; location; in_call= []}]) in
    let<*> astate, (value_addr, _) = to_internal_value_deref path Read location optional astate in
    let astate = PulseOperations.write_id ret_id ret_value astate in
    let result_non_empty =
      PulseArithmetic.prune_positive value_addr astate
      >>= PulseArithmetic.prune_positive ret_addr
      |> map_continue
    in
    let result_empty =
      PulseArithmetic.prune_eq_zero value_addr astate
      >>= PulseArithmetic.prune_eq_zero ret_addr
      |> map_continue
    in
    [result_non_empty; result_empty]


  let get_pointer optional ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, value_addr = to_internal_value_deref path Read location optional astate in
    let value_update_hist = (fst value_addr, event :: snd value_addr) in
    let astate_value_addr =
      PulseOperations.write_id ret_id value_update_hist astate
      |> PulseArithmetic.prune_positive (fst value_addr)
      |> map_continue
    in
    let nullptr = (AbstractValue.mk_fresh (), [event]) in
    let astate_null =
      PulseOperations.write_id ret_id nullptr astate
      |> PulseArithmetic.prune_eq_zero (fst value_addr)
      >>= PulseArithmetic.and_eq_int (fst nullptr) IntLit.zero
      >>= PulseOperations.invalidate path
            (StackAddress (Var.of_id ret_id, snd nullptr))
            location (ConstantDereference IntLit.zero) nullptr
      |> map_continue
    in
    [astate_value_addr; astate_null]


  let value_or optional default ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, value_addr = to_internal_value_deref path Read location optional astate in
    let astate_non_empty =
      let+ astate_non_empty, value =
        PulseArithmetic.prune_positive (fst value_addr) astate
        >>= PulseOperations.eval_access path Read location value_addr Dereference
      in
      let value_update_hist = (fst value, event :: snd value) in
      PulseOperations.write_id ret_id value_update_hist astate_non_empty |> continue
    in
    let astate_default =
      let* astate, (default_val, default_hist) =
        PulseOperations.eval_access path Read location default Dereference astate
      in
      let default_value_hist = (default_val, event :: default_hist) in
      PulseArithmetic.prune_eq_zero (fst value_addr) astate
      >>| PulseOperations.write_id ret_id default_value_hist
      |> map_continue
    in
    [astate_non_empty; astate_default]
end

module Cplusplus = struct
  let delete deleted_arg : model =
   fun model_data astate -> Misc.free_or_delete `Delete CppDelete deleted_arg model_data astate


  (* NOTE: [new\[\]] is not yet modelled as allocating an array of objects hence why this model
     deletes only the root address *)
  let delete_array deleted_arg : model =
   fun model_data astate -> Misc.free_or_delete `Delete CppDeleteArray deleted_arg model_data astate


  let new_ type_name : model =
   fun model_data astate ->
    let<+> astate =
      (* Java and C++ [new] share the same builtin (note that ObjC gets its own [objc_alloc_no_fail]
         builtin for [\[Class new\]]) *)
      if Procname.is_java @@ Procdesc.get_proc_name model_data.analysis_data.proc_desc then
        Misc.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new" model_data astate
      else
        (* C++ *)
        Misc.alloc_not_null ~initialize:true ~desc:"new" CppNew (Some type_name) model_data astate
    in
    astate


  (* TODO: actually allocate an array  *)
  let new_array type_name : model =
   fun model_data astate ->
    let<+> astate =
      (* Java and C++ [new\[\]] share the same builtin *)
      if Procname.is_java @@ Procdesc.get_proc_name model_data.analysis_data.proc_desc then
        Misc.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new[]" model_data
          astate
      else
        (* C++ *)
        Misc.alloc_not_null ~initialize:true ~desc:"new[]" CppNewArray (Some type_name) model_data
          astate
    in
    astate


  let placement_new actuals : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "<placement new>()"; location; in_call= []} in
    ( match List.rev actuals with
    | ProcnameDispatcher.Call.FuncArg.{arg_payload= address, hist} :: _ ->
        PulseOperations.write_id ret_id (address, event :: hist) astate
    | _ ->
        PulseOperations.havoc_id ret_id [event] astate )
    |> ok_continue
end

module StdAtomicInteger = struct
  let internal_int =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "atomic"]))
      "__infer_model_backing_int"


  let load_backing_int path location this astate =
    let* astate, obj = PulseOperations.eval_access path Read location this Dereference astate in
    let* astate, int_addr =
      PulseOperations.eval_access path Read location obj (FieldAccess internal_int) astate
    in
    let+ astate, int_val =
      PulseOperations.eval_access path Read location int_addr Dereference astate
    in
    (astate, int_addr, int_val)


  let constructor this_address init_value : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::atomic()"; location; in_call= []} in
    let this = (AbstractValue.mk_fresh (), [event]) in
    let<*> astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    let<*> astate =
      PulseOperations.write_deref path location ~ref:int_field ~obj:init_value astate
    in
    let<+> astate = PulseOperations.write_deref path location ~ref:this_address ~obj:this astate in
    astate


  let arith_bop path prepost location event ret_id bop this operand astate =
    let* astate, int_addr, (old_int, hist) = load_backing_int path location this astate in
    let bop_addr = AbstractValue.mk_fresh () in
    let* astate, bop_addr =
      PulseArithmetic.eval_binop bop_addr bop (AbstractValueOperand old_int) operand astate
    in
    let+ astate =
      PulseOperations.write_deref path location ~ref:int_addr ~obj:(bop_addr, event :: hist) astate
    in
    let ret_int = match prepost with `Pre -> bop_addr | `Post -> old_int in
    PulseOperations.write_id ret_id (ret_int, event :: hist) astate


  let fetch_add this (increment, _) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::fetch_add()"; location; in_call= []} in
    let<+> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let fetch_sub this (increment, _) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::fetch_sub()"; location; in_call= []} in
    let<+> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let operator_plus_plus_pre this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::operator++()"; location; in_call= []} in
    let<+> astate =
      arith_bop path `Pre location event ret_id (PlusA None) this (LiteralOperand IntLit.one) astate
    in
    astate


  let operator_plus_plus_post this _int : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event =
      ValueHistory.Call {f= Model "std::atomic<T>::operator++(T)"; location; in_call= []}
    in
    let<+> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (LiteralOperand IntLit.one)
        astate
    in
    astate


  let operator_minus_minus_pre this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::operator--()"; location; in_call= []} in
    let<+> astate =
      arith_bop path `Pre location event ret_id (MinusA None) this (LiteralOperand IntLit.one)
        astate
    in
    astate


  let operator_minus_minus_post this _int : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event =
      ValueHistory.Call {f= Model "std::atomic<T>::operator--(T)"; location; in_call= []}
    in
    let<+> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (LiteralOperand IntLit.one)
        astate
    in
    astate


  let load_instr model_desc this _memory_ordering_opt : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    let<+> astate, _int_addr, (int, hist) = load_backing_int path location this astate in
    PulseOperations.write_id ret_id (int, event :: hist) astate


  let load = load_instr "std::atomic<T>::load()"

  let operator_t = load_instr "std::atomic<T>::operator_T()"

  let store_backing_int path location this_address new_value astate =
    let* astate, this =
      PulseOperations.eval_access path Read location this_address Dereference astate
    in
    let astate =
      AddressAttributes.add_one (fst this_address)
        (WrittenTo (Trace.Immediate {location; history= []}))
        astate
    in
    let* astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    PulseOperations.write_deref path location ~ref:int_field ~obj:new_value astate


  let store this_address (new_value, new_hist) _memory_ordering : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::store()"; location; in_call= []} in
    let<+> astate =
      store_backing_int path location this_address (new_value, event :: new_hist) astate
    in
    astate


  let exchange this_address (new_value, new_hist) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::exchange()"; location; in_call= []} in
    let<*> astate, _int_addr, (old_int, old_hist) =
      load_backing_int path location this_address astate
    in
    let<+> astate =
      store_backing_int path location this_address (new_value, event :: new_hist) astate
    in
    PulseOperations.write_id ret_id (old_int, event :: old_hist) astate
end

module JavaObject = struct
  (* naively modeled as shallow copy. *)
  let clone src_pointer_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "Object.clone"; location; in_call= []} in
    let<*> astate, obj =
      PulseOperations.eval_access path Read location src_pointer_hist Dereference astate
    in
    let<+> astate, obj_copy = PulseOperations.shallow_copy path location obj astate in
    PulseOperations.write_id ret_id (fst obj_copy, event :: snd obj_copy) astate
end

let string_length_access = HilExp.Access.FieldAccess PulseOperations.ModeledField.string_length

module StdBasicString = struct
  let internal_string =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "basic_string"]))
      "__infer_model_backing_string"


  let internal_string_access = HilExp.Access.FieldAccess internal_string

  let to_internal_string path location bstring astate =
    PulseOperations.eval_access path Read location bstring internal_string_access astate


  (* constructor from constant string *)
  let constructor this_hist init_hist : model =
   fun {path; location} astate ->
    let event =
      ValueHistory.Call {f= Model "std::basic_string::basic_string()"; location; in_call= []}
    in
    let<*> astate, (addr, hist) =
      PulseOperations.eval_access path Write location this_hist Dereference astate
    in
    let<+> astate =
      PulseOperations.write_field path location
        ~ref:(addr, event :: hist)
        internal_string ~obj:init_hist astate
    in
    astate


  let data this_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::data()"; location; in_call= []} in
    let<*> astate, string_addr_hist = to_internal_string path location this_hist astate in
    let<+> astate, (string, hist) =
      PulseOperations.eval_access path Read location string_addr_hist Dereference astate
    in
    PulseOperations.write_id ret_id (string, event :: hist) astate


  let destructor this_hist : model =
   fun {path; location} astate ->
    let model = CallEvent.Model "std::basic_string::~basic_string()" in
    let call_event = ValueHistory.Call {f= model; location; in_call= []} in
    let<*> astate, (string_addr, string_hist) = to_internal_string path location this_hist astate in
    let string_addr_hist = (string_addr, call_event :: string_hist) in
    let<*> astate =
      PulseOperations.invalidate_access path location CppDelete string_addr_hist Dereference astate
    in
    let<+> astate =
      PulseOperations.invalidate path
        (MemoryAccess
           { pointer= this_hist
           ; access= internal_string_access
           ; hist_obj_default= snd string_addr_hist } )
        location CppDelete string_addr_hist astate
    in
    astate


  let empty this_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::empty()"; location; in_call= []} in
    let<*> astate, internal_string = to_internal_string path location this_hist astate in
    let<*> astate, (len_addr, hist) =
      PulseOperations.eval_access path Read location internal_string string_length_access astate
    in
    let ((ret_addr, _) as ret_hist) = (AbstractValue.mk_fresh (), event :: hist) in
    let astate_empty =
      let* astate = PulseArithmetic.prune_eq_zero len_addr astate in
      let+ astate = PulseArithmetic.and_eq_int ret_addr IntLit.one astate in
      PulseOperations.write_id ret_id ret_hist astate |> continue
    in
    let astate_non_empty =
      let* astate = PulseArithmetic.prune_positive len_addr astate in
      let+ astate = PulseArithmetic.and_eq_int ret_addr IntLit.zero astate in
      PulseOperations.write_id ret_id ret_hist astate |> continue
    in
    [astate_empty; astate_non_empty]


  let length this_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::length()"; location; in_call= []} in
    let<*> astate, internal_string = to_internal_string path location this_hist astate in
    let<+> astate, (length, hist) =
      PulseOperations.eval_access path Read location internal_string string_length_access astate
    in
    PulseOperations.write_id ret_id (length, event :: hist) astate
end

module StdFunction = struct
  let operator_call ProcnameDispatcher.Call.FuncArg.{arg_payload= lambda_ptr_hist; typ} actuals :
      model =
   fun {path; analysis_data= {analyze_dependency; tenv; proc_desc}; location; ret} astate ->
    let havoc_ret (ret_id, _) astate =
      let event = ValueHistory.Call {f= Model "std::function::operator()"; location; in_call= []} in
      [PulseOperations.havoc_id ret_id [event] astate]
    in
    let<*> astate, (lambda, _) =
      PulseOperations.eval_access path Read location lambda_ptr_hist Dereference astate
    in
    let<*> astate = PulseOperations.Closures.check_captured_addresses path location lambda astate in
    match AddressAttributes.get_closure_proc_name lambda astate with
    | None ->
        (* we don't know what proc name this lambda resolves to *)
        havoc_ret ret astate |> List.map ~f:(fun astate -> Ok (ContinueProgram astate))
    | Some callee_proc_name ->
        let actuals =
          (lambda_ptr_hist, typ)
          :: List.map actuals ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
                 (arg_payload, typ) )
        in
        PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc
          ~callee_data:(analyze_dependency callee_proc_name)
          location callee_proc_name ~ret ~actuals ~formals_opt:None astate


  let assign dest ProcnameDispatcher.Call.FuncArg.{arg_payload= src; typ= src_typ} ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    if PulseArithmetic.is_known_zero astate (fst src) then
      let empty_target = AbstractValue.mk_fresh () in
      let<+> astate =
        PulseOperations.write_deref path location ~ref:dest ~obj:(empty_target, [event]) astate
      in
      PulseOperations.havoc_id ret_id [event] astate
    else
      match src_typ.Typ.desc with
      | Tptr (_, Pk_reference) ->
          Misc.shallow_copy path location event ret_id dest src astate
      | _ ->
          Misc.shallow_copy_value path location event ret_id dest src astate
end

module GenericArrayBackedCollection = struct
  let field = Fieldname.make PulseOperations.pulse_model_type "backing_array"

  let last_field = Fieldname.make PulseOperations.pulse_model_type "past_the_end"

  let is_empty = Fieldname.make PulseOperations.pulse_model_type "is_empty"

  let access = HilExp.Access.FieldAccess field

  let eval path mode location collection astate =
    PulseOperations.eval_deref_access path mode location collection access astate


  let eval_element path location internal_array index astate =
    PulseOperations.eval_access path Read location internal_array
      (ArrayAccess (StdTyp.void, index))
      astate


  let element path location collection index astate =
    let* astate, internal_array = eval path Read location collection astate in
    eval_element path location internal_array index astate


  let eval_pointer_to_last_element path location collection astate =
    let+ astate, pointer =
      PulseOperations.eval_deref_access path Write location collection (FieldAccess last_field)
        astate
    in
    let astate = AddressAttributes.mark_as_end_of_collection (fst pointer) astate in
    (astate, pointer)


  let eval_is_empty path location collection astate =
    PulseOperations.eval_deref_access path Write location collection (FieldAccess is_empty) astate
end

module GenericArrayBackedCollectionIterator = struct
  let internal_pointer = Fieldname.make PulseOperations.pulse_model_type "backing_pointer"

  let internal_pointer_access = HilExp.Access.FieldAccess internal_pointer

  let to_internal_pointer path mode location iterator astate =
    PulseOperations.eval_access path mode location iterator internal_pointer_access astate


  let to_internal_pointer_deref path mode location iterator astate =
    let* astate, pointer = to_internal_pointer path Read location iterator astate in
    let+ astate, index =
      PulseOperations.eval_access path mode location pointer Dereference astate
    in
    (astate, pointer, index)


  let to_elem_pointed_by_iterator path mode ?(step = None) location iterator astate =
    let* astate, pointer = to_internal_pointer path Read location iterator astate in
    let* astate, index =
      PulseOperations.eval_access path mode location pointer Dereference astate
    in
    (* Check if not end iterator *)
    let is_minus_minus = match step with Some `MinusMinus -> true | _ -> false in
    let* astate =
      if AddressAttributes.is_end_of_collection (fst pointer) astate && not is_minus_minus then
        let invalidation_trace = Trace.Immediate {location; history= []} in
        let access_trace = Trace.Immediate {location; history= snd pointer} in
        Error
          (ReportableError
             { diagnostic=
                 Diagnostic.AccessToInvalidAddress
                   { calling_context= []
                   ; invalidation= EndIterator
                   ; invalidation_trace
                   ; access_trace
                   ; must_be_valid_reason= None }
             ; astate } )
      else Ok astate
    in
    (* We do not want to create internal array if iterator pointer has an invalid value *)
    let* astate = PulseOperations.check_addr_access path Read location index astate in
    let+ astate, elem =
      GenericArrayBackedCollection.element path location iterator (fst index) astate
    in
    (astate, pointer, elem)


  let construct path location event ~init ~ref astate =
    let* astate, (arr_addr, arr_hist) =
      GenericArrayBackedCollection.eval path Read location init astate
    in
    let* astate =
      PulseOperations.write_deref_field path location ~ref GenericArrayBackedCollection.field
        ~obj:(arr_addr, event :: arr_hist)
        astate
    in
    let* astate, (p_addr, p_hist) = to_internal_pointer path Read location init astate in
    PulseOperations.write_field path location ~ref internal_pointer
      ~obj:(p_addr, event :: p_hist)
      astate


  let constructor ~desc this init : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<+> astate = construct path location event ~init ~ref:this astate in
    astate


  let operator_compare comparison ~desc iter_lhs iter_rhs : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, _, (index_lhs, _) =
      to_internal_pointer_deref path Read location iter_lhs astate
    in
    let<*> astate, _, (index_rhs, _) =
      to_internal_pointer_deref path Read location iter_rhs astate
    in
    let ret_val = AbstractValue.mk_fresh () in
    let astate = PulseOperations.write_id ret_id (ret_val, [event]) astate in
    let ret_val_equal, ret_val_notequal =
      match comparison with
      | `Equal ->
          (IntLit.one, IntLit.zero)
      | `NotEqual ->
          (IntLit.zero, IntLit.one)
    in
    let astate_equal =
      PulseArithmetic.and_eq_int ret_val ret_val_equal astate
      >>= PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand index_lhs)
            (AbstractValueOperand index_rhs)
      |> map_continue
    in
    let astate_notequal =
      PulseArithmetic.and_eq_int ret_val ret_val_notequal astate
      >>= PulseArithmetic.prune_binop ~negated:false Ne (AbstractValueOperand index_lhs)
            (AbstractValueOperand index_rhs)
      |> map_continue
    in
    [astate_equal; astate_notequal]


  let operator_star ~desc iter : model =
   fun {path; location; ret} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<+> astate, pointer, (elem, _) =
      to_elem_pointed_by_iterator path Read location iter astate
    in
    PulseOperations.write_id (fst ret) (elem, event :: snd pointer) astate


  let operator_step step ~desc iter : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let index_new = AbstractValue.mk_fresh () in
    let<*> astate, pointer, _ =
      to_elem_pointed_by_iterator path Read ~step:(Some step) location iter astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:pointer
        ~obj:(index_new, event :: snd pointer)
        astate
    in
    astate
end

module JavaIterator = struct
  let constructor ~desc init : model =
   fun {path; location; ret} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ref = (AbstractValue.mk_fresh (), [event]) in
    let<+> astate =
      GenericArrayBackedCollectionIterator.construct path location event ~init ~ref astate
    in
    PulseOperations.write_id (fst ret) ref astate


  (* {curr -> v_c} is modified to {curr -> v_fresh} and returns array[v_c] *)
  let next ~desc iter : model =
   fun {path; location; ret} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let new_index = AbstractValue.mk_fresh () in
    let<*> astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollectionIterator.to_internal_pointer path Read location iter astate
    in
    let<*> astate, (curr_elem_val, curr_elem_hist) =
      GenericArrayBackedCollection.element path location iter curr_index astate
    in
    let<+> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer
        ~obj:(new_index, event :: curr_index_hist)
        astate
    in
    PulseOperations.write_id (fst ret) (curr_elem_val, event :: curr_elem_hist) astate


  (* {curr -> v_c } is modified to {curr -> v_fresh} and writes to array[v_c] *)
  let remove ~desc iter : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let new_index = AbstractValue.mk_fresh () in
    let<*> astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollectionIterator.to_internal_pointer path Read location iter astate
    in
    let<*> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer
        ~obj:(new_index, event :: curr_index_hist)
        astate
    in
    let new_elem = AbstractValue.mk_fresh () in
    let<*> astate, arr = GenericArrayBackedCollection.eval path Read location iter astate in
    let<+> astate =
      PulseOperations.write_arr_index path location ~ref:arr ~index:curr_index
        ~obj:(new_elem, event :: curr_index_hist)
        astate
    in
    astate
end

module StdVector = struct
  let reallocate_internal_array path trace vector vector_f location astate =
    let* astate, array_address =
      GenericArrayBackedCollection.eval path NoAccess location vector astate
    in
    PulseOperations.invalidate_array_elements path location (StdVector vector_f) array_address
      astate
    >>= PulseOperations.invalidate_deref_access path location (StdVector vector_f) vector
          GenericArrayBackedCollection.access
    >>= PulseOperations.havoc_deref_field path location vector GenericArrayBackedCollection.field
          trace


  let init_list_constructor this init_list : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "std::vector::vector()"; location; in_call= []} in
    let<*> astate, init_copy = PulseOperations.shallow_copy path location init_list astate in
    let<+> astate =
      PulseOperations.write_deref_field path location ~ref:this GenericArrayBackedCollection.field
        ~obj:(fst init_copy, event :: snd init_copy)
        astate
    in
    astate


  let invalidate_references vector_f vector : model =
   fun {path; location} astate ->
    let crumb =
      ValueHistory.Call
        { f= Model (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
        ; location
        ; in_call= [] }
    in
    let<+> astate = reallocate_internal_array path [crumb] vector vector_f location astate in
    astate


  let at ~desc vector index : model =
   fun {path; location; ret} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<+> astate, (addr, hist) =
      GenericArrayBackedCollection.element path location vector (fst index) astate
    in
    PulseOperations.write_id (fst ret) (addr, event :: hist) astate


  let vector_begin vector iter : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "std::vector::begin()"; location; in_call= []} in
    let pointer_hist = event :: snd iter in
    let pointer_val = (AbstractValue.mk_fresh (), pointer_hist) in
    let index_zero = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_eq_int index_zero IntLit.zero astate in
    let<*> astate, ((arr_addr, _) as arr) =
      GenericArrayBackedCollection.eval path Read location vector astate
    in
    let<*> astate, _ =
      GenericArrayBackedCollection.eval_element path location arr index_zero astate
    in
    let<+> astate =
      PulseOperations.write_deref_field path location ~ref:iter GenericArrayBackedCollection.field
        ~obj:(arr_addr, pointer_hist) astate
      >>= PulseOperations.write_field path location ~ref:iter
            GenericArrayBackedCollectionIterator.internal_pointer ~obj:pointer_val
      >>= PulseOperations.write_deref path location ~ref:pointer_val ~obj:(index_zero, pointer_hist)
    in
    astate


  let vector_end vector iter : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "std::vector::end()"; location; in_call= []} in
    let<*> astate, (arr_addr, _) =
      GenericArrayBackedCollection.eval path Read location vector astate
    in
    let<*> astate, (pointer_addr, _) =
      GenericArrayBackedCollection.eval_pointer_to_last_element path location vector astate
    in
    let pointer_hist = event :: snd iter in
    let pointer_val = (pointer_addr, pointer_hist) in
    let<*> astate =
      PulseOperations.write_deref_field path location ~ref:iter GenericArrayBackedCollection.field
        ~obj:(arr_addr, pointer_hist) astate
    in
    let<+> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer ~obj:pointer_val astate
    in
    astate


  let reserve vector : model =
   fun {path; location} astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::reserve()"; location; in_call= []} in
    let<+> astate =
      reallocate_internal_array path [crumb] vector Reserve location astate
      >>| AddressAttributes.std_vector_reserve (fst vector)
    in
    astate


  let push_back vector : model =
   fun {path; location} astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::push_back()"; location; in_call= []} in
    if AddressAttributes.is_std_vector_reserved (fst vector) astate then
      (* assume that any call to [push_back] is ok after one called [reserve] on the same vector
         (a perfect analysis would also make sure we don't exceed the reserved size) *)
      ok_continue astate
    else
      (* simulate a re-allocation of the underlying array every time an element is added *)
      let<+> astate = reallocate_internal_array path [crumb] vector PushBack location astate in
      astate


  let empty vector : model =
   fun {path; location; ret= ret_id, _} astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::empty()"; location; in_call= []} in
    let<+> astate, (value_addr, value_hist) =
      GenericArrayBackedCollection.eval_is_empty path location vector astate
    in
    PulseOperations.write_id ret_id (value_addr, crumb :: value_hist) astate
end

module Java = struct
  let mk_java_field pkg clazz field =
    Fieldname.make (Typ.JavaClass (JavaClassName.make ~package:(Some pkg) ~classname:clazz)) field


  let load_field path field location obj astate =
    let* astate, field_addr =
      PulseOperations.eval_access path Read location obj (FieldAccess field) astate
    in
    let+ astate, field_val =
      PulseOperations.eval_access path Read location field_addr Dereference astate
    in
    (astate, field_addr, field_val)


  let write_field path field new_val location addr astate =
    let* astate, field_addr =
      PulseOperations.eval_access path Write location addr (FieldAccess field) astate
    in
    PulseOperations.write_deref path location ~ref:field_addr ~obj:new_val astate


  let instance_of (argv, hist) typeexpr : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "Java.instanceof"; location; in_call= []} in
    let res_addr = AbstractValue.mk_fresh () in
    match typeexpr with
    | Exp.Sizeof {typ} ->
        let<+> astate = PulseArithmetic.and_equal_instanceof res_addr argv typ astate in
        PulseOperations.write_id ret_id (res_addr, event :: hist) astate
    (* The type expr is sometimes a Var expr but this is not expected.
       This seems to be introduced by inline mechanism of Java synthetic methods during preanalysis *)
    | _ ->
        astate |> ok_continue
end

module JavaCollection = struct
  let pkg_name = "java.util"

  let class_name = "Collection"

  let fst_field = Java.mk_java_field pkg_name class_name "__infer_model_backing_collection_fst"

  let snd_field = Java.mk_java_field pkg_name class_name "__infer_model_backing_collection_snd"

  let is_empty_field =
    Java.mk_java_field pkg_name class_name "__infer_model_backing_collection_empty"


  let init ~desc this : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let fresh_val = (AbstractValue.mk_fresh (), [event]) in
    let is_empty_value = AbstractValue.mk_fresh () in
    let init_value = AbstractValue.mk_fresh () in
    (* The two internal fields are initially set to null *)
    let<*> astate =
      Java.write_field path fst_field (init_value, [event]) location fresh_val astate
    in
    let<*> astate =
      Java.write_field path snd_field (init_value, [event]) location fresh_val astate
    in
    (* The empty field is initially set to true *)
    let<*> astate =
      Java.write_field path is_empty_field (is_empty_value, [event]) location fresh_val astate
    in
    let<*> astate =
      PulseOperations.write_deref path location ~ref:this ~obj:fresh_val astate
      >>= PulseArithmetic.and_eq_int init_value IntLit.zero
      >>= PulseArithmetic.and_eq_int is_empty_value IntLit.one
    in
    astate |> ok_continue


  let add ~desc coll new_elem : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ret_value = AbstractValue.mk_fresh () in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    (* reads snd_field from collection *)
    let<*> astate, snd_addr, snd_val = Java.load_field path snd_field location coll_val astate in
    (* fst_field takes value stored in snd_field *)
    let<*> astate = Java.write_field path fst_field snd_val location coll_val astate in
    (* snd_field takes new value given *)
    let<*> astate = PulseOperations.write_deref path location ~ref:snd_addr ~obj:new_elem astate in
    (* Collection.add returns a boolean, in this case the return always has value one *)
    let<*> astate =
      PulseArithmetic.and_eq_int ret_value IntLit.one astate
      >>| PulseOperations.write_id ret_id (ret_value, [event])
    in
    (* empty field set to false if the collection was empty *)
    let<*> astate, _, (is_empty_val, hist) =
      Java.load_field path is_empty_field location coll_val astate
    in
    if PulseArithmetic.is_known_zero astate is_empty_val then astate |> ok_continue
    else
      let is_empty_new_val = AbstractValue.mk_fresh () in
      let<*> astate =
        Java.write_field path is_empty_field
          (is_empty_new_val, event :: hist)
          location coll_val astate
        >>= PulseArithmetic.and_eq_int is_empty_new_val IntLit.zero
      in
      astate |> ok_continue


  let update path coll new_val new_val_hist event location ret_id astate =
    (* case0: element not present in collection *)
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, fst_val = Java.load_field path fst_field location coll_val astate in
    let<*> astate, _, snd_val = Java.load_field path snd_field location coll_val astate in
    let is_empty_val = AbstractValue.mk_fresh () in
    let<*> astate' =
      Java.write_field path is_empty_field (is_empty_val, [event]) location coll_val astate
    in
    (* case1: fst_field is updated *)
    let astate1 =
      Java.write_field path fst_field (new_val, event :: new_val_hist) location coll astate'
      >>| PulseOperations.write_id ret_id fst_val
      |> map_continue
    in
    (* case2: snd_field is updated *)
    let astate2 =
      Java.write_field path snd_field (new_val, event :: new_val_hist) location coll astate'
      >>| PulseOperations.write_id ret_id snd_val
      |> map_continue
    in
    [Ok (continue astate); astate1; astate2]


  let set coll (new_val, new_val_hist) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "Collection.set()"; location; in_call= []} in
    update path coll new_val new_val_hist event location ret_id astate


  let remove_at path ~desc coll location ret_id astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let new_val = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_eq_int new_val IntLit.zero astate in
    update path coll new_val [] event location ret_id astate


  (* Auxiliary function that updates the state by:
     (1) assuming that value to be removed is equal to field value
     (2) assigning field to null value
     Collection.remove should return a boolean. In this case, the return val is one *)
  let remove_elem_found path coll_val elem field_addr field_val ret_id location event astate =
    let null_val = AbstractValue.mk_fresh () in
    let ret_val = AbstractValue.mk_fresh () in
    let is_empty_val = AbstractValue.mk_fresh () in
    let* astate =
      Java.write_field path is_empty_field (is_empty_val, [event]) location coll_val astate
    in
    let* astate =
      PulseArithmetic.and_eq_int null_val IntLit.zero astate
      >>= PulseArithmetic.and_eq_int ret_val IntLit.one
    in
    let* astate =
      PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand elem)
        (AbstractValueOperand field_val) astate
    in
    let+ astate =
      PulseOperations.write_deref path location ~ref:field_addr ~obj:(null_val, [event]) astate
    in
    PulseOperations.write_id ret_id (ret_val, [event]) astate


  let remove_obj path ~desc coll (elem, _) location ret_id astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, fst_addr, (fst_val, _) =
      Java.load_field path fst_field location coll_val astate
    in
    let<*> astate, snd_addr, (snd_val, _) =
      Java.load_field path snd_field location coll_val astate
    in
    (* case1: given element is equal to fst_field *)
    let astate1 =
      remove_elem_found path coll_val elem fst_addr fst_val ret_id location event astate
      |> map_continue
    in
    (* case2: given element is equal to snd_field *)
    let astate2 =
      remove_elem_found path coll_val elem snd_addr snd_val ret_id location event astate
      |> map_continue
    in
    (* case 3: given element is not equal to the fst AND not equal to the snd *)
    let astate3 =
      PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand elem)
        (AbstractValueOperand fst_val) astate
      >>= PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand elem)
            (AbstractValueOperand snd_val)
      |> map_continue
    in
    [astate1; astate2; astate3]


  let remove ~desc args : model =
   fun {path; location; ret= ret_id, _} astate ->
    match args with
    | [ {ProcnameDispatcher.Call.FuncArg.arg_payload= coll_arg}
      ; {ProcnameDispatcher.Call.FuncArg.arg_payload= elem_arg; typ} ] -> (
      match typ.desc with
      | Tint _ ->
          (* Case of remove(int index) *)
          remove_at path ~desc coll_arg location ret_id astate
      | _ ->
          (* Case of remove(Object o) *)
          remove_obj path ~desc coll_arg elem_arg location ret_id astate )
    | _ ->
        astate |> ok_continue


  let is_empty ~desc coll : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, (is_empty_val, hist) =
      Java.load_field path is_empty_field location coll_val astate
    in
    PulseOperations.write_id ret_id (is_empty_val, event :: hist) astate |> ok_continue


  let clear ~desc coll : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let null_val = AbstractValue.mk_fresh () in
    let is_empty_val = AbstractValue.mk_fresh () in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate = Java.write_field path fst_field (null_val, [event]) location coll_val astate in
    let<*> astate = Java.write_field path snd_field (null_val, [event]) location coll_val astate in
    let<*> astate =
      Java.write_field path is_empty_field (is_empty_val, [event]) location coll_val astate
    in
    let<+> astate =
      PulseArithmetic.and_eq_int null_val IntLit.zero astate
      >>= PulseArithmetic.and_eq_int is_empty_val IntLit.one
    in
    astate


  (* Auxiliary function that changes the state by
     (1) assuming that internal is_empty field has value one
     (2) in such case we can return 0 *)
  let get_elem_coll_is_empty path is_empty_val is_empty_expected_val event location ret_id astate =
    let not_found_val = AbstractValue.mk_fresh () in
    let* astate =
      PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand is_empty_val)
        (AbstractValueOperand is_empty_expected_val) astate
      >>= PulseArithmetic.and_eq_int not_found_val IntLit.zero
      >>= PulseArithmetic.and_eq_int is_empty_expected_val IntLit.one
    in
    let hist = [event] in
    let astate = PulseOperations.write_id ret_id (not_found_val, hist) astate in
    PulseOperations.invalidate path
      (StackAddress (Var.of_id ret_id, hist))
      location (ConstantDereference IntLit.zero)
      (not_found_val, [event])
      astate


  (* Auxiliary function that splits the state into three, considering the case that
     the internal is_empty field is not known to have value 1 *)
  let get_elem_coll_not_known_empty elem found_val fst_val snd_val astate =
    (* case 1: given element is not equal to the fst AND not equal to the snd *)
    let astate1 =
      PulseArithmetic.prune_binop ~negated:true Eq (AbstractValueOperand elem)
        (AbstractValueOperand fst_val) astate
      >>= PulseArithmetic.prune_binop ~negated:true Eq (AbstractValueOperand elem)
            (AbstractValueOperand snd_val)
      |> map_continue
    in
    (* case 2: given element is equal to fst_field *)
    let astate2 =
      PulseArithmetic.and_positive found_val astate
      >>= PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand elem)
            (AbstractValueOperand fst_val)
      |> map_continue
    in
    (* case 3: given element is equal to snd_field *)
    let astate3 =
      PulseArithmetic.and_positive found_val astate
      >>= PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand elem)
            (AbstractValueOperand snd_val)
      |> map_continue
    in
    [astate1; astate2; astate3]


  let get ~desc coll (elem, _) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, (is_empty_val, _) =
      Java.load_field path is_empty_field location coll_val astate
    in
    (* case 1: collection is empty *)
    let true_val = AbstractValue.mk_fresh () in
    let astate1 =
      get_elem_coll_is_empty path is_empty_val true_val event location ret_id astate |> map_continue
    in
    (* case 2: collection is not known to be empty *)
    let found_val = AbstractValue.mk_fresh () in
    let astates2 =
      let<*> astate2, _, (fst_val, _) = Java.load_field path fst_field location coll_val astate in
      let<*> astate2, _, (snd_val, _) = Java.load_field path snd_field location coll_val astate2 in
      let<*> astate2 =
        PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand is_empty_val)
          (AbstractValueOperand true_val) astate2
        >>= PulseArithmetic.and_eq_int true_val IntLit.one
        >>| PulseOperations.write_id ret_id (found_val, [event])
      in
      get_elem_coll_not_known_empty elem found_val fst_val snd_val astate2
    in
    astate1 :: astates2
end

module JavaInteger = struct
  let internal_int = Java.mk_java_field "java.lang" "Integer" "__infer_model_backing_int"

  let load_backing_int path location this astate =
    let* astate, obj = PulseOperations.eval_access path Read location this Dereference astate in
    Java.load_field path internal_int location obj astate


  let construct path this_address init_value event location astate =
    let this = (AbstractValue.mk_fresh (), [event]) in
    let* astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    let* astate = PulseOperations.write_deref path location ~ref:int_field ~obj:init_value astate in
    PulseOperations.write_deref path location ~ref:this_address ~obj:this astate


  let init this_address init_value : model =
   fun {path; location} astate ->
    let event = ValueHistory.Call {f= Model "Integer.init"; location; in_call= []} in
    let<+> astate = construct path this_address init_value event location astate in
    astate


  let equals this arg : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, _int_addr1, (int1, hist) = load_backing_int path location this astate in
    let<*> astate, _int_addr2, (int2, _) = load_backing_int path location arg astate in
    let binop_addr = AbstractValue.mk_fresh () in
    let<+> astate, binop_addr =
      PulseArithmetic.eval_binop binop_addr Binop.Eq (AbstractValueOperand int1)
        (AbstractValueOperand int2) astate
    in
    PulseOperations.write_id ret_id (binop_addr, hist) astate


  let int_val this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, _int_addr1, int_value_hist = load_backing_int path location this astate in
    PulseOperations.write_id ret_id int_value_hist astate |> ok_continue


  let value_of init_value : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "Integer.valueOf"; location; in_call= []} in
    let new_alloc = (AbstractValue.mk_fresh (), [event]) in
    let<+> astate = construct path new_alloc init_value event location astate in
    PulseOperations.write_id ret_id new_alloc astate
end

module JavaPreconditions = struct
  let check_not_null (address, hist) : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model "Preconditions.checkNotNull"; location; in_call= []} in
    let<+> astate = PulseArithmetic.prune_positive address astate in
    PulseOperations.write_id ret_id (address, event :: hist) astate


  let check_state_argument (address, _) : model =
   fun _ astate ->
    let<+> astate = PulseArithmetic.prune_positive address astate in
    astate
end

module Android = struct
  let text_utils_is_empty ~desc ((addr, hist) as addr_hist) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ret_val = AbstractValue.mk_fresh () in
    let astate_null =
      PulseArithmetic.prune_eq_zero addr astate
      >>= PulseArithmetic.and_eq_int ret_val IntLit.one
      >>| PulseOperations.write_id ret_id (ret_val, event :: hist)
      |> map_continue
    in
    let astate_not_null =
      let<*> astate = PulseArithmetic.prune_positive addr astate in
      let<*> astate, (len_addr, hist) =
        PulseOperations.eval_access path Read location addr_hist string_length_access astate
      in
      let astate = PulseOperations.write_id ret_id (ret_val, event :: hist) astate in
      let astate_empty =
        PulseArithmetic.prune_eq_zero len_addr astate
        >>= PulseArithmetic.and_eq_int ret_val IntLit.one
        |> map_continue
      in
      let astate_not_empty =
        PulseArithmetic.prune_positive len_addr astate
        >>= PulseArithmetic.and_eq_int ret_val IntLit.zero
        |> map_continue
      in
      [astate_empty; astate_not_empty]
    in
    astate_null :: astate_not_null
end

module Erlang = struct
  let error_badkey : model =
   fun {location} astate ->
    [ Error
        (ReportableError {astate; diagnostic= ErlangError (Badkey {calling_context= []; location})})
    ]


  let error_badmap : model =
   fun {location} astate ->
    [ Error
        (ReportableError {astate; diagnostic= ErlangError (Badmap {calling_context= []; location})})
    ]


  let error_badmatch : model =
   fun {location} astate ->
    [ Error
        (ReportableError {astate; diagnostic= ErlangError (Badmatch {calling_context= []; location})}
        ) ]


  let error_badrecord : model =
   fun {location} astate ->
    [ Error
        (ReportableError
           {astate; diagnostic= ErlangError (Badrecord {calling_context= []; location})} ) ]


  let error_case_clause : model =
   fun {location} astate ->
    [ Error
        (ReportableError
           {astate; diagnostic= ErlangError (Case_clause {calling_context= []; location})} ) ]


  let error_function_clause : model =
   fun {location} astate ->
    [ Error
        (ReportableError
           {astate; diagnostic= ErlangError (Function_clause {calling_context= []; location})} ) ]


  let error_if_clause : model =
   fun {location} astate ->
    [ Error
        (ReportableError
           {astate; diagnostic= ErlangError (If_clause {calling_context= []; location})} ) ]


  let write_field_and_deref path location ~struct_addr ~field_addr ~field_val field_name astate =
    let* astate =
      PulseOperations.write_field path location ~ref:struct_addr field_name ~obj:field_addr astate
    in
    PulseOperations.write_deref path location ~ref:field_addr ~obj:field_val astate


  let write_dynamic_type_and_return (addr_val, hist) typ ret_id astate =
    let typ = Typ.mk_struct (ErlangType typ) in
    let astate = PulseOperations.add_dynamic_type typ addr_val astate in
    PulseOperations.write_id ret_id (addr_val, hist) astate


  let make_nil : model =
   fun {location; ret= ret_id, _} astate ->
    let event = ValueHistory.Allocation {f= Model "[]"; location} in
    let addr_nil = (AbstractValue.mk_fresh (), [event]) in
    let astate = write_dynamic_type_and_return addr_nil Nil ret_id astate in
    [Ok (ContinueProgram astate)]


  let make_cons head tail : model =
   fun {location; path; ret= ret_id, _} astate ->
    let event = ValueHistory.Allocation {f= Model "[X|Xs]"; location} in
    let addr_head = (AbstractValue.mk_fresh (), [event]) in
    let addr_tail = (AbstractValue.mk_fresh (), [event]) in
    let addr_cons = (AbstractValue.mk_fresh (), [event]) in
    let field name = Fieldname.make (ErlangType Cons) name in
    let<*> astate =
      write_field_and_deref path location ~struct_addr:addr_cons ~field_addr:addr_head
        ~field_val:head (field ErlangTypeName.cons_head) astate
    in
    let<+> astate =
      write_field_and_deref path location ~struct_addr:addr_cons ~field_addr:addr_tail
        ~field_val:tail (field ErlangTypeName.cons_tail) astate
    in
    write_dynamic_type_and_return addr_cons Cons ret_id astate


  let make_tuple (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let tuple_size = List.length args in
    let tuple_typ_name : Typ.name = ErlangType (Tuple tuple_size) in
    let event = ValueHistory.Allocation {f= Model "{}"; location} in
    let addr_tuple = (AbstractValue.mk_fresh (), [event]) in
    let addr_elems = List.map ~f:(function _ -> (AbstractValue.mk_fresh (), [event])) args in
    let mk_field name = Fieldname.make tuple_typ_name name in
    let field_names = ErlangTypeName.tuple_field_names tuple_size in
    let get_payload (arg : 'a ProcnameDispatcher.Call.FuncArg.t) = arg.arg_payload in
    let arg_payloads = List.map ~f:get_payload args in
    let addr_elems_fields_payloads =
      List.zip_exn addr_elems (List.zip_exn field_names arg_payloads)
    in
    let write_tuple_field astate (addr_elem, (field_name, payload)) =
      write_field_and_deref path location ~struct_addr:addr_tuple ~field_addr:addr_elem
        ~field_val:payload (mk_field field_name) astate
    in
    let<+> astate = List.fold_result addr_elems_fields_payloads ~init:astate ~f:write_tuple_field in
    write_dynamic_type_and_return addr_tuple (Tuple tuple_size) ret_id astate


  (** Maps are currently approximated to store only the latest key/value *)
  let mk_map_field f = Fieldname.make (ErlangType Map) f

  let map_key_field = mk_map_field "__infer_model_backing_map_key"

  let map_value_field = mk_map_field "__infer_model_backing_map_value"

  let map_is_empty_field = mk_map_field "__infer_model_backing_map_is_empty"

  let map_create (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let event = ValueHistory.Allocation {f= Model "#{}"; location} in
    let addr_map = (AbstractValue.mk_fresh (), [event]) in
    let addr_is_empty = (AbstractValue.mk_fresh (), [event]) in
    let is_empty_value = AbstractValue.mk_fresh () in
    let fresh_val = (is_empty_value, [event]) in
    let is_empty_lit = match args with [] -> IntLit.one | _ -> IntLit.zero in
    (* Reverse the list so we can get last key/value *)
    let<*> astate =
      match List.rev args with
      (* Non-empty map: we just consider the last key/value, rest is ignored (approximation) *)
      | value_arg :: key_arg :: _ ->
          let addr_key = (AbstractValue.mk_fresh (), [event]) in
          let addr_value = (AbstractValue.mk_fresh (), [event]) in
          write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
            ~field_val:key_arg.arg_payload map_key_field astate
          >>= write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
                ~field_val:value_arg.arg_payload map_value_field
      | _ :: _ ->
          L.die InternalError "Map create got one argument (requires even number)"
      (* Empty map *)
      | [] ->
          Ok astate
    in
    let<*> astate =
      write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
        ~field_val:fresh_val map_is_empty_field astate
    in
    let<+> astate = PulseArithmetic.and_eq_int is_empty_value is_empty_lit astate in
    write_dynamic_type_and_return addr_map Map ret_id astate


  let make_astate_badmap (map_val, _map_hist) data astate =
    let typ = Typ.mk_struct (ErlangType Map) in
    let instanceof_val = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof instanceof_val map_val typ astate in
    let<*> astate = PulseArithmetic.prune_eq_zero instanceof_val astate in
    error_badmap data astate


  let make_astate_goodmap (map_val, _map_hist) astate =
    let typ = Typ.mk_struct (ErlangType Map) in
    let instanceof_val = AbstractValue.mk_fresh () in
    let* astate = PulseArithmetic.and_equal_instanceof instanceof_val map_val typ astate in
    PulseArithmetic.prune_positive instanceof_val astate


  let map_is_key (key, _key_history) map : model =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    match Java.load_field path map_is_empty_field location map astate with
    | Error _ ->
        (* Field not found, seems like it's not a map *)
        error_badmap data astate
    | Ok (astate, _isempty_addr, (is_empty, _isempty_hist)) ->
        let ret_val_true = AbstractValue.mk_fresh () in
        let ret_val_false = AbstractValue.mk_fresh () in
        let event = ValueHistory.Call {f= Model "map_is_key"; location; in_call= []} in
        (* Return 3 cases:
         * - Error & assume not map
         * - Ok & assume map & assume empty & return false
         * - Ok & assume map & assume not empty & assume key is the tracked key & return true
         *)
        let astate_badmap = make_astate_badmap map data astate in
        let astate_empty =
          let* astate = make_astate_goodmap map astate in
          let* astate = PulseArithmetic.prune_positive is_empty astate in
          let+ astate = PulseArithmetic.and_eq_int ret_val_false IntLit.zero astate in
          PulseOperations.write_id ret_id (ret_val_false, [event]) astate |> continue
        in
        let astate_haskey =
          let* astate = make_astate_goodmap map astate in
          let* astate = PulseArithmetic.prune_eq_zero is_empty astate in
          let* astate, _key_addr, (tracked_key, _hist) =
            Java.load_field path map_key_field location map astate
          in
          let* astate =
            PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
              (AbstractValueOperand tracked_key) astate
          in
          let+ astate = PulseArithmetic.and_eq_int ret_val_true IntLit.one astate in
          PulseOperations.write_id ret_id (ret_val_true, [event]) astate |> continue
        in
        astate_badmap @ [astate_empty; astate_haskey]


  let map_get (key, _key_history) map : model =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    match Java.load_field path map_is_empty_field location map astate with
    | Error _ ->
        (* Field not found, seems like it's not a map *)
        error_badmap data astate
    | Ok (astate, _isempty_addr, (is_empty, _isempty_hist)) ->
        (* Return 3 cases:
         * - Error & assume not map
         * - Error & assume map & assume empty;
         * - Ok & assume map & assume nonempty & assume key is the tracked key & return tracked value
         *)
        let astate_badmap = make_astate_badmap map data astate in
        let astate_ok =
          let* astate = make_astate_goodmap map astate in
          let* astate = PulseArithmetic.prune_eq_zero is_empty astate in
          let* astate, _key_addr, (tracked_key, _hist) =
            Java.load_field path map_key_field location map astate
          in
          let* astate =
            PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
              (AbstractValueOperand tracked_key) astate
          in
          let+ astate, _value_addr, tracked_value =
            Java.load_field path map_value_field location map astate
          in
          PulseOperations.write_id ret_id tracked_value astate |> continue
        in
        let astate_badkey =
          let<*> astate = make_astate_goodmap map astate in
          let<*> astate = PulseArithmetic.prune_positive is_empty astate in
          error_badkey data astate
        in
        (astate_ok :: astate_badkey) @ astate_badmap


  let map_put key value map : model =
   fun ({location; path; ret= ret_id, _} as data) astate ->
    (* Ignore old map. We only store one key/value so we can simply create a new map. *)
    (* Return 2 cases:
     * - Error & assume not map
     * - Ok & assume map & return new map
     *)
    let event = ValueHistory.Allocation {f= Model "map_put"; location} in
    let astate_badmap = make_astate_badmap map data astate in
    let astate_ok =
      let addr_map = (AbstractValue.mk_fresh (), [event]) in
      let addr_is_empty = (AbstractValue.mk_fresh (), [event]) in
      let is_empty_value = AbstractValue.mk_fresh () in
      let fresh_val = (is_empty_value, [event]) in
      let addr_key = (AbstractValue.mk_fresh (), [event]) in
      let addr_value = (AbstractValue.mk_fresh (), [event]) in
      let<*> astate = make_astate_goodmap map astate in
      let<*> astate =
        write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
          ~field_val:key map_key_field astate
      in
      let<*> astate =
        write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
          ~field_val:value map_value_field astate
      in
      let<+> astate =
        write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
          ~field_val:fresh_val map_is_empty_field astate
        >>= PulseArithmetic.and_eq_int is_empty_value IntLit.zero
      in
      write_dynamic_type_and_return addr_map Map ret_id astate
    in
    astate_ok @ astate_badmap
end

module StringSet = Caml.Set.Make (String)

module ProcNameDispatcher = struct
  let dispatch : (Tenv.t * Procname.t, model, arg_payload) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let pushback_modeled =
      StringSet.of_list
        ["add"; "addAll"; "append"; "delete"; "remove"; "replace"; "poll"; "put"; "putAll"]
    in
    let transfer_ownership_matchers =
      let transfer_ownership_namespace_matchers =
        List.map
          ~f:(fun (namespace, m) ->
            -namespace &:: m $ capt_arg_payload $+...$--> ObjCCoreFoundation.cf_bridging_release )
          Config.pulse_model_transfer_ownership_namespace
      in
      let transfer_ownership_name_matchers =
        List.map
          ~f:(fun m -> -m $ capt_arg_payload $+...$--> ObjCCoreFoundation.cf_bridging_release)
          Config.pulse_model_transfer_ownership
      in
      ( -"NSString" &:: "initWithBytesNoCopy:length:encoding:freeWhenDone:" <>$ any_arg
      $+ capt_arg_payload $+ any_arg $+ any_arg $+ any_arg $--> ObjC.init_with_bytes_free_when_done
      )
      :: ( -"NSData" &:: "initWithBytesNoCopy:length:freeWhenDone:" <>$ any_arg $+ capt_arg_payload
         $+ any_arg $+ any_arg $--> ObjC.init_with_bytes_free_when_done )
      :: transfer_ownership_namespace_matchers
      @ transfer_ownership_name_matchers
    in
    let get_cpp_matchers config ~model =
      let cpp_separator_regex = Str.regexp_string "::" in
      List.filter_map
        ~f:(fun m ->
          match Str.split cpp_separator_regex m with
          | [] ->
              None
          | first :: rest ->
              Some (List.fold rest ~f:( &:: ) ~init:(-first) &--> model m) )
        config
    in
    let abort_matchers =
      get_cpp_matchers ~model:(fun _ -> Misc.early_exit) Config.pulse_model_abort
    in
    let match_regexp_opt r_opt (_tenv, proc_name) _ =
      Option.exists r_opt ~f:(fun r ->
          let s = Procname.to_string proc_name in
          Str.string_match r s 0 )
    in
    let map_context_tenv f (x, _) = f x in
    make_dispatcher
      ( transfer_ownership_matchers @ abort_matchers
      @ [ +BuiltinDecl.(match_builtin free) <>$ capt_arg $--> C.free
        ; +match_regexp_opt Config.pulse_model_free_pattern <>$ capt_arg $+...$--> C.free
        ; +BuiltinDecl.(match_builtin malloc) <>$ capt_exp $--> C.malloc
        ; +match_regexp_opt Config.pulse_model_malloc_pattern <>$ capt_exp $+...$--> C.custom_malloc
        ; -"realloc" <>$ capt_arg $+ capt_exp $--> C.realloc
        ; +match_regexp_opt Config.pulse_model_realloc_pattern
          <>$ capt_arg $+ capt_exp $+...$--> C.custom_realloc
        ; +BuiltinDecl.(match_builtin __delete) <>$ capt_arg $--> Cplusplus.delete
        ; +BuiltinDecl.(match_builtin __delete_array) <>$ capt_arg $--> Cplusplus.delete_array
        ; +BuiltinDecl.(match_builtin __new) <>$ capt_exp $--> Cplusplus.new_
        ; +BuiltinDecl.(match_builtin __new_array) <>$ capt_exp $--> Cplusplus.new_array
        ; +BuiltinDecl.(match_builtin __placement_new) &++> Cplusplus.placement_new
        ; -"random" <>$$--> Misc.nondet ~fn_name:"random"
        ; +BuiltinDecl.(match_builtin objc_cpp_throw) <>--> Misc.early_exit
        ; +BuiltinDecl.(match_builtin __cast)
          <>$ capt_arg_payload $+...$--> Misc.id_first_arg ~desc:"cast"
        ; +BuiltinDecl.(match_builtin abort) <>--> Misc.early_exit
        ; +BuiltinDecl.(match_builtin exit) <>--> Misc.early_exit
        ; +BuiltinDecl.(match_builtin __erlang_make_cons)
          <>$ capt_arg_payload $+ capt_arg_payload $--> Erlang.make_cons
        ; +BuiltinDecl.(match_builtin __erlang_make_tuple) &++> Erlang.make_tuple
        ; +BuiltinDecl.(match_builtin __erlang_make_nil) <>--> Erlang.make_nil
        ; +BuiltinDecl.(match_builtin __erlang_map_create) &++> Erlang.map_create
        ; +BuiltinDecl.(match_builtin __erlang_map_is_key)
          <>$ capt_arg_payload $+ capt_arg_payload $--> Erlang.map_is_key
        ; +BuiltinDecl.(match_builtin __erlang_map_get)
          <>$ capt_arg_payload $+ capt_arg_payload $--> Erlang.map_get
        ; +BuiltinDecl.(match_builtin __erlang_map_put)
          <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> Erlang.map_put
        ; +BuiltinDecl.(match_builtin __erlang_error_badkey) <>--> Erlang.error_badkey
        ; +BuiltinDecl.(match_builtin __erlang_error_badmap) <>--> Erlang.error_badmap
        ; +BuiltinDecl.(match_builtin __erlang_error_badmatch) <>--> Erlang.error_badmatch
        ; +BuiltinDecl.(match_builtin __erlang_error_badrecord) <>--> Erlang.error_badrecord
        ; +BuiltinDecl.(match_builtin __erlang_error_case_clause) <>--> Erlang.error_case_clause
        ; +BuiltinDecl.(match_builtin __erlang_error_function_clause)
          <>--> Erlang.error_function_clause
        ; +BuiltinDecl.(match_builtin __erlang_error_if_clause) <>--> Erlang.error_if_clause
        ; +BuiltinDecl.(match_builtin __infer_initializer_list)
          <>$ capt_arg_payload
          $+...$--> Misc.id_first_arg ~desc:"infer_init_list"
        ; +map_context_tenv (PatternMatch.Java.implements_lang "System")
          &:: "exit" <>--> Misc.early_exit
        ; +BuiltinDecl.(match_builtin __get_array_length) <>--> Misc.return_unknown_size
        ; (* consider that all fbstrings are small strings to avoid false positives due to manual
             ref-counting *)
          -"folly" &:: "fbstring_core" &:: "category" &--> Misc.return_int Int64.zero
        ; -"folly" &:: "DelayedDestruction" &:: "destroy"
          &++> Misc.skip "folly::DelayedDestruction::destroy is modelled as skip"
        ; -"folly" &:: "SocketAddress" &:: "~SocketAddress"
          &++> Misc.skip "folly::SocketAddress's destructor is modelled as skip"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
          $+ any_arg_of_typ (-"folly" &:: "None")
          $--> Optional.assign_none ~desc:"folly::Optional::Optional(=None)"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
          $--> Optional.assign_none ~desc:"folly::Optional::Optional()"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
          $+ capt_arg_payload_of_typ (-"folly" &:: "Optional")
          $--> Optional.assign_optional_value
                 ~desc:"folly::Optional::Optional(folly::Optional<Value> arg)"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.assign_value ~desc:"folly::Optional::Optional(Value arg)"
        ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload
          $+ any_arg_of_typ (-"folly" &:: "None")
          $--> Optional.assign_none ~desc:"folly::Optional::assign(=None)"
        ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload
          $+ capt_arg_payload_of_typ (-"folly" &:: "Optional")
          $--> Optional.assign_optional_value
                 ~desc:"folly::Optional::assign(folly::Optional<Value> arg)"
        ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.assign_value ~desc:"folly::Optional::assign(Value arg)"
        ; -"folly" &:: "Optional" &:: "emplace<>" $ capt_arg_payload
          $+...$--> Optional.emplace ~desc:"folly::Optional::emplace()"
        ; -"folly" &:: "Optional" &:: "emplace" $ capt_arg_payload
          $+...$--> Optional.emplace ~desc:"folly::Optional::emplace()"
        ; -"folly" &:: "Optional" &:: "has_value" <>$ capt_arg_payload
          $+...$--> Optional.has_value ~desc:"folly::Optional::has_value()"
        ; -"folly" &:: "Optional" &:: "reset" <>$ capt_arg_payload
          $+...$--> Optional.assign_none ~desc:"folly::Optional::reset()"
        ; -"folly" &:: "Optional" &:: "value" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"folly::Optional::value()"
        ; -"folly" &:: "Optional" &:: "operator*" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"folly::Optional::operator*()"
        ; -"folly" &:: "Optional" &:: "operator->" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"folly::Optional::operator->()"
        ; -"folly" &:: "Optional" &:: "get_pointer" $ capt_arg_payload
          $+...$--> Optional.get_pointer ~desc:"folly::Optional::get_pointer()"
        ; -"folly" &:: "Optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.value_or ~desc:"folly::Optional::value_or()"
          (* std::optional *)
        ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
          $+ any_arg_of_typ (-"std" &:: "nullopt_t")
          $--> Optional.assign_none ~desc:"std::optional::optional(=nullopt)"
        ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
          $--> Optional.assign_none ~desc:"std::optional::optional()"
        ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
          $+ capt_arg_payload_of_typ (-"std" &:: "optional")
          $--> Optional.assign_optional_value
                 ~desc:"std::optional::optional(std::optional<Value> arg)"
        ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.assign_value ~desc:"std::optional::optional(Value arg)"
        ; -"std" &:: "optional" &:: "operator=" $ capt_arg_payload
          $+ any_arg_of_typ (-"std" &:: "nullopt_t")
          $--> Optional.assign_none ~desc:"std::optional::operator=(None)"
        ; -"std" &:: "optional" &:: "operator=" $ capt_arg_payload
          $+ capt_arg_payload_of_typ (-"std" &:: "optional")
          $--> Optional.assign_optional_value
                 ~desc:"std::optional::operator=(std::optional<Value> arg)"
        ; -"std" &:: "optional" &:: "operator=" $ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.assign_value ~desc:"std::optional::operator=(Value arg)"
        ; -"std" &:: "optional" &:: "emplace<>" $ capt_arg_payload
          $+...$--> Optional.emplace ~desc:"std::optional::emplace()"
        ; -"std" &:: "optional" &:: "emplace" $ capt_arg_payload
          $+...$--> Optional.emplace ~desc:"std::optional::emplace()"
        ; -"std" &:: "optional" &:: "has_value" <>$ capt_arg_payload
          $+...$--> Optional.has_value ~desc:"std::optional::has_value()"
        ; -"std" &:: "__optional_storage_base" &:: "has_value" $ capt_arg_payload
          $+...$--> Optional.has_value ~desc:"std::optional::has_value()"
        ; -"std" &:: "optional" &:: "operator_bool" <>$ capt_arg_payload
          $+...$--> Optional.has_value ~desc:"std::optional::operator_bool()"
        ; -"std" &:: "optional" &:: "reset" <>$ capt_arg_payload
          $+...$--> Optional.assign_none ~desc:"std::optional::reset()"
        ; -"std" &:: "optional" &:: "value" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"std::optional::value()"
        ; -"std" &:: "optional" &:: "operator*" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"std::optional::operator*()"
        ; -"std" &:: "optional" &:: "operator->" <>$ capt_arg_payload
          $+...$--> Optional.value ~desc:"std::optional::operator->()"
        ; -"std" &:: "optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
          $+...$--> Optional.value_or ~desc:"std::optional::value_or()"
          (* end std::optional *)
        ; -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload $+ capt_arg_payload
          $--> StdBasicString.constructor
        ; -"std" &:: "basic_string" &:: "data" <>$ capt_arg_payload $--> StdBasicString.data
        ; -"std" &:: "basic_string" &:: "empty" <>$ capt_arg_payload $--> StdBasicString.empty
        ; -"std" &:: "basic_string" &:: "length" <>$ capt_arg_payload $--> StdBasicString.length
        ; -"std" &:: "basic_string" &:: "~basic_string" <>$ capt_arg_payload
          $--> StdBasicString.destructor
        ; -"std" &:: "function" &:: "function" $ capt_arg_payload $+ capt_arg
          $--> StdFunction.assign ~desc:"std::function::function"
        ; -"std" &:: "function" &:: "operator()" $ capt_arg $++$--> StdFunction.operator_call
        ; -"std" &:: "function" &:: "operator=" $ capt_arg_payload $+ capt_arg
          $--> StdFunction.assign ~desc:"std::function::operator="
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Object")
          &:: "clone" $ capt_arg_payload $--> JavaObject.clone
        ; ( +map_context_tenv (PatternMatch.Java.implements_lang "System")
          &:: "arraycopy" $ capt_arg_payload $+ any_arg $+ capt_arg_payload
          $+...$--> fun src dest -> Misc.shallow_copy_model "System.arraycopy" dest src )
        ; -"std" &:: "atomic" &:: "atomic" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdAtomicInteger.constructor
        ; -"std" &:: "__atomic_base" &:: "fetch_add" <>$ capt_arg_payload $+ capt_arg_payload
          $+ capt_arg_payload $--> StdAtomicInteger.fetch_add
        ; -"std" &:: "__atomic_base" &:: "fetch_sub" <>$ capt_arg_payload $+ capt_arg_payload
          $+ capt_arg_payload $--> StdAtomicInteger.fetch_sub
        ; -"std" &:: "__atomic_base" &:: "exchange" <>$ capt_arg_payload $+ capt_arg_payload
          $+ capt_arg_payload $--> StdAtomicInteger.exchange
        ; -"std" &:: "__atomic_base" &:: "load" <>$ capt_arg_payload $+? capt_arg_payload
          $--> StdAtomicInteger.load
        ; -"std" &:: "__atomic_base" &:: "store" <>$ capt_arg_payload $+ capt_arg_payload
          $+ capt_arg_payload $--> StdAtomicInteger.store
        ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload
          $--> StdAtomicInteger.operator_plus_plus_pre
        ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdAtomicInteger.operator_plus_plus_post
        ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload
          $--> StdAtomicInteger.operator_minus_minus_pre
        ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdAtomicInteger.operator_minus_minus_post
        ; -"std" &:: "__atomic_base"
          &::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
          <>$ capt_arg_payload $+? capt_arg_payload $--> StdAtomicInteger.operator_t
        ; -"std" &:: "integral_constant" < any_typ &+ capt_int
          >::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
          <>--> Misc.return_int
        ; -"std" &:: "vector" &:: "vector" <>$ capt_arg_payload
          $+ capt_arg_payload_of_typ (-"std" &:: "initializer_list")
          $+...$--> StdVector.init_list_constructor
        ; -"std" &:: "__wrap_iter" &:: "__wrap_iter" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> GenericArrayBackedCollectionIterator.constructor ~desc:"iterator constructor"
        ; -"std" &:: "__wrap_iter" &:: "operator*" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_star ~desc:"iterator operator*"
        ; -"std" &:: "__wrap_iter" &:: "operator++" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_step `PlusPlus
                 ~desc:"iterator operator++"
        ; -"std" &:: "__wrap_iter" &:: "operator--" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_step `MinusMinus
                 ~desc:"iterator operator--"
        ; -"std" &:: "operator=="
          $ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
          $+ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
          $--> GenericArrayBackedCollectionIterator.operator_compare `Equal
                 ~desc:"iterator operator=="
        ; -"std" &:: "operator!="
          $ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
          $+ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
          $--> GenericArrayBackedCollectionIterator.operator_compare `NotEqual
                 ~desc:"iterator operator!="
        ; -"__gnu_cxx" &:: "__normal_iterator" &:: "__normal_iterator" <>$ capt_arg_payload
          $+ capt_arg_payload
          $+...$--> GenericArrayBackedCollectionIterator.constructor ~desc:"iterator constructor"
        ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator*" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_star ~desc:"iterator operator*"
        ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator++" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_step `PlusPlus
                 ~desc:"iterator operator++"
        ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator--" <>$ capt_arg_payload
          $--> GenericArrayBackedCollectionIterator.operator_step `MinusMinus
                 ~desc:"iterator operator--"
        ; -"__gnu_cxx" &:: "operator=="
          $ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
          $+ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
          $--> GenericArrayBackedCollectionIterator.operator_compare `Equal
                 ~desc:"iterator operator=="
        ; -"__gnu_cxx" &:: "operator!="
          $ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
          $+ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
          $--> GenericArrayBackedCollectionIterator.operator_compare `NotEqual
                 ~desc:"iterator operator!="
        ; -"std" &:: "vector" &:: "assign" <>$ capt_arg_payload
          $+...$--> StdVector.invalidate_references Assign
        ; -"std" &:: "vector" &:: "at" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.at ~desc:"std::vector::at()"
        ; -"std" &:: "vector" &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.vector_begin
        ; -"std" &:: "vector" &:: "end" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.vector_end
        ; -"std" &:: "vector" &:: "clear" <>$ capt_arg_payload
          $--> StdVector.invalidate_references Clear
        ; -"std" &:: "vector" &:: "emplace" $ capt_arg_payload
          $+...$--> StdVector.invalidate_references Emplace
        ; -"std" &:: "vector" &:: "emplace_back" $ capt_arg_payload
          $+...$--> StdVector.invalidate_references EmplaceBack
        ; -"std" &:: "vector" &:: "insert" <>$ capt_arg_payload
          $+...$--> StdVector.invalidate_references Insert
        ; -"std" &:: "vector" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.at ~desc:"std::vector::at()"
        ; -"std" &:: "vector" &:: "shrink_to_fit" <>$ capt_arg_payload
          $--> StdVector.invalidate_references ShrinkToFit
        ; -"std" &:: "vector" &:: "push_back" <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; -"std" &:: "vector" &:: "empty" <>$ capt_arg_payload $+...$--> StdVector.empty
        ; -"maps" &:: "is_key" <>$ capt_arg_payload $+ capt_arg_payload $--> Erlang.map_is_key
        ; -"maps" &:: "get" <>$ capt_arg_payload $+ capt_arg_payload $--> Erlang.map_get
        ; -"maps" &:: "put" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
          $--> Erlang.map_put
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "<init>" <>$ capt_arg_payload
          $--> JavaCollection.init ~desc:"Collection.init()"
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "add" <>$ capt_arg_payload $+ capt_arg_payload
          $--> JavaCollection.add ~desc:"Collection.add"
        ; +map_context_tenv PatternMatch.Java.implements_list
          &:: "add" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
          $--> JavaCollection.add ~desc:"Collection.add()"
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "remove"
          &++> JavaCollection.remove ~desc:"Collection.remove"
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "isEmpty" <>$ capt_arg_payload
          $--> JavaCollection.is_empty ~desc:"Collection.isEmpty()"
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "clear" <>$ capt_arg_payload
          $--> JavaCollection.clear ~desc:"Collection.clear()"
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "<init>" <>$ capt_arg_payload
          $--> JavaCollection.init ~desc:"Map.init()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "put" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> JavaCollection.add ~desc:"Map.put()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "remove"
          &++> JavaCollection.remove ~desc:"Map.remove()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "get" <>$ capt_arg_payload $+ capt_arg_payload
          $--> JavaCollection.get ~desc:"Map.get()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "containsKey" <>$ capt_arg_payload $+ capt_arg_payload
          $--> JavaCollection.get ~desc:"Map.containsKey()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "isEmpty" <>$ capt_arg_payload
          $--> JavaCollection.is_empty ~desc:"Map.isEmpty()"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "clear" <>$ capt_arg_payload
          $--> JavaCollection.clear ~desc:"Map.clear()"
        ; +map_context_tenv PatternMatch.Java.implements_queue
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +map_context_tenv (PatternMatch.Java.implements_lang "StringBuilder")
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +map_context_tenv (PatternMatch.Java.implements_lang "StringBuilder")
          &:: "setLength" <>$ capt_arg_payload
          $+...$--> StdVector.invalidate_references ShrinkToFit
        ; +map_context_tenv (PatternMatch.Java.implements_lang "String")
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
          &:: "<init>" $ capt_arg_payload $+ capt_arg_payload $--> JavaInteger.init
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
          &:: "equals" $ capt_arg_payload $+ capt_arg_payload $--> JavaInteger.equals
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
          &:: "intValue" <>$ capt_arg_payload $--> JavaInteger.int_val
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
          &:: "valueOf" <>$ capt_arg_payload $--> JavaInteger.value_of
        ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
          &:: "checkNotNull" $ capt_arg_payload $+...$--> JavaPreconditions.check_not_null
        ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
          &:: "checkState" $ capt_arg_payload $+...$--> JavaPreconditions.check_state_argument
        ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
          &:: "checkArgument" $ capt_arg_payload $+...$--> JavaPreconditions.check_state_argument
        ; +map_context_tenv PatternMatch.Java.implements_iterator
          &:: "remove" <>$ capt_arg_payload
          $+...$--> JavaIterator.remove ~desc:"remove"
        ; +map_context_tenv PatternMatch.Java.implements_map
          &:: "putAll" <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> StdVector.reserve
        ; +map_context_tenv PatternMatch.Java.implements_collection
          &:: "get" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.at ~desc:"Collection.get()"
        ; +map_context_tenv PatternMatch.Java.implements_list
          &:: "set" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $--> JavaCollection.set
        ; +map_context_tenv PatternMatch.Java.implements_iterator
          &:: "hasNext"
          &--> Misc.nondet ~fn_name:"Iterator.hasNext()"
        ; +map_context_tenv PatternMatch.Java.implements_enumeration
          &:: "hasMoreElements"
          &--> Misc.nondet ~fn_name:"Enumeration.hasMoreElements()"
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Object")
          &:: "equals"
          &--> Misc.nondet ~fn_name:"Object.equals"
        ; +map_context_tenv (PatternMatch.Java.implements_lang "Iterable")
          &:: "iterator" <>$ capt_arg_payload
          $+...$--> JavaIterator.constructor ~desc:"Iterable.iterator"
        ; +map_context_tenv PatternMatch.Java.implements_iterator
          &:: "next" <>$ capt_arg_payload
          $!--> JavaIterator.next ~desc:"Iterator.next()"
        ; +BuiltinDecl.(match_builtin __instanceof)
          <>$ capt_arg_payload $+ capt_exp $--> Java.instance_of
        ; ( +map_context_tenv PatternMatch.Java.implements_enumeration
          &:: "nextElement" <>$ capt_arg_payload
          $!--> fun x ->
          StdVector.at ~desc:"Enumeration.nextElement" x (AbstractValue.mk_fresh (), []) )
        ; +map_context_tenv (PatternMatch.Java.implements_android "text.TextUtils")
          &:: "isEmpty" <>$ capt_arg_payload
          $--> Android.text_utils_is_empty ~desc:"TextUtils.isEmpty"
        ; -"dispatch_sync" &++> ObjC.dispatch_sync
        ; +map_context_tenv PatternMatch.ObjectiveC.is_core_graphics_create_or_copy
          &--> C.custom_alloc
        ; +map_context_tenv PatternMatch.ObjectiveC.is_core_foundation_create_or_copy
          &--> C.custom_alloc
        ; +BuiltinDecl.(match_builtin malloc_no_fail) <>$ capt_exp $--> C.malloc_not_null
        ; +match_regexp_opt Config.pulse_model_alloc_pattern &--> C.custom_alloc_not_null
        ; +map_context_tenv PatternMatch.ObjectiveC.is_core_graphics_release
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFRelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +match_regexp_opt Config.pulse_model_release_pattern
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFAutorelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFBridgingRelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +BuiltinDecl.(match_builtin __objc_bridge_transfer)
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +BuiltinDecl.(match_builtin __objc_alloc_no_fail) <>$ capt_exp $--> ObjC.alloc_no_fail
        ; -"NSObject" &:: "init" <>$ capt_arg_payload $--> Misc.id_first_arg ~desc:"NSObject.init"
        ; +BuiltinDecl.(match_builtin objc_insert_key)
          <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"key insertion into collection literal"
        ; +BuiltinDecl.(match_builtin objc_insert_value)
          <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"value insertion into collection literal"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
          &:: "setObject:forKey:" <>$ any_arg $+ capt_arg_payload $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_and_value
                 ~desc:"NSMutableDictionary.setObject:forKey:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
          &:: "setObject:forKeyedSubscript:" <>$ any_arg $+ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"mutableDictionary[someKey] = value"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
          &:: "removeObjectForKey:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"NSMutableDictionary.removeObjectForKey"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
          &:: "dictionaryWithSharedKeySet:" <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"NSMutableDictionary.dictionaryWithSharedKeySet"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableDictionary")
          &:: "objectForKey:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.read_from_collection ~desc:"NSMutableDictionary.objectForKey"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
          &:: "addObject:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSMutableArray.addObject:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
          &:: "insertObject:atIndex:" <>$ any_arg $+ capt_arg_payload $+ any_arg
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSMutableArray.insertObject:atIndex:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
          &:: "replaceObjectAtIndex:withObject:" <>$ any_arg $+ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSMutableArray.replaceObjectAtIndex:withObject:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableSet")
          &:: "addObject:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSMutableSet.addObject:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableSet")
          &:: "removeObject:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSMutableSet.removeObject:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
          &:: "removeObjectsAtIndexes:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"NSMutableArray.removeObjectsAtIndexes:"
        ; ( +map_context_tenv (PatternMatch.ObjectiveC.implements "NSMutableArray")
          &:: "replaceObjectsAtIndexes:withObjects:" <>$ any_arg $+ capt_arg_payload
          $+ capt_arg_payload
          $--> fun k v ->
          ObjC.insertion_into_collection_key_and_value v k
            ~desc:"NSMutableArray.replaceObjectsAtIndexes:withObjects:" )
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
          &:: "dictionaryWithObject:forKey:" <>$ capt_arg_payload $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_and_value
                 ~desc:"NSDictionary.dictionaryWithObject:forKey:"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
          &:: "sharedKeySetForKeys:" <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Key
                 ~desc:"NSDictionary.sharedKeySetForKeys"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSDictionary")
          &:: "objectForKey:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.read_from_collection ~desc:"NSDictionary.objectForKey"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSSet")
          &:: "setWithObject:" <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSSet.setWithObject"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSSet")
          &:: "setByAddingObject:" <>$ any_arg $+ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSSet.setByAddingObject"
        ; +map_context_tenv (PatternMatch.ObjectiveC.implements "NSArray")
          &:: "arrayWithObject:" <>$ capt_arg_payload
          $--> ObjC.insertion_into_collection_key_or_value ~value_kind:`Value
                 ~desc:"NSArray.arrayWithObject"
        ; +match_regexp_opt Config.pulse_model_return_nonnull
          &::.*--> Misc.return_positive
                     ~desc:"modelled as returning not null due to configuration option"
        ; +match_regexp_opt Config.pulse_model_return_first_arg
          &::+ (fun _ _ -> true)
          <>$ capt_arg_payload
          $+...$--> Misc.id_first_arg
                      ~desc:"modelled as returning the first argument due to configuration option"
        ; +match_regexp_opt Config.pulse_model_skip_pattern
          &::.*++> Misc.skip "modelled as skip due to configuration option" ] )
end

let dispatch tenv proc_name args = ProcNameDispatcher.dispatch (tenv, proc_name) proc_name args
