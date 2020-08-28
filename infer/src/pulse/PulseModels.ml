(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open IResult.Let_syntax
open PulseBasicInterface
open PulseDomainInterface

type arg_payload = AbstractValue.t * ValueHistory.t

type model =
     PulseSummary.t InterproceduralAnalysis.t
  -> callee_procname:Procname.t
  -> Location.t
  -> ret:Ident.t * Typ.t
  -> AbductiveDomain.t
  -> ExecutionDomain.t list PulseOperations.access_result

let cpp_model_namespace = QualifiedCppName.of_list ["__infer_pulse_model"]

module Misc = struct
  let shallow_copy location event ret_id dest_pointer_hist src_pointer_hist astate =
    let* astate, obj = PulseOperations.eval_access location src_pointer_hist Dereference astate in
    let* astate, obj_copy = PulseOperations.shallow_copy location obj astate in
    let+ astate =
      PulseOperations.write_deref location ~ref:dest_pointer_hist
        ~obj:(fst obj_copy, event :: snd obj_copy)
        astate
    in
    PulseOperations.havoc_id ret_id [event] astate


  let shallow_copy_model model_desc dest_pointer_hist src_pointer_hist : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    let+ astate = shallow_copy location event ret_id dest_pointer_hist src_pointer_hist astate in
    [ExecutionDomain.ContinueProgram astate]


  let early_exit : model =
   fun _ ~callee_procname:_ _ ~ret:_ astate -> Ok [ExecutionDomain.ExitProgram astate]


  let return_int : Int64.t -> model =
   fun i64 _ ~callee_procname:_ _location ~ret:(ret_id, _) astate ->
    let i = IntLit.of_int64 i64 in
    let ret_addr = AbstractValue.Constants.get_int i in
    let astate = PulseArithmetic.and_eq_int ret_addr i astate in
    PulseOperations.write_id ret_id (ret_addr, []) astate |> PulseOperations.ok_continue


  let return_positive ~desc : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, [event]) in
    PulseOperations.write_id ret_id ret_value astate
    |> PulseArithmetic.and_positive ret_addr
    |> PulseOperations.ok_continue


  let return_unknown_size : model =
   fun _ ~callee_procname:_ _location ~ret:(ret_id, _) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let astate = PulseArithmetic.and_nonnegative ret_addr astate in
    PulseOperations.write_id ret_id (ret_addr, []) astate |> PulseOperations.ok_continue


  let skip : model = fun _ ~callee_procname:_ _ ~ret:_ astate -> PulseOperations.ok_continue astate

  let nondet ~fn_name : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model fn_name; location; in_call= []} in
    PulseOperations.havoc_id ret_id [event] astate |> PulseOperations.ok_continue


  let id_first_arg arg_access_hist : model =
   fun _ ~callee_procname:_ _ ~ret astate ->
    PulseOperations.write_id (fst ret) arg_access_hist astate |> PulseOperations.ok_continue


  let free_or_delete operation deleted_access : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    (* NOTE: we could introduce a case-split explicitly on =0 vs ≠0 but instead only act on what we
       currently know about the value. This is purely to avoid contributing to path explosion. *)
    (* freeing 0 is a no-op *)
    if PulseArithmetic.is_known_zero astate (fst deleted_access) then
      PulseOperations.ok_continue astate
    else
      let astate = PulseArithmetic.and_positive (fst deleted_access) astate in
      let invalidation =
        match operation with `Free -> Invalidation.CFree | `Delete -> Invalidation.CppDelete
      in
      let+ astate = PulseOperations.invalidate location invalidation deleted_access astate in
      [ExecutionDomain.ContinueProgram astate]
end

module C = struct
  let free deleted_access : model = Misc.free_or_delete `Free deleted_access

  let malloc _ : model =
   fun _ ~callee_procname location ~ret:(ret_id, _) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value =
      (ret_addr, [ValueHistory.Allocation {f= Model (Procname.to_string callee_procname); location}])
    in
    let astate = PulseOperations.write_id ret_id ret_value astate in
    let astate_alloc =
      PulseArithmetic.and_positive ret_addr astate
      |> PulseOperations.allocate callee_procname location ret_value
    in
    let+ astate_null =
      PulseArithmetic.and_eq_int ret_addr IntLit.zero astate
      |> PulseOperations.invalidate location (ConstantDereference IntLit.zero) ret_value
    in
    [ExecutionDomain.ContinueProgram astate_alloc; ExecutionDomain.ContinueProgram astate_null]


  let malloc_not_null _ : model =
   fun _ ~callee_procname location ~ret:(ret_id, _) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value =
      (ret_addr, [ValueHistory.Allocation {f= Model (Procname.to_string callee_procname); location}])
    in
    let astate = PulseOperations.write_id ret_id ret_value astate in
    PulseOperations.allocate callee_procname location ret_value astate
    |> PulseArithmetic.and_positive ret_addr
    |> PulseOperations.ok_continue
end

module ObjCCoreFoundation = struct
  let cf_bridging_release access : model =
   fun _ ~callee_procname:_ _ ~ret:(ret_id, _) astate ->
    let astate = PulseOperations.write_id ret_id access astate in
    PulseOperations.remove_allocation_attr (fst access) astate |> PulseOperations.ok_continue
end

module ObjC = struct
  let alloc_not_null arg : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let dynamic_type =
      match arg with
      | Exp.Sizeof {typ= {desc= Tstruct name}} ->
          name
      | _ ->
          Logging.die InternalError "Expected arg should be a sizeof expression"
    in
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value = (ret_addr, [ValueHistory.Allocation {f= Model "alloc"; location}]) in
    let astate = PulseOperations.write_id ret_id ret_value astate in
    PulseOperations.add_dynamic_type dynamic_type ret_addr astate
    |> PulseArithmetic.and_positive ret_addr
    |> PulseOperations.ok_continue
end

module FollyOptional = struct
  let internal_value = Fieldname.make (Typ.CStruct cpp_model_namespace) "backing_value"

  let internal_value_access = HilExp.Access.FieldAccess internal_value

  let to_internal_value location optional astate =
    PulseOperations.eval_access location optional internal_value_access astate


  let to_internal_value_deref location optional astate =
    let* astate, pointer = to_internal_value location optional astate in
    PulseOperations.eval_access location pointer Dereference astate


  let write_value location this ~value ~desc astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let* astate, value_field = to_internal_value location this astate in
    let value_hist = (fst value, event :: snd value) in
    let+ astate = PulseOperations.write_deref location ~ref:value_field ~obj:value_hist astate in
    (astate, value_hist)


  let assign_value_fresh location this ~desc astate =
    write_value location this ~value:(AbstractValue.mk_fresh (), []) ~desc astate


  let assign_none this ~desc : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let* astate, value = assign_value_fresh location this ~desc astate in
    let astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
    let+ astate = PulseOperations.invalidate location Invalidation.OptionalEmpty value astate in
    [ExecutionDomain.ContinueProgram astate]


  let assign_value this init ~desc : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let* astate, value = to_internal_value_deref location init astate in
    let+ astate, _ = write_value location this ~value ~desc astate in
    [ExecutionDomain.ContinueProgram astate]


  let emplace optional : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let+ astate, _ =
      assign_value_fresh location optional ~desc:"folly::Optional::emplace()" astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let value optional : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "folly::Optional::value()"; location; in_call= []} in
    let* astate, (value_addr, value_hist) = to_internal_value_deref location optional astate in
    PulseOperations.write_id ret_id (value_addr, event :: value_hist) astate
    |> PulseOperations.ok_continue


  let has_value optional : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let ret_addr = AbstractValue.mk_fresh () in
    let ret_value =
      (ret_addr, [ValueHistory.Allocation {f= Model "folly::Optional::has_value()"; location}])
    in
    let+ astate, (value_addr, _) = to_internal_value_deref location optional astate in
    let astate = PulseOperations.write_id ret_id ret_value astate in
    let astate_non_empty = PulseArithmetic.and_positive value_addr astate in
    let astate_true = PulseArithmetic.and_positive ret_addr astate_non_empty in
    let astate_empty = PulseArithmetic.and_eq_int value_addr IntLit.zero astate in
    let astate_false = PulseArithmetic.and_eq_int ret_addr IntLit.zero astate_empty in
    [ExecutionDomain.ContinueProgram astate_false; ExecutionDomain.ContinueProgram astate_true]
end

module Cplusplus = struct
  let delete deleted_access : model = Misc.free_or_delete `Delete deleted_access

  let placement_new actuals : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "<placement new>()"; location; in_call= []} in
    ( match List.rev actuals with
    | ProcnameDispatcher.Call.FuncArg.{arg_payload= address, hist} :: _ ->
        PulseOperations.write_id ret_id (address, event :: hist) astate
    | _ ->
        PulseOperations.havoc_id ret_id [event] astate )
    |> PulseOperations.ok_continue
end

module StdAtomicInteger = struct
  let internal_int =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "atomic"]))
      "__infer_model_backing_int"


  let load_backing_int location this astate =
    let* astate, obj = PulseOperations.eval_access location this Dereference astate in
    let* astate, int_addr =
      PulseOperations.eval_access location obj (FieldAccess internal_int) astate
    in
    let+ astate, int_val = PulseOperations.eval_access location int_addr Dereference astate in
    (astate, int_addr, int_val)


  let constructor this_address init_value : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::atomic()"; location; in_call= []} in
    let this = (AbstractValue.mk_fresh (), [event]) in
    let* astate, int_field =
      PulseOperations.eval_access location this (FieldAccess internal_int) astate
    in
    let* astate = PulseOperations.write_deref location ~ref:int_field ~obj:init_value astate in
    let+ astate = PulseOperations.write_deref location ~ref:this_address ~obj:this astate in
    [ExecutionDomain.ContinueProgram astate]


  let arith_bop prepost location event ret_id bop this operand astate =
    let* astate, int_addr, (old_int, hist) = load_backing_int location this astate in
    let bop_addr = AbstractValue.mk_fresh () in
    let astate =
      PulseArithmetic.eval_binop bop_addr bop (AbstractValueOperand old_int) operand astate
    in
    let+ astate =
      PulseOperations.write_deref location ~ref:int_addr ~obj:(bop_addr, event :: hist) astate
    in
    let ret_int = match prepost with `Pre -> bop_addr | `Post -> old_int in
    PulseOperations.write_id ret_id (ret_int, event :: hist) astate


  let fetch_add this (increment, _) _memory_ordering : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::fetch_add()"; location; in_call= []} in
    let+ astate =
      arith_bop `Post location event ret_id (PlusA None) this (AbstractValueOperand increment)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let fetch_sub this (increment, _) _memory_ordering : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::fetch_sub()"; location; in_call= []} in
    let+ astate =
      arith_bop `Post location event ret_id (MinusA None) this (AbstractValueOperand increment)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let operator_plus_plus_pre this : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::operator++()"; location; in_call= []} in
    let+ astate =
      arith_bop `Pre location event ret_id (PlusA None) this (LiteralOperand IntLit.one) astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let operator_plus_plus_post this _int : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event =
      ValueHistory.Call {f= Model "std::atomic<T>::operator++(T)"; location; in_call= []}
    in
    let+ astate =
      arith_bop `Post location event ret_id (PlusA None) this (LiteralOperand IntLit.one) astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let operator_minus_minus_pre this : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::operator--()"; location; in_call= []} in
    let+ astate =
      arith_bop `Pre location event ret_id (MinusA None) this (LiteralOperand IntLit.one) astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let operator_minus_minus_post this _int : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event =
      ValueHistory.Call {f= Model "std::atomic<T>::operator--(T)"; location; in_call= []}
    in
    let+ astate =
      arith_bop `Post location event ret_id (MinusA None) this (LiteralOperand IntLit.one) astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let load_instr model_desc this _memory_ordering_opt : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model model_desc; location; in_call= []} in
    let+ astate, _int_addr, (int, hist) = load_backing_int location this astate in
    let astate = PulseOperations.write_id ret_id (int, event :: hist) astate in
    [ExecutionDomain.ContinueProgram astate]


  let load = load_instr "std::atomic<T>::load()"

  let operator_t = load_instr "std::atomic<T>::operator_T()"

  let store_backing_int location this_address new_value astate =
    let* astate, this = PulseOperations.eval_access location this_address Dereference astate in
    let* astate, int_field =
      PulseOperations.eval_access location this (FieldAccess internal_int) astate
    in
    PulseOperations.write_deref location ~ref:int_field ~obj:new_value astate


  let store this_address (new_value, new_hist) _memory_ordering : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::store()"; location; in_call= []} in
    let+ astate = store_backing_int location this_address (new_value, event :: new_hist) astate in
    [ExecutionDomain.ContinueProgram astate]


  let exchange this_address (new_value, new_hist) _memory_ordering : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::atomic::exchange()"; location; in_call= []} in
    let* astate, _int_addr, (old_int, old_hist) = load_backing_int location this_address astate in
    let+ astate = store_backing_int location this_address (new_value, event :: new_hist) astate in
    let astate = PulseOperations.write_id ret_id (old_int, event :: old_hist) astate in
    [ExecutionDomain.ContinueProgram astate]
end

module JavaObject = struct
  (* naively modeled as shallow copy. *)
  let clone src_pointer_hist : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "Object.clone"; location; in_call= []} in
    let* astate, obj = PulseOperations.eval_access location src_pointer_hist Dereference astate in
    let+ astate, obj_copy = PulseOperations.shallow_copy location obj astate in
    let astate = PulseOperations.write_id ret_id (fst obj_copy, event :: snd obj_copy) astate in
    [ExecutionDomain.ContinueProgram astate]
end

module StdBasicString = struct
  let internal_string =
    Fieldname.make
      (Typ.CStruct (QualifiedCppName.of_list ["std"; "basic_string"]))
      "__infer_model_backing_string"


  let internal_string_access = HilExp.Access.FieldAccess internal_string

  let to_internal_string location bstring astate =
    PulseOperations.eval_access location bstring internal_string_access astate


  let data this_hist : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::basic_string::data()"; location; in_call= []} in
    let* astate, string_addr_hist = to_internal_string location this_hist astate in
    let+ astate, (string, hist) =
      PulseOperations.eval_access location string_addr_hist Dereference astate
    in
    let astate = PulseOperations.write_id ret_id (string, event :: hist) astate in
    [ExecutionDomain.ContinueProgram astate]


  let destructor this_hist : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let model = CallEvent.Model "std::basic_string::~basic_string()" in
    let call_event = ValueHistory.Call {f= model; location; in_call= []} in
    let* astate, (string_addr, string_hist) = to_internal_string location this_hist astate in
    let string_addr_hist = (string_addr, call_event :: string_hist) in
    let* astate =
      PulseOperations.invalidate_access location CppDelete string_addr_hist Dereference astate
    in
    let+ astate = PulseOperations.invalidate location CppDelete string_addr_hist astate in
    [ExecutionDomain.ContinueProgram astate]
end

module StdFunction = struct
  let operator_call ProcnameDispatcher.Call.FuncArg.{arg_payload= lambda_ptr_hist; typ} actuals :
      model =
   fun {analyze_dependency} ~callee_procname:_ location ~ret astate ->
    let havoc_ret (ret_id, _) astate =
      let event = ValueHistory.Call {f= Model "std::function::operator()"; location; in_call= []} in
      [PulseOperations.havoc_id ret_id [event] astate]
    in
    let* astate, (lambda, _) =
      PulseOperations.eval_access location lambda_ptr_hist Dereference astate
    in
    let* astate = PulseOperations.Closures.check_captured_addresses location lambda astate in
    match AddressAttributes.get_closure_proc_name lambda astate with
    | None ->
        (* we don't know what proc name this lambda resolves to *)
        Ok (havoc_ret ret astate |> List.map ~f:ExecutionDomain.continue)
    | Some callee_proc_name ->
        let actuals =
          (lambda_ptr_hist, typ)
          :: List.map actuals ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
                 (arg_payload, typ) )
        in
        PulseOperations.call
          ~callee_data:(analyze_dependency callee_proc_name)
          location callee_proc_name ~ret ~actuals ~formals_opt:None astate


  let operator_equal dest src : model =
   fun _ ~callee_procname:_ location ~ret:(ret_id, _) astate ->
    let event = ValueHistory.Call {f= Model "std::function::operator="; location; in_call= []} in
    let+ astate =
      if PulseArithmetic.is_known_zero astate (fst src) then
        let empty_target = AbstractValue.mk_fresh () in
        let+ astate =
          PulseOperations.write_deref location ~ref:dest ~obj:(empty_target, [event]) astate
        in
        PulseOperations.havoc_id ret_id [event] astate
      else Misc.shallow_copy location event ret_id dest src astate
    in
    [ExecutionDomain.ContinueProgram astate]
end

module GenericArrayBackedCollection = struct
  let field = Fieldname.make (Typ.CStruct cpp_model_namespace) "backing_array"

  let last_field = Fieldname.make (Typ.CStruct cpp_model_namespace) "past_the_end"

  let access = HilExp.Access.FieldAccess field

  let eval location collection astate =
    PulseOperations.eval_access location collection access astate


  let eval_element location internal_array index astate =
    PulseOperations.eval_access location internal_array (ArrayAccess (Typ.void, index)) astate


  let element location collection index astate =
    let* astate, internal_array = eval location collection astate in
    eval_element location internal_array index astate


  let eval_pointer_to_last_element location collection astate =
    let+ astate, pointer =
      PulseOperations.eval_access location collection (FieldAccess last_field) astate
    in
    let astate = AddressAttributes.mark_as_end_of_collection (fst pointer) astate in
    (astate, pointer)
end

module GenericArrayBackedCollectionIterator = struct
  let internal_pointer = Fieldname.make (Typ.CStruct cpp_model_namespace) "backing_pointer"

  let internal_pointer_access = HilExp.Access.FieldAccess internal_pointer

  let to_internal_pointer location iterator astate =
    PulseOperations.eval_access location iterator internal_pointer_access astate


  let to_internal_pointer_deref location iterator astate =
    let* astate, pointer = to_internal_pointer location iterator astate in
    let+ astate, index = PulseOperations.eval_access location pointer Dereference astate in
    (astate, pointer, index)


  let to_elem_pointed_by_iterator ?(step = None) location iterator astate =
    let* astate, pointer = to_internal_pointer location iterator astate in
    let* astate, index = PulseOperations.eval_access location pointer Dereference astate in
    (* Check if not end iterator *)
    let is_minus_minus = match step with Some `MinusMinus -> true | _ -> false in
    let* astate =
      if AddressAttributes.is_end_of_collection (fst pointer) astate && not is_minus_minus then
        let invalidation_trace = Trace.Immediate {location; history= []} in
        let access_trace = Trace.Immediate {location; history= snd pointer} in
        Error
          ( Diagnostic.AccessToInvalidAddress
              {invalidation= EndIterator; invalidation_trace; access_trace}
          , astate )
      else Ok astate
    in
    (* We do not want to create internal array if iterator pointer has an invalid value *)
    let* astate = PulseOperations.check_addr_access location index astate in
    let+ astate, elem = GenericArrayBackedCollection.element location iterator (fst index) astate in
    (astate, pointer, elem)


  let construct location event ~init ~ref astate =
    let* astate, (arr_addr, arr_hist) = GenericArrayBackedCollection.eval location init astate in
    let* astate =
      PulseOperations.write_field location ~ref GenericArrayBackedCollection.field
        ~obj:(arr_addr, event :: arr_hist)
        astate
    in
    let* astate, (p_addr, p_hist) = to_internal_pointer location init astate in
    PulseOperations.write_field location ~ref internal_pointer ~obj:(p_addr, event :: p_hist) astate


  let constructor ~desc this init : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    construct location event ~init ~ref:this astate >>| ExecutionDomain.continue >>| List.return


  let operator_compare comparison ~desc iter_lhs iter_rhs _ ~callee_procname:_ location
      ~ret:(ret_id, _) astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let* astate, _, (index_lhs, _) = to_internal_pointer_deref location iter_lhs astate in
    let+ astate, _, (index_rhs, _) = to_internal_pointer_deref location iter_rhs astate in
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
      |> PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand index_lhs)
           (AbstractValueOperand index_rhs)
    in
    let astate_notequal =
      PulseArithmetic.and_eq_int ret_val ret_val_notequal astate
      |> PulseArithmetic.prune_binop ~negated:false Ne (AbstractValueOperand index_lhs)
           (AbstractValueOperand index_rhs)
    in
    [ExecutionDomain.ContinueProgram astate_equal; ExecutionDomain.ContinueProgram astate_notequal]


  let operator_star ~desc iter _ ~callee_procname:_ location ~ret astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let+ astate, pointer, (elem, _) = to_elem_pointed_by_iterator location iter astate in
    let astate = PulseOperations.write_id (fst ret) (elem, event :: snd pointer) astate in
    [ExecutionDomain.ContinueProgram astate]


  let operator_step step ~desc iter _ ~callee_procname:_ location ~ret:_ astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let index_new = AbstractValue.mk_fresh () in
    let* astate, pointer, _ = to_elem_pointed_by_iterator ~step:(Some step) location iter astate in
    PulseOperations.write_deref location ~ref:pointer ~obj:(index_new, event :: snd pointer) astate
    >>| ExecutionDomain.continue >>| List.return
end

module JavaIterator = struct
  let constructor ~desc init : model =
   fun _ ~callee_procname:_ location ~ret astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let ref = (AbstractValue.mk_fresh (), [event]) in
    let+ astate = GenericArrayBackedCollectionIterator.construct location event ~init ~ref astate in
    let astate = PulseOperations.write_id (fst ret) ref astate in
    [ExecutionDomain.ContinueProgram astate]


  (* {curr -> v_c} is modified to {curr -> v_fresh} and returns array[v_c] *)
  let next ~desc iter _ ~callee_procname:_ location ~ret astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let new_index = AbstractValue.mk_fresh () in
    let* astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollectionIterator.to_internal_pointer location iter astate
    in
    let* astate, (curr_elem_val, curr_elem_hist) =
      GenericArrayBackedCollection.element location iter curr_index astate
    in
    let+ astate =
      PulseOperations.write_field location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer
        ~obj:(new_index, event :: curr_index_hist)
        astate
    in
    let astate =
      PulseOperations.write_id (fst ret) (curr_elem_val, event :: curr_elem_hist) astate
    in
    [ExecutionDomain.ContinueProgram astate]


  (* {curr -> v_c } is modified to {curr -> v_fresh} and writes to array[v_c] *)
  let remove ~desc iter _ ~callee_procname:_ location ~ret:_ astate =
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let new_index = AbstractValue.mk_fresh () in
    let* astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollectionIterator.to_internal_pointer location iter astate
    in
    let* astate =
      PulseOperations.write_field location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer
        ~obj:(new_index, event :: curr_index_hist)
        astate
    in
    let new_elem = AbstractValue.mk_fresh () in
    let* astate, arr = GenericArrayBackedCollection.eval location iter astate in
    let+ astate =
      PulseOperations.write_arr_index location ~ref:arr ~index:curr_index
        ~obj:(new_elem, event :: curr_index_hist)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]
end

module StdVector = struct
  let reallocate_internal_array trace vector vector_f location astate =
    let* astate, array_address = GenericArrayBackedCollection.eval location vector astate in
    PulseOperations.invalidate_array_elements location (StdVector vector_f) array_address astate
    >>= PulseOperations.invalidate_access location (StdVector vector_f) vector
          GenericArrayBackedCollection.access
    >>= PulseOperations.havoc_field location vector GenericArrayBackedCollection.field trace


  let init_list_constructor this init_list _ ~callee_procname:_ location ~ret:_ astate =
    let event = ValueHistory.Call {f= Model "std::vector::vector()"; location; in_call= []} in
    let* astate, init_copy = PulseOperations.shallow_copy location init_list astate in
    let+ astate =
      PulseOperations.write_field location ~ref:this GenericArrayBackedCollection.field
        ~obj:(fst init_copy, event :: snd init_copy)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let invalidate_references vector_f vector : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let crumb =
      ValueHistory.Call
        { f= Model (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
        ; location
        ; in_call= [] }
    in
    reallocate_internal_array [crumb] vector vector_f location astate
    >>| ExecutionDomain.continue >>| List.return


  let at ~desc vector index : model =
   fun _ ~callee_procname:_ location ~ret astate ->
    let event = ValueHistory.Call {f= Model desc; location; in_call= []} in
    let+ astate, (addr, hist) =
      GenericArrayBackedCollection.element location vector (fst index) astate
    in
    let astate = PulseOperations.write_id (fst ret) (addr, event :: hist) astate in
    [ExecutionDomain.ContinueProgram astate]


  let vector_begin vector iter : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "std::vector::begin()"; location; in_call= []} in
    let pointer_hist = event :: snd iter in
    let pointer_val = (AbstractValue.mk_fresh (), pointer_hist) in
    let index_zero = AbstractValue.mk_fresh () in
    let astate = PulseArithmetic.and_eq_int index_zero IntLit.zero astate in
    let* astate, ((arr_addr, _) as arr) =
      GenericArrayBackedCollection.eval location vector astate
    in
    let* astate, _ = GenericArrayBackedCollection.eval_element location arr index_zero astate in
    PulseOperations.write_field location ~ref:iter GenericArrayBackedCollection.field
      ~obj:(arr_addr, pointer_hist) astate
    >>= PulseOperations.write_field location ~ref:iter
          GenericArrayBackedCollectionIterator.internal_pointer ~obj:pointer_val
    >>= PulseOperations.write_deref location ~ref:pointer_val ~obj:(index_zero, pointer_hist)
    >>| ExecutionDomain.continue >>| List.return


  let vector_end vector iter : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "std::vector::end()"; location; in_call= []} in
    let* astate, (arr_addr, _) = GenericArrayBackedCollection.eval location vector astate in
    let* astate, (pointer_addr, _) =
      GenericArrayBackedCollection.eval_pointer_to_last_element location vector astate
    in
    let pointer_hist = event :: snd iter in
    let pointer_val = (pointer_addr, pointer_hist) in
    let* astate =
      PulseOperations.write_field location ~ref:iter GenericArrayBackedCollection.field
        ~obj:(arr_addr, pointer_hist) astate
    in
    let+ astate =
      PulseOperations.write_field location ~ref:iter
        GenericArrayBackedCollectionIterator.internal_pointer ~obj:pointer_val astate
    in
    [ExecutionDomain.ContinueProgram astate]


  let reserve vector : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::reserve()"; location; in_call= []} in
    reallocate_internal_array [crumb] vector Reserve location astate
    >>| AddressAttributes.std_vector_reserve (fst vector)
    >>| ExecutionDomain.continue >>| List.return


  let push_back vector : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let crumb = ValueHistory.Call {f= Model "std::vector::push_back()"; location; in_call= []} in
    if AddressAttributes.is_std_vector_reserved (fst vector) astate then
      (* assume that any call to [push_back] is ok after one called [reserve] on the same vector
         (a perfect analysis would also make sure we don't exceed the reserved size) *)
      PulseOperations.ok_continue astate
    else
      (* simulate a re-allocation of the underlying array every time an element is added *)
      reallocate_internal_array [crumb] vector PushBack location astate
      >>| ExecutionDomain.continue >>| List.return
end

module JavaCollection = struct
  (* modifies arr[index]-> old_elem to arr[index]-> new_elem and returns old_elem *)
  let set coll (index, _) (new_elem, new_elem_hist) : model =
   fun _ ~callee_procname:_ location ~ret astate ->
    let event = ValueHistory.Call {f= Model "Collection.set"; location; in_call= []} in
    let* astate, arr = GenericArrayBackedCollection.eval location coll astate in
    let* astate, (old_addr, old_hist) =
      GenericArrayBackedCollection.element location coll index astate
    in
    let+ astate =
      PulseOperations.write_arr_index location ~ref:arr ~index
        ~obj:(new_elem, event :: new_elem_hist)
        astate
    in
    let astate = PulseOperations.write_id (fst ret) (old_addr, event :: old_hist) astate in
    [ExecutionDomain.ContinueProgram astate]


  (* writes to arr[index]-> new_elem   *)
  let add_at coll (index, _) (new_elem, new_elem_hist) : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "Collection.add"; location; in_call= []} in
    let* astate, arr = GenericArrayBackedCollection.eval location coll astate in
    let+ astate =
      PulseOperations.write_arr_index location ~ref:arr ~index
        ~obj:(new_elem, event :: new_elem_hist)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]


  (* writes to arr[index]-> new_elem where index is a fresh address *)
  let add coll new_elem : model =
    let index = AbstractValue.mk_fresh () in
    add_at coll (index, []) new_elem


  (* writes to arr[index]-> fresh_elem   *)
  let remove_at coll (index, _) : model =
   fun _ ~callee_procname:_ location ~ret:_ astate ->
    let event = ValueHistory.Call {f= Model "Collection.add"; location; in_call= []} in
    let* astate, arr = GenericArrayBackedCollection.eval location coll astate in
    let fresh_elem = AbstractValue.mk_fresh () in
    let+ astate =
      PulseOperations.write_arr_index location ~ref:arr ~index
        ~obj:(fresh_elem, event :: snd arr)
        astate
    in
    [ExecutionDomain.ContinueProgram astate]


  (* writes to arr[index]-> fresh_elem where index is fresh *)
  let remove coll : model =
    let index = AbstractValue.mk_fresh () in
    remove_at coll (index, [])
end

module StringSet = Caml.Set.Make (String)

module ProcNameDispatcher = struct
  let dispatch : (Tenv.t, model, arg_payload) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let match_builtin builtin _ s = String.equal s (Procname.get_method builtin) in
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
      transfer_ownership_namespace_matchers @ transfer_ownership_name_matchers
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
    let return_nonnull_matchers =
      get_cpp_matchers
        ~model:(fun m -> Misc.return_positive ~desc:m)
        Config.pulse_model_return_nonnull
    in
    make_dispatcher
      ( transfer_ownership_matchers @ abort_matchers @ return_nonnull_matchers
      @ [ +match_builtin BuiltinDecl.free <>$ capt_arg_payload $--> C.free
        ; +match_builtin BuiltinDecl.malloc <>$ capt_arg_payload $--> C.malloc
        ; +match_builtin BuiltinDecl.__delete <>$ capt_arg_payload $--> Cplusplus.delete
        ; +match_builtin BuiltinDecl.__new &--> Misc.return_positive ~desc:"new"
        ; +match_builtin BuiltinDecl.__placement_new &++> Cplusplus.placement_new
        ; +match_builtin BuiltinDecl.objc_cpp_throw <>--> Misc.early_exit
        ; +match_builtin BuiltinDecl.__cast <>$ capt_arg_payload $+...$--> Misc.id_first_arg
        ; +match_builtin BuiltinDecl.abort <>--> Misc.early_exit
        ; +match_builtin BuiltinDecl.exit <>--> Misc.early_exit
        ; +match_builtin BuiltinDecl.__infer_initializer_list
          <>$ capt_arg_payload $+...$--> Misc.id_first_arg
        ; +PatternMatch.Java.implements_lang "System" &:: "exit" <>--> Misc.early_exit
        ; +match_builtin BuiltinDecl.__get_array_length <>--> Misc.return_unknown_size
        ; (* consider that all fbstrings are small strings to avoid false positives due to manual
             ref-counting *)
          -"folly" &:: "fbstring_core" &:: "category" &--> Misc.return_int Int64.zero
        ; -"folly" &:: "DelayedDestruction" &:: "destroy" &--> Misc.skip
        ; -"folly" &:: "SocketAddress" &:: "~SocketAddress" &--> Misc.skip
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
          $+ any_arg_of_typ (-"folly" &:: "None")
          $--> FollyOptional.assign_none ~desc:"folly::Optional::Optional(=None)"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
          $--> FollyOptional.assign_none ~desc:"folly::Optional::Optional()"
        ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> FollyOptional.assign_value ~desc:"folly::Optional::Optional(arg)"
        ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload
          $+ any_arg_of_typ (-"folly" &:: "None")
          $--> FollyOptional.assign_none ~desc:"folly::Optional::assign(=None)"
        ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload $+ capt_arg_payload
          $+...$--> FollyOptional.assign_value ~desc:"folly::Optional::assign()"
        ; -"folly" &:: "Optional" &:: "emplace<>" $ capt_arg_payload $+...$--> FollyOptional.emplace
        ; -"folly" &:: "Optional" &:: "emplace" $ capt_arg_payload $+...$--> FollyOptional.emplace
        ; -"folly" &:: "Optional" &:: "has_value" <>$ capt_arg_payload
          $+...$--> FollyOptional.has_value
        ; -"folly" &:: "Optional" &:: "reset" <>$ capt_arg_payload
          $+...$--> FollyOptional.assign_none ~desc:"folly::Optional::reset()"
        ; -"folly" &:: "Optional" &:: "value" <>$ capt_arg_payload $+...$--> FollyOptional.value
        ; -"std" &:: "basic_string" &:: "data" <>$ capt_arg_payload $--> StdBasicString.data
        ; -"std" &:: "basic_string" &:: "~basic_string" <>$ capt_arg_payload
          $--> StdBasicString.destructor
        ; -"std" &:: "function" &:: "operator()" $ capt_arg $++$--> StdFunction.operator_call
        ; -"std" &:: "function" &:: "operator=" $ capt_arg_payload $+ capt_arg_payload
          $--> StdFunction.operator_equal
        ; +PatternMatch.Java.implements_lang "Object"
          &:: "clone" $ capt_arg_payload $--> JavaObject.clone
        ; ( +PatternMatch.Java.implements_lang "System"
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
        ; +PatternMatch.Java.implements_collection
          &:: "add" <>$ capt_arg_payload $+ capt_arg_payload $--> JavaCollection.add
        ; +PatternMatch.Java.implements_list
          &:: "add" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
          $--> JavaCollection.add_at
        ; +PatternMatch.Java.implements_collection
          &:: "remove" <>$ capt_arg_payload $+ any_arg $--> JavaCollection.remove
        ; +PatternMatch.Java.implements_list
          &:: "remove" <>$ capt_arg_payload $+ capt_arg_payload $+ any_arg
          $--> JavaCollection.remove_at
        ; +PatternMatch.Java.implements_collection
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +PatternMatch.Java.implements_queue
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +PatternMatch.Java.implements_lang "StringBuilder"
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +PatternMatch.Java.implements_lang "StringBuilder"
          &:: "setLength" <>$ capt_arg_payload
          $+...$--> StdVector.invalidate_references ShrinkToFit
        ; +PatternMatch.Java.implements_lang "String"
          &::+ (fun _ str -> StringSet.mem str pushback_modeled)
          <>$ capt_arg_payload $+...$--> StdVector.push_back
        ; +PatternMatch.Java.implements_iterator
          &:: "remove" <>$ capt_arg_payload
          $+...$--> JavaIterator.remove ~desc:"remove"
        ; +PatternMatch.Java.implements_map &:: "put" <>$ capt_arg_payload
          $+...$--> StdVector.push_back
        ; +PatternMatch.Java.implements_map &:: "putAll" <>$ capt_arg_payload
          $+...$--> StdVector.push_back
        ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> StdVector.reserve
        ; +PatternMatch.Java.implements_collection
          &:: "get" <>$ capt_arg_payload $+ capt_arg_payload
          $--> StdVector.at ~desc:"Collection.get()"
        ; +PatternMatch.Java.implements_list
          &:: "set" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
          $--> JavaCollection.set
        ; +PatternMatch.Java.implements_iterator
          &:: "hasNext"
          &--> Misc.nondet ~fn_name:"Iterator.hasNext()"
        ; +PatternMatch.Java.implements_enumeration
          &:: "hasMoreElements"
          &--> Misc.nondet ~fn_name:"Enumeration.hasMoreElements()"
        ; +PatternMatch.Java.implements_lang "Object"
          &:: "equals"
          &--> Misc.nondet ~fn_name:"Object.equals"
        ; +PatternMatch.Java.implements_lang "Iterable"
          &:: "iterator" <>$ capt_arg_payload
          $+...$--> JavaIterator.constructor ~desc:"Iterable.iterator"
        ; +PatternMatch.Java.implements_iterator
          &:: "next" <>$ capt_arg_payload
          $!--> JavaIterator.next ~desc:"Iterator.next()"
        ; ( +PatternMatch.Java.implements_enumeration
          &:: "nextElement" <>$ capt_arg_payload
          $!--> fun x ->
          StdVector.at ~desc:"Enumeration.nextElement" x (AbstractValue.mk_fresh (), []) )
        ; +PatternMatch.ObjectiveC.is_core_graphics_create_or_copy &++> C.malloc
        ; +PatternMatch.ObjectiveC.is_core_foundation_create_or_copy &++> C.malloc
        ; +match_builtin BuiltinDecl.malloc_no_fail <>$ capt_arg_payload $--> C.malloc_not_null
        ; +PatternMatch.ObjectiveC.is_modelled_as_alloc &++> C.malloc_not_null
        ; +PatternMatch.ObjectiveC.is_core_graphics_release
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFRelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +PatternMatch.ObjectiveC.is_modelled_as_release
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFAutorelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; -"CFBridgingRelease" <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +match_builtin BuiltinDecl.__objc_bridge_transfer
          <>$ capt_arg_payload $--> ObjCCoreFoundation.cf_bridging_release
        ; +match_builtin BuiltinDecl.__objc_alloc_no_fail <>$ capt_exp $--> ObjC.alloc_not_null
        ; -"NSObject" &:: "init" <>$ capt_arg_payload $--> Misc.id_first_arg ] )
end

let dispatch tenv proc_name args = ProcNameDispatcher.dispatch tenv proc_name args
