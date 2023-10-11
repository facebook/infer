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
open PulseOperationResult.Import
open PulseModelsImport
module GenericArrayBackedCollection = PulseModelsGenericArrayBackedCollection

let string_length_access = MemoryAccess.FieldAccess PulseOperations.ModeledField.string_length

(* NOTE: The semantic models do not check overflow for now. *)
let binop_overflow_common binop (x, x_hist) (y, y_hist) res : model =
 fun {path; location} astate ->
  let bop_addr = AbstractValue.mk_fresh () in
  let<**> astate, bop_addr = PulseArithmetic.eval_binop_absval bop_addr binop x y astate in
  let hist = Hist.binop path binop x_hist y_hist in
  let<+> astate = PulseOperations.write_deref path location ~ref:res ~obj:(bop_addr, hist) astate in
  astate


let add_overflow = binop_overflow_common (PlusA None)

let mul_overflow = binop_overflow_common (Mult None)

let sub_overflow = binop_overflow_common (MinusA None)

let delete deleted_arg : model =
 fun model_data astate -> Basic.free_or_delete `Delete CppDelete deleted_arg model_data astate


(* NOTE: [new\[\]] is not yet modelled as allocating an array of objects hence why this model
   deletes only the root address *)
let delete_array deleted_arg : model =
 fun model_data astate -> Basic.free_or_delete `Delete CppDeleteArray deleted_arg model_data astate


let new_ type_name : model =
 fun model_data astate ->
  let<++> astate =
    (* Java, Hack and C++ [new] share the same builtin (note that ObjC gets its own [objc_alloc_no_fail]
       builtin for [\[Class new\]]) *)
    let proc_name = Procdesc.get_proc_name model_data.analysis_data.proc_desc in
    if
      Procname.is_java proc_name || Procname.is_csharp proc_name || Procname.is_hack proc_name
      || Procname.is_python proc_name
    then
      Basic.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new" model_data astate
    else
      (* C++ *)
      Basic.alloc_not_null ~initialize:true ~desc:"new" CppNew (Some type_name) model_data astate
  in
  astate


let constructor_dsl type_name fields : PulseModelsDSL.aval PulseModelsDSL.model_monad =
  let open PulseModelsDSL.Syntax in
  let exp =
    Exp.Sizeof
      {typ= Typ.mk_struct type_name; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
  in
  let* new_obj = lift_to_monad_and_get_result (new_ exp) in
  let* () =
    list_iter fields ~f:(fun (fieldname, obj) ->
        let field = Fieldname.make type_name fieldname in
        write_deref_field ~ref:new_obj field ~obj )
  in
  ret new_obj


(* TODO: actually allocate an array  *)
let new_array type_name : model =
 fun model_data astate ->
  let<++> astate =
    (* Java, Hack and C++ [new\[\]] share the same builtin *)
    let pdesc = Procdesc.get_proc_name model_data.analysis_data.proc_desc in
    if Procname.is_java pdesc || Procname.is_hack pdesc || Procname.is_python pdesc then
      Basic.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new[]" model_data astate
    else
      (* C++ *)
      Basic.alloc_not_null ~initialize:true ~desc:"new[]" CppNewArray (Some type_name) model_data
        astate
  in
  astate


let placement_new =
  let std_nothrow_t_matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::nothrow_t"] in
  fun actuals {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "<placement new>()" in
    ( match (List.rev actuals : _ ProcnameDispatcher.Call.FuncArg.t list) with
    | {typ= {desc= Tstruct (CppClass {name})}} :: _
      when QualifiedCppName.Match.match_qualifiers std_nothrow_t_matcher name ->
        PulseOperations.havoc_id ret_id (Hist.single_event path event) astate
    | {arg_payload= address, hist} :: _ ->
        PulseOperations.write_id ret_id (address, Hist.add_event path event hist) astate
    | _ ->
        PulseOperations.havoc_id ret_id (Hist.single_event path event) astate )
    |> Basic.ok_continue


let infer_structured_binding var {ProcnameDispatcher.Call.FuncArg.exp= arg; arg_payload} _ astate =
  let astate =
    match (var, arg) with
    | Exp.Lvar pvar, Var arg ->
        AbductiveDomain.Stack.remove_vars [Var.of_id arg] astate
        |> AbductiveDomain.Stack.add (Var.of_pvar pvar) arg_payload
    | _ ->
        L.internal_error "Unexpected arguments for c17_structured_binding: %a, %a" Exp.pp var Exp.pp
          arg ;
        astate
  in
  Basic.ok_continue astate


module AtomicInteger = struct
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
    let this =
      (AbstractValue.mk_fresh (), Hist.single_call path location "std::atomic::atomic()")
    in
    let<*> astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    let<*> astate =
      PulseOperations.write_deref path location ~ref:int_field ~obj:init_value astate
    in
    let<+> astate = PulseOperations.write_deref path location ~ref:this_address ~obj:this astate in
    astate


  let arith_bop path prepost location event ret_id bop this operand astate =
    let=* astate, int_addr, (old_int, old_hist) = load_backing_int path location this astate in
    let hist = Hist.add_event path event old_hist in
    let bop_addr = AbstractValue.mk_fresh () in
    let+* astate, bop_addr =
      PulseArithmetic.eval_binop bop_addr bop (AbstractValueOperand old_int) operand astate
    in
    let+ astate =
      PulseOperations.write_deref path location ~ref:int_addr ~obj:(bop_addr, hist) astate
    in
    let ret_int = match prepost with `Pre -> bop_addr | `Post -> old_int in
    PulseOperations.write_id ret_id (ret_int, hist) astate


  let fetch_add this (increment, _) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::fetch_add()" in
    let<++> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let fetch_sub this (increment, _) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::fetch_sub()" in
    let<++> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let operator_plus_plus_pre this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::operator++()" in
    let<++> astate =
      arith_bop path `Pre location event ret_id (PlusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_plus_plus_post this _int : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic<T>::operator++(T)" in
    let<++> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_minus_minus_pre this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::operator--()" in
    let<++> astate =
      arith_bop path `Pre location event ret_id (MinusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_minus_minus_post this _int : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic<T>::operator--(T)" in
    let<++> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let load_instr model_desc this _memory_ordering_opt : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<+> astate, _int_addr, (int, hist) = load_backing_int path location this astate in
    PulseOperations.write_id ret_id (int, Hist.add_call path location model_desc hist) astate


  let load this memory_ordering_opt = load_instr "std::atomic<T>::load()" this memory_ordering_opt

  let operator_t this memory_ordering_opt =
    load_instr "std::atomic<T>::operator_T()" this memory_ordering_opt


  let store_backing_int ({PathContext.timestamp} as path) location this_address new_value astate =
    let* astate, this =
      PulseOperations.eval_access path Read location this_address Dereference astate
    in
    let astate =
      AddressAttributes.add_one (fst this_address)
        (WrittenTo (timestamp, Trace.Immediate {location; history= ValueHistory.epoch}))
        astate
    in
    let* astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    PulseOperations.write_deref path location ~ref:int_field ~obj:new_value astate


  let store this_address (new_value, new_hist) _memory_ordering : model =
   fun {path; location} astate ->
    let<+> astate =
      store_backing_int path location this_address
        (new_value, Hist.add_call path location "std::atomic::store()" new_hist)
        astate
    in
    astate


  let exchange this_address (new_value, new_hist) _memory_ordering : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::exchange()" in
    let<*> astate, _int_addr, (old_int, old_hist) =
      load_backing_int path location this_address astate
    in
    let<+> astate =
      store_backing_int path location this_address
        (new_value, Hist.add_event path event new_hist)
        astate
    in
    PulseOperations.write_id ret_id (old_int, Hist.add_event path event old_hist) astate
end

module BasicString = struct
  let internal_string_access = MemoryAccess.FieldAccess PulseOperations.ModeledField.internal_string

  let to_internal_string path location bstring astate =
    PulseOperations.eval_access path Read location bstring internal_string_access astate


  (* constructor from constant string *)
  let constructor_from_constant ~desc (this, hist) init_hist : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate =
      PulseOperations.write_field path location
        ~ref:(this, Hist.add_event path event hist)
        PulseOperations.ModeledField.internal_string ~obj:init_hist astate
    in
    astate


  let constructor (this, hist) init_hist : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location "std::basic_string::basic_string()" in
    let<*> astate, init_data = to_internal_string path location init_hist astate in
    let<*> astate, copied_data = PulseOperations.shallow_copy path location init_data astate in
    let<+> astate =
      PulseOperations.write_field path location
        ~ref:(this, Hist.add_event path event hist)
        PulseOperations.ModeledField.internal_string ~obj:copied_data astate
    in
    astate


  let data this_hist ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (string, hist) = to_internal_string path location this_hist astate in
    PulseOperations.write_id ret_id (string, Hist.add_event path event hist) astate


  let iterator_common ((this, hist) as this_hist) ((iter, _) as iter_hist) ~desc {path; location}
      astate =
    let event = Hist.call_event path location desc in
    let* astate, backing_ptr =
      PulseOperations.eval_access path Read location iter_hist
        (FieldAccess GenericArrayBackedCollection.field) astate
    in
    let+ astate, internal_string = to_internal_string path location this_hist astate in
    let astate =
      AbductiveDomain.AddressAttributes.add_one iter
        (PropagateTaintFrom [{v= this; history= hist}])
        astate
    in
    (astate, event, backing_ptr, internal_string)


  let begin_ this_hist iter_hist ~desc : model =
   fun ({path; location} as model_data) astate ->
    let<*> astate, event, backing_ptr, (string, hist) =
      iterator_common this_hist iter_hist ~desc model_data astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:backing_ptr
        ~obj:(string, Hist.add_event path event hist)
        astate
    in
    astate


  let end_ this_hist iter_hist ~desc : model =
   fun ({path; location} as model_data) astate ->
    let<*> astate, event, backing_ptr, string_hist =
      iterator_common this_hist iter_hist ~desc model_data astate
    in
    let<*> astate, (last, hist) =
      GenericArrayBackedCollection.eval_pointer_to_last_element path location string_hist astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:backing_ptr
        ~obj:(last, Hist.add_event path event hist)
        astate
    in
    astate


  let destructor this_hist : model =
   fun {path; location} astate ->
    let call_event = Hist.call_event path location "std::basic_string::~basic_string()" in
    let<*> astate, (string_addr, string_hist) = to_internal_string path location this_hist astate in
    let string_hist = Hist.add_event path call_event string_hist in
    let<+> astate =
      PulseOperations.check_and_invalidate path
        (MemoryAccess
           {pointer= this_hist; access= internal_string_access; hist_obj_default= string_hist} )
        location CppDelete (string_addr, string_hist) astate
    in
    astate


  let empty this_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::basic_string::empty()" in
    let<*> astate, internal_string = to_internal_string path location this_hist astate in
    let<*> astate, (len_addr, hist) =
      PulseOperations.eval_access path Read location internal_string string_length_access astate
    in
    let ((ret_addr, _) as ret_hist) = (AbstractValue.mk_fresh (), Hist.add_event path event hist) in
    let astate_empty =
      let** astate = PulseArithmetic.prune_eq_zero len_addr astate in
      let++ astate = PulseArithmetic.and_eq_int ret_addr IntLit.one astate in
      PulseOperations.write_id ret_id ret_hist astate |> Basic.continue
    in
    let astate_non_empty =
      let** astate = PulseArithmetic.prune_positive len_addr astate in
      let++ astate = PulseArithmetic.and_eq_int ret_addr IntLit.zero astate in
      PulseOperations.write_id ret_id ret_hist astate |> Basic.continue
    in
    SatUnsat.to_list astate_empty @ SatUnsat.to_list astate_non_empty


  let length this_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::basic_string::length()" in
    let<*> astate, internal_string = to_internal_string path location this_hist astate in
    let<+> astate, (length, hist) =
      PulseOperations.eval_access path Read location internal_string string_length_access astate
    in
    PulseOperations.write_id ret_id (length, Hist.add_event path event hist) astate


  let address ~desc ptr_hist (idx, _) : model =
   fun ({ret= ret_id, _} as model_env) astate ->
    let astate_zero_idx =
      let++ astate = PulseArithmetic.prune_eq_zero idx astate in
      PulseOperations.write_id ret_id ptr_hist astate |> Basic.continue
    in
    let astate_non_zero_idx =
      let<**> astate = PulseArithmetic.prune_ne_zero idx astate in
      Basic.nondet ~desc model_env astate
    in
    SatUnsat.to_list astate_zero_idx @ astate_non_zero_idx
end

module Function = struct
  let operator_call ProcnameDispatcher.Call.FuncArg.{arg_payload= lambda_ptr_hist; typ} actuals :
      model =
   fun { path
       ; analysis_data= {analyze_dependency; tenv; proc_desc}
       ; location
       ; ret= (ret_id, _) as ret } astate ->
    let<*> astate, (lambda, _) =
      PulseOperations.eval_access path Read location lambda_ptr_hist Dereference astate
    in
    let<*> astate = PulseOperations.Closures.check_captured_addresses path location lambda astate in
    match AddressAttributes.get_closure_proc_name lambda astate with
    | None ->
        (* we don't know what proc name this lambda resolves to *)
        let desc = "std::function::operator()" in
        let hist = Hist.single_event path (Hist.call_event path location desc) in
        let astate = PulseOperations.havoc_id ret_id hist astate in
        let astate =
          let unknown_effect = Attribute.UnknownEffect (Model desc, hist) in
          List.fold actuals ~init:astate
            ~f:(fun acc ProcnameDispatcher.Call.FuncArg.{arg_payload= actual, _} ->
              AddressAttributes.add_one actual unknown_effect acc )
        in
        [Ok (ContinueProgram astate)]
    | Some callee_proc_name ->
        let actuals =
          (lambda_ptr_hist, typ)
          :: List.map actuals ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
                 (arg_payload, typ) )
        in
        PulseCallOperations.call tenv path ~caller_proc_desc:proc_desc ~analyze_dependency location
          callee_proc_name ~ret ~actuals ~formals_opt:None ~call_kind:`ResolvedProcname astate
        |> fst3


  let assign dest ProcnameDispatcher.Call.FuncArg.{arg_payload= src; typ= src_typ} ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    if PulseArithmetic.is_known_zero astate (fst src) then
      let empty_target = AbstractValue.mk_fresh () in
      let<+> astate =
        PulseOperations.write_deref path location ~ref:dest
          ~obj:(empty_target, Hist.single_event path event)
          astate
      in
      PulseOperations.havoc_id ret_id (Hist.single_event path event) astate
    else
      match src_typ.Typ.desc with
      | Tptr (_, (Pk_lvalue_reference | Pk_rvalue_reference)) ->
          Basic.shallow_copy path location event ret_id dest src astate
      | _ ->
          Basic.shallow_copy_value path location event ret_id dest src astate
end

module Std = struct
  let make_move_iterator vector : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<+> astate, (backing_array, _) =
      PulseOperations.eval_deref_access path NoAccess location vector
        (FieldAccess GenericArrayBackedCollection.field) astate
    in
    let astate = AddressAttributes.add_one backing_array StdMoved astate in
    PulseOperations.write_id ret_id
      (fst vector, Hist.add_call path location "std::make_move_iterator" (snd vector))
      astate
end

module Vector = struct
  let reallocate_internal_array path trace vector vector_f location astate =
    let* astate, array_address =
      GenericArrayBackedCollection.eval path NoAccess location vector astate
    in
    PulseOperations.invalidate_array_elements path location (StdVector vector_f) array_address
      astate
    >>| PulseOperations.invalidate_deref_access path location (StdVector vector_f) vector
          GenericArrayBackedCollection.access
    >>= PulseOperations.havoc_deref_field path location vector GenericArrayBackedCollection.field
          trace


  let shallow_copy_init_list path location this init_list ~desc astate =
    let event = Hist.call_event path location desc in
    let* astate, init_copy = PulseOperations.shallow_copy path location init_list astate in
    PulseOperations.write_deref_field path location ~ref:this GenericArrayBackedCollection.field
      ~obj:(fst init_copy, Hist.add_event path event (snd init_copy))
      astate


  let init_list_constructor this init_list ~desc : model =
   fun {path; location} astate ->
    (* missing a more precise model for std::initializer_list *)
    let<**> astate =
      GenericArrayBackedCollection.assign_size_constant path location this ~constant:IntLit.zero
        ~desc astate
    in
    let<+> astate = shallow_copy_init_list path location this init_list ~desc astate in
    astate


  let init_copy_constructor this init_vector ~desc : model =
   fun {path; location} astate ->
    let<*> astate, init_list =
      PulseOperations.eval_deref_access path Read location init_vector
        (FieldAccess GenericArrayBackedCollection.field) astate
    in
    let<*> astate, other_size =
      GenericArrayBackedCollection.to_internal_size_deref path Read location init_vector astate
    in
    let<*> astate =
      PulseOperations.write_deref_field path location ~ref:this
        GenericArrayBackedCollection.size_field ~obj:other_size astate
    in
    let<+> astate = shallow_copy_init_list path location this init_list ~desc astate in
    astate


  let invalidate_references vector_f vector : model =
   fun {path; location} astate ->
    let event =
      Hist.call_event path location
        (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
    in
    let<+> astate =
      reallocate_internal_array path (Hist.single_event path event) vector vector_f location astate
    in
    astate


  let invalidate_references_with_ret vector_f vector : model =
   fun ({ret= ret_id, _} as model_data) astate ->
    PulseOperations.write_id ret_id vector astate
    |> invalidate_references vector_f vector model_data


  let at ~desc vector index : model =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (addr, hist) =
      GenericArrayBackedCollection.element path location vector (fst index) astate
    in
    PulseOperations.write_id (fst ret) (addr, Hist.add_event path event hist) astate


  let vector_begin vector iter : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location "std::vector::begin()" in
    let pointer_hist = Hist.add_event path event (snd iter) in
    let pointer_val = (AbstractValue.mk_fresh (), pointer_hist) in
    let index_zero = AbstractValue.mk_fresh () in
    let<**> astate = PulseArithmetic.and_eq_int index_zero IntLit.zero astate in
    let<*> astate, (arr_addr, _arr_hist) =
      GenericArrayBackedCollection.eval path Read location vector astate
    in
    let<*> astate, elem_at_zero =
      GenericArrayBackedCollection.eval_element path location (arr_addr, pointer_hist) index_zero
        astate
    in
    let<+> astate =
      PulseOperations.write_deref_field path location ~ref:iter GenericArrayBackedCollection.field
        ~obj:(arr_addr, pointer_hist) astate
      >>= PulseOperations.write_field path location ~ref:iter
            GenericArrayBackedCollection.Iterator.internal_pointer ~obj:pointer_val
      >>= PulseOperations.write_deref path location ~ref:pointer_val ~obj:elem_at_zero
    in
    astate


  let vector_end vector iter : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location "std::vector::end()" in
    let<*> astate, (arr_addr, _) =
      GenericArrayBackedCollection.eval path Read location vector astate
    in
    let<*> astate, (pointer_addr, _) =
      GenericArrayBackedCollection.eval_pointer_to_last_element path location vector astate
    in
    let pointer_hist = Hist.add_event path event (snd iter) in
    let pointer_val = (pointer_addr, pointer_hist) in
    let<*> astate =
      PulseOperations.write_deref_field path location ~ref:iter GenericArrayBackedCollection.field
        ~obj:(arr_addr, pointer_hist) astate
    in
    let<+> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollection.Iterator.internal_pointer ~obj:pointer_val astate
    in
    astate


  let reserve vector : model =
   fun {path; location} astate ->
    let hist = Hist.single_call path location "std::vector::reserve()" in
    let<+> astate =
      reallocate_internal_array path hist vector Reserve location astate
      >>| AddressAttributes.std_vector_reserve (fst vector)
    in
    astate


  let pop_back vector ~desc : model =
   fun {path; location} astate ->
    let<++> astate = GenericArrayBackedCollection.decrease_size path location vector ~desc astate in
    astate


  let push_back_common vector ~vector_f ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<**> astate = GenericArrayBackedCollection.increase_size path location vector ~desc astate in
    let<+> astate =
      let hist = Hist.single_call path location desc in
      if AddressAttributes.is_std_vector_reserved (fst vector) astate then
        (* assume that any call to [push_back] is ok after one called [reserve] on the same vector
           (a perfect analysis would also make sure we don't exceed the reserved size) *)
        Ok astate
      else
        match vector_f with
        | None ->
            Ok astate
        | Some vector_f ->
            (* simulate a re-allocation of the underlying array every time an element is added *)
            reallocate_internal_array path hist vector vector_f location astate
    in
    PulseOperations.write_id ret_id
      (fst vector, Hist.add_call path location desc (snd vector))
      astate


  let push_back_cpp vector ~vector_f ~desc = push_back_common vector ~vector_f:(Some vector_f) ~desc

  let push_back vector ~desc = push_back_common vector ~vector_f:None ~desc
end

module GenericMapCollection = struct
  let pair_field = Fieldname.make PulseOperations.pulse_model_type "__infer_map_pair"

  let pair_access = MemoryAccess.FieldAccess pair_field

  let pair_type key_t value_t =
    Typ.CppClass
      { name= QualifiedCppName.of_qual_string "std::pair"
      ; template_spec_info=
          Template {mangled= None; args= [TType (Typ.set_to_const key_t); TType value_t]}
      ; is_union= false }


  let pair_first_field key_t value_t = Fieldname.make (pair_type key_t value_t) "first"

  let pair_second_field key_t value_t = Fieldname.make (pair_type key_t value_t) "second"

  let extract_key_and_value_types (map : 'a ProcnameDispatcher.Call.FuncArg.t) =
    match map.typ.desc with
    | Typ.Tptr
        ( { desc=
              Tstruct
                (CppClass {template_spec_info= Template {args= TType key_t :: TType value_t :: _}})
          }
        , _ ) ->
        (key_t, value_t)
    | _ ->
        (* This can never happen, as we already know from using capt_arg_of_typ that map
           is of some map type, hence it should have a first and second template arguments
           mapping to key and value types respectively. *)
        assert false


  let reset_backing_fields ({ProcnameDispatcher.Call.FuncArg.arg_payload} as map) path location desc
      astate =
    let hist = Hist.single_call path location desc in
    let first = (AbstractValue.mk_fresh (), hist) in
    let second = (AbstractValue.mk_fresh (), hist) in
    let pair = (AbstractValue.mk_fresh (), hist) in
    let key_t, value_t = extract_key_and_value_types map in
    let<+> astate =
      PulseOperations.write_field path location ~ref:pair (pair_first_field key_t value_t)
        ~obj:first astate
      >>= PulseOperations.write_field path location ~ref:pair (pair_second_field key_t value_t)
            ~obj:second
      >>= PulseOperations.write_field path location ~ref:arg_payload pair_field ~obj:pair
    in
    astate


  let constructor map_t classname map : model =
   fun {path; location} astate ->
    let desc = Format.asprintf "%a::%s" Invalidation.pp_map_type map_t classname in
    reset_backing_fields map path location desc astate


  let invalidate_references map_t map_f ({ProcnameDispatcher.Call.FuncArg.arg_payload} as map) :
      model =
   fun {path; location} astate ->
    let key_t, value_t = extract_key_and_value_types map in
    let desc =
      Format.asprintf "%a::%a" Invalidation.pp_map_type map_t Invalidation.pp_map_function map_f
    in
    let cause = Invalidation.CppMap (map_t, map_f) in
    let<*> astate, pair =
      PulseOperations.eval_access path NoAccess location arg_payload pair_access astate
    in
    let astate =
      PulseOperations.invalidate_access path location cause arg_payload pair_access astate
    in
    let astate =
      PulseOperations.invalidate_access path location cause pair
        (MemoryAccess.FieldAccess (pair_first_field key_t value_t))
        astate
    in
    let astate =
      PulseOperations.invalidate_access path location cause pair
        (MemoryAccess.FieldAccess (pair_second_field key_t value_t))
        astate
    in
    reset_backing_fields map path location desc astate


  let at map_t ({ProcnameDispatcher.Call.FuncArg.arg_payload} as map) : model =
   fun {path; location; ret} astate ->
    let key_t, value_t = extract_key_and_value_types map in
    let event =
      Hist.call_event path location (Format.asprintf "%a::at" Invalidation.pp_map_type map_t)
    in
    let<*> astate, pair =
      PulseOperations.eval_access path Read location arg_payload pair_access astate
    in
    let<+> astate, (addr, hist) =
      PulseOperations.eval_access path Read location pair
        (MemoryAccess.FieldAccess (pair_second_field key_t value_t))
        astate
    in
    PulseOperations.write_id (fst ret) (addr, Hist.add_event path event hist) astate


  let find desc arg_payload it : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, (addr, hist) =
      PulseOperations.eval_access path Read location arg_payload pair_access astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:it
        ~obj:(addr, Hist.add_event path event hist)
        astate
    in
    astate


  let iterator_star desc it : model =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (addr, hist) =
      PulseOperations.eval_access path Read location it Dereference astate
    in
    PulseOperations.write_id (fst ret) (addr, Hist.add_event path event hist) astate
end

let get_cpp_matchers config ~model =
  let open ProcnameDispatcher.Call in
  let cpp_separator_regex = Str.regexp_string "::" in
  List.filter_map
    ~f:(fun m ->
      match Str.split cpp_separator_regex m with
      | [] ->
          None
      | first :: rest ->
          Some (List.fold rest ~f:( &:: ) ~init:(-first) &--> model m) )
    config


let abort_matchers : matcher list =
  get_cpp_matchers ~model:(fun _ -> Basic.early_exit) Config.pulse_model_abort


module Pair = struct
  let make_pair type1 type2 value1 value2 (return_param, _) : model =
    let make_pair_field =
      Fieldname.make
        (Typ.CppClass
           { name= QualifiedCppName.of_qual_string "std::pair"
           ; template_spec_info= Template {mangled= None; args= [TType type1; TType type2]}
           ; is_union= false } )
    in
    let first = make_pair_field "first" in
    let second = make_pair_field "second" in
    let desc = "std::make_pair()" in
    fun {path; location} astate ->
      let hist = Hist.single_call path location desc in
      let<**> astate, value1 = Basic.deep_copy path location ~value:value1 ~desc astate in
      let<**> astate, value2 = Basic.deep_copy path location ~value:value2 ~desc astate in
      let<*> astate =
        PulseOperations.write_field path location ~ref:(return_param, hist) first ~obj:value1 astate
      in
      let<+> astate =
        PulseOperations.write_field path location ~ref:(return_param, hist) second ~obj:value2
          astate
      in
      astate
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ +BuiltinDecl.(match_builtin __delete) <>$ capt_arg $--> delete
  ; +BuiltinDecl.(match_builtin __delete_array) <>$ capt_arg $--> delete_array ]


let map_matchers =
  let open ProcnameDispatcher.Call in
  let folly_matchers =
    List.concat_map
      [ ("F14ValueMap", Invalidation.FollyF14Value)
      ; ("F14VectorMap", Invalidation.FollyF14Vector)
      ; ("F14FastMap", Invalidation.FollyF14Fast) ]
      ~f:(fun (map_s, map_t) ->
        [ -"folly" <>:: map_s &:: map_s
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.constructor map_t map_s
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "operator="
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.invalidate_references map_t OperatorEqual
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "clear"
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $--> GenericMapCollection.invalidate_references map_t Clear
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "rehash"
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.invalidate_references map_t Rehash
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "reserve"
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.invalidate_references map_t Reserve
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "at"
          <>$ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.at map_t
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "find"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ any_arg $+ capt_arg_payload
          $--> GenericMapCollection.find (Format.asprintf "folly::%s::find" map_s)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "begin"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload
          $--> GenericMapCollection.find (Format.asprintf "folly::%s::begin" map_s)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "cbegin"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload
          $--> GenericMapCollection.find (Format.asprintf "folly::%s::cbegin" map_s) ] )
  in
  let folly_iterator_matchers =
    List.concat_map ["ValueContainerIterator"; "VectorContainerIterator"] ~f:(fun it ->
        [ -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator->" <>$ capt_arg_payload
          $--> GenericMapCollection.iterator_star
                 (Format.asprintf "folly::f14::detail::%s::operator->" it)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator*" <>$ capt_arg_payload
          $--> GenericMapCollection.iterator_star
                 (Format.asprintf "folly::f14::detail::%s::operator*" it) ] )
  in
  folly_matchers @ folly_iterator_matchers


let simple_matchers =
  let char_ptr_typ = Typ.mk (Tptr (Typ.mk (Tint IChar), Pk_pointer)) in
  let open ProcnameDispatcher.Call in
  map_matchers
  @ [ +BuiltinDecl.(match_builtin __builtin_add_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> add_overflow
    ; +BuiltinDecl.(match_builtin __builtin_mul_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> mul_overflow
    ; +BuiltinDecl.(match_builtin __builtin_sub_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> sub_overflow
    ; +BuiltinDecl.(match_builtin __infer_skip) &--> Basic.skip
    ; +BuiltinDecl.(match_builtin __infer_structured_binding)
      <>$ capt_exp $+ capt_arg $--> infer_structured_binding
    ; +BuiltinDecl.(match_builtin __new) <>$ capt_exp $--> new_
    ; +BuiltinDecl.(match_builtin __new_array) <>$ capt_exp $--> new_array
    ; +BuiltinDecl.(match_builtin __placement_new) &++> placement_new
    ; -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload
      $+ capt_arg_payload_of_prim_typ char_ptr_typ
      $--> BasicString.constructor_from_constant ~desc:"std::basic_string::basic_string()"
    ; -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.constructor
    ; -"std" &:: "basic_string" &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.begin_ ~desc:"std::basic_string::begin()"
    ; -"std" &:: "basic_string" &:: "end" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.end_ ~desc:"std::basic_string::end()"
    ; -"std" &:: "basic_string" &:: "data" <>$ capt_arg_payload
      $--> BasicString.data ~desc:"std::basic_string::data()"
    ; -"std" &:: "basic_string" &:: "empty" <>$ capt_arg_payload $--> BasicString.empty
    ; -"std" &:: "basic_string" &:: "length" <>$ capt_arg_payload $--> BasicString.length
    ; -"std" &:: "basic_string" &:: "substr" &--> Basic.nondet ~desc:"std::basic_string::substr"
    ; -"std" &:: "basic_string" &:: "size" &--> Basic.nondet ~desc:"std::basic_string::size"
    ; -"std" &:: "basic_string" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.address ~desc:"std::basic_string::operator[]"
    ; -"std" &:: "basic_string" &:: "~basic_string" <>$ capt_arg_payload $--> BasicString.destructor
    ; -"std" &:: "basic_string_view" &:: "basic_string_view" $ capt_arg_payload
      $+ capt_arg_payload_of_prim_typ char_ptr_typ
      $--> BasicString.constructor_from_constant ~desc:"std::basic_string_view::basic_string_view()"
    ; -"std" &:: "basic_string_view" &:: "data" <>$ capt_arg_payload
      $--> BasicString.data ~desc:"std::basic_string_view::data()"
    ; -"std" &:: "function" &:: "function" $ capt_arg_payload $+ capt_arg
      $--> Function.assign ~desc:"std::function::function"
    ; -"std" &:: "function" &:: "operator()" $ capt_arg $++$--> Function.operator_call
    ; -"std" &:: "function" &:: "operator=" $ capt_arg_payload $+ capt_arg
      $--> Function.assign ~desc:"std::function::operator="
    ; -"std" &:: "atomic" &:: "atomic" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.constructor
    ; -"std" &:: "__atomic_base" &:: "fetch_add" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.fetch_add
    ; -"std" &:: "__atomic_base" &:: "fetch_sub" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.fetch_sub
    ; -"std" &:: "__atomic_base" &:: "exchange" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.exchange
    ; -"std" &:: "__atomic_base" &:: "load" <>$ capt_arg_payload $+? capt_arg_payload
      $--> AtomicInteger.load
    ; -"std" &:: "__atomic_base" &:: "store" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.store
    ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload
      $--> AtomicInteger.operator_plus_plus_pre
    ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.operator_plus_plus_post
    ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload
      $--> AtomicInteger.operator_minus_minus_pre
    ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.operator_minus_minus_post
    ; -"std" &:: "__atomic_base"
      &::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
      <>$ capt_arg_payload $+? capt_arg_payload $--> AtomicInteger.operator_t
    ; -"std" &:: "make_move_iterator" $ capt_arg_payload $+...$--> Std.make_move_iterator
    ; -"std" &:: "make_pair" < capt_typ &+ capt_typ >$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> Pair.make_pair
    ; -"std" &:: "vector" &:: "vector" $ capt_arg_payload
      $--> GenericArrayBackedCollection.default_constructor ~desc:"std::vector::vector()"
    ; -"std" &:: "vector" &:: "vector" <>$ capt_arg_payload
      $+ capt_arg_payload_of_typ (-"std" &:: "initializer_list")
      $+...$--> Vector.init_list_constructor ~desc:"std::vector::vector()"
    ; -"std" &:: "vector" &:: "vector" <>$ capt_arg_payload
      $+ capt_arg_payload_of_typ (-"std" &:: "vector")
      $+...$--> Vector.init_copy_constructor ~desc:"std::vector::vector()"
    ; -"std" &:: "vector" &:: "assign" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references Assign
    ; -"std" &:: "vector" &:: "at" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.at ~desc:"std::vector::at()"
    ; -"std" &:: "vector" &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.vector_begin
    ; -"std" &:: "vector" &:: "end" <>$ capt_arg_payload $+ capt_arg_payload $--> Vector.vector_end
    ; -"std" &:: "vector" &:: "clear" <>$ capt_arg_payload $--> Vector.invalidate_references Clear
    ; -"std" &:: "vector" &:: "emplace" $ capt_arg_payload
      $+...$--> Vector.invalidate_references Emplace
    ; -"std" &:: "vector" &:: "emplace_back" $ capt_arg_payload
      $+...$--> Vector.push_back_cpp ~vector_f:EmplaceBack ~desc:"std::vector::emplace_back()"
    ; -"std" &:: "vector" &:: "insert" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references Insert
    ; -"std" &:: "vector" &:: "operator=" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references_with_ret Assign
    ; -"std" &:: "vector" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.at ~desc:"std::vector::at()"
    ; -"std" &:: "vector" &:: "shrink_to_fit" <>$ capt_arg_payload
      $--> Vector.invalidate_references ShrinkToFit
    ; -"std" &:: "vector" &:: "push_back" <>$ capt_arg_payload
      $+...$--> Vector.push_back_cpp ~vector_f:PushBack ~desc:"std::vector::push_back()"
    ; -"std" &:: "vector" &:: "pop_back" <>$ capt_arg_payload
      $+...$--> Vector.pop_back ~desc:"std::vector::pop_back()"
    ; -"std" &:: "vector" &:: "empty" <>$ capt_arg_payload
      $--> GenericArrayBackedCollection.empty ~desc:"std::vector::is_empty()"
    ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> Vector.reserve
    ; -"std" &:: "vector" &:: "size" $ capt_arg_payload
      $--> GenericArrayBackedCollection.size ~desc:"std::vector::size()"
    ; -"std" &:: "distance" &--> Basic.nondet ~desc:"std::distance"
    ; -"std" &:: "integral_constant" < any_typ &+ capt_int
      >::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
      <>--> Basic.return_int ~desc:"std::integral_constant"
    ; (* consider that all fbstrings are small strings to avoid false positives due to manual
         ref-counting *)
      -"folly" &:: "fbstring_core" &:: "category"
      &--> Basic.return_int Int64.zero ~desc:"folly::fbstring_core::category"
    ; -"folly" &:: "DelayedDestruction" &:: "destroy"
      &++> Basic.unknown_call "folly::DelayedDestruction::destroy is modelled as skip"
    ; -"folly" &:: "SocketAddress" &:: "~SocketAddress"
      &++> Basic.unknown_call "folly::SocketAddress's destructor is modelled as skip" ]


let matchers =
  matchers
  @ List.map simple_matchers
      ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
