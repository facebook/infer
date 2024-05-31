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
module FuncArg = ProcnameDispatcher.Call.FuncArg

(* NOTE: The semantic models do not check overflow for now. *)
let binop_overflow_common binop (x, x_hist) (y, y_hist) res : model_no_non_disj =
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


let new_ type_exp =
  let open PulseModelsDSL.Syntax in
  start_model
  @@ let* ret_val = new_ type_exp in
     assign_ret ret_val


(* TODO: actually allocate an array  *)
let new_array type_name : model_no_non_disj =
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
    ( match (List.rev actuals : _ FuncArg.t list) with
    | {typ= {desc= Tstruct (CppClass {name})}} :: _
      when QualifiedCppName.Match.match_qualifiers std_nothrow_t_matcher name ->
        PulseOperations.havoc_id ret_id (Hist.single_event path event) astate
    | {arg_payload= address, hist} :: _ ->
        PulseOperations.write_id ret_id (address, Hist.add_event path event hist) astate
    | _ ->
        PulseOperations.havoc_id ret_id (Hist.single_event path event) astate )
    |> Basic.ok_continue


let infer_structured_binding var {FuncArg.exp= arg; arg_payload} _ astate =
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


  let constructor this_address init_value : model_no_non_disj =
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


  let fetch_add this (increment, _) _memory_ordering : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::fetch_add()" in
    let<++> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let fetch_sub this (increment, _) _memory_ordering : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::fetch_sub()" in
    let<++> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (AbstractValueOperand increment)
        astate
    in
    astate


  let operator_plus_plus_pre this : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::operator++()" in
    let<++> astate =
      arith_bop path `Pre location event ret_id (PlusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_plus_plus_post this _int : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic<T>::operator++(T)" in
    let<++> astate =
      arith_bop path `Post location event ret_id (PlusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_minus_minus_pre this : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic::operator--()" in
    let<++> astate =
      arith_bop path `Pre location event ret_id (MinusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let operator_minus_minus_post this _int : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "std::atomic<T>::operator--(T)" in
    let<++> astate =
      arith_bop path `Post location event ret_id (MinusA None) this (ConstOperand (Cint IntLit.one))
        astate
    in
    astate


  let load_instr model_desc this _memory_ordering_opt : model_no_non_disj =
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


  let store this_address (new_value, new_hist) _memory_ordering : model_no_non_disj =
   fun {path; location} astate ->
    let<+> astate =
      store_backing_int path location this_address
        (new_value, Hist.add_call path location "std::atomic::store()" new_hist)
        astate
    in
    astate


  let exchange this_address (new_value, new_hist) _memory_ordering : model_no_non_disj =
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
  let constructor_from_constant_dsl ~desc (this, hist) init_hist : unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    let* {path; location} = get_data in
    let event = Hist.call_event path location desc in
    let this_hist = (this, Hist.add_event path event hist) in
    let* () = write_deref_field ~ref:this_hist ~obj:init_hist ModeledField.internal_string in
    let* s_opt = as_constant_string init_hist in
    let* len =
      exec_operation (fun astate ->
          match s_opt with
          | Some s ->
              let astate, len =
                String.length s |> IntLit.of_int |> PulseArithmetic.absval_of_int astate
              in
              (len, astate)
          | None ->
              (AbstractValue.mk_fresh_restricted (), astate) )
    in
    write_field ~ref:this_hist ~obj:(len, snd init_hist) ModeledField.string_length


  (** constructor from constant string *)
  let constructor_from_constant ~desc this_hist init_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model @@ constructor_from_constant_dsl ~desc this_hist init_hist


  let default_constructor ~desc this_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@
    let* init_hist = eval_const_string "" in
    constructor_from_constant_dsl ~desc this_hist init_hist


  let copy_constructor ~desc (this, hist) src_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let event = Hist.call_event path location desc in
       let this_hist = (this, Hist.add_event path event hist) in
       let* src_string =
         eval_deref_access Read src_hist (FieldAccess ModeledField.internal_string)
       in
       let* src_length = eval_access Read src_hist (FieldAccess ModeledField.string_length) in
       let* () = write_deref_field ~ref:this_hist ~obj:src_string ModeledField.internal_string in
       write_field ~ref:this_hist ~obj:src_length ModeledField.string_length


  let constructor_rev ~desc init this : model = copy_constructor ~desc this init

  let data this_hist ~desc : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let event = Hist.call_event path location desc in
       let* this_string, this_string_hist =
         eval_access Read this_hist (FieldAccess ModeledField.internal_string)
       in
       assign_ret (this_string, Hist.add_event path event this_string_hist)


  let iterator_common ((this, hist) as this_hist) ((iter, _) as iter_hist) ~desc :
      (ValueHistory.event * PulseModelsDSL.aval * PulseModelsDSL.aval) PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    let* {path; location} = get_data in
    let event = Hist.call_event path location desc in
    let* backing_ptr =
      eval_access Read iter_hist (FieldAccess GenericArrayBackedCollection.field)
    in
    let* internal_string =
      eval_deref_access Read this_hist (FieldAccess ModeledField.internal_string)
    in
    let* () =
      exec_command
        (AbductiveDomain.AddressAttributes.add_one iter
           (PropagateTaintFrom (InternalModel, [{v= this; history= hist}])) )
    in
    ret (event, backing_ptr, internal_string)


  let begin_ this_hist iter_hist ~desc : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* event, backing_ptr, (string, hist) = iterator_common this_hist iter_hist ~desc in
       let* {path} = get_data in
       write_deref ~ref:backing_ptr ~obj:(string, Hist.add_event path event hist)


  let end_ this_hist iter_hist ~desc : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let* event, backing_ptr, string_hist = iterator_common this_hist iter_hist ~desc in
       let* last, hist =
         exec_partial_operation (fun astate ->
             Sat
               (GenericArrayBackedCollection.eval_pointer_to_last_element path location string_hist
                  astate ) )
       in
       write_deref ~ref:backing_ptr ~obj:(last, Hist.add_event path event hist)


  let destructor this_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let call_event = Hist.call_event path location "std::basic_string::~basic_string()" in
       let* string_addr, string_hist =
         eval_access Read this_hist (FieldAccess ModeledField.internal_string)
       in
       let string_hist = Hist.add_event path call_event string_hist in
       exec_partial_command (fun astate ->
           Sat
             (PulseOperations.check_and_invalidate path
                (MemoryAccess
                   { pointer= this_hist
                   ; access= FieldAccess ModeledField.internal_string
                   ; hist_obj_default= string_hist } )
                location CppDelete (string_addr, string_hist) astate ) )


  let empty this_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let event = Hist.call_event path location "std::basic_string::empty()" in
       let* len = eval_access Read this_hist (FieldAccess ModeledField.string_length) in
       let* string = eval_deref_access Read this_hist (FieldAccess ModeledField.internal_string) in
       let ret_hist = Hist.add_event path event (snd string) in
       disjuncts
         [ (let* () = prune_eq_zero len in
            let* () = prune_eq_string string "" in
            let* ret_v = aval_of_int ret_hist 1 in
            assign_ret ret_v )
         ; (let* () = prune_positive len in
            let* () = prune_ne_string string "" in
            let* ret_v = aval_of_int ret_hist 0 in
            assign_ret ret_v ) ]


  let length this_hist : model =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* {path; location} = get_data in
       let event = Hist.call_event path location "std::basic_string::length()" in
       let* length, hist = eval_access Read this_hist (FieldAccess ModeledField.string_length) in
       assign_ret (length, Hist.add_event path event hist)


  let address ~desc ptr_hist (idx, _) : model_no_non_disj =
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
  let operator_call FuncArg.{arg_payload= lambda_ptr_hist; typ} actuals : model =
   fun {path; analysis_data; location; ret= (ret_id, _) as ret} astate non_disj ->
    let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
    let<*> astate, (lambda, _) =
      PulseOperations.eval_access path Read location lambda_ptr_hist Dereference astate
    in
    let<*> astate = PulseOperations.Closures.check_captured_addresses path location lambda astate in
    let callee_proc_name_opt =
      match PulseArithmetic.get_dynamic_type lambda astate with
      | Some {typ= {desc= Typ.Tstruct name}} -> (
        match Tenv.lookup analysis_data.tenv name with
        | Some tstruct ->
            List.find ~f:(fun m -> Procname.is_cpp_lambda m) tstruct.Struct.methods
        | None ->
            None )
      | _ ->
          None
    in
    match callee_proc_name_opt with
    | Some callee_proc_name ->
        let actuals =
          (lambda_ptr_hist, typ)
          :: List.map actuals ~f:(fun FuncArg.{arg_payload; typ} -> (arg_payload, typ))
        in
        let astate, non_disj, _, _ =
          PulseCallOperations.call analysis_data path location callee_proc_name ~ret ~actuals
            ~formals_opt:None ~call_kind:`ResolvedProcname astate non_disj
        in
        (astate, non_disj)
    | _ ->
        (* we don't know what proc name this lambda resolves to *)
        let desc = "std::function::operator()" in
        let hist = Hist.single_event path (Hist.call_event path location desc) in
        let astate = PulseOperations.havoc_id ret_id hist astate in
        let astate = AbductiveDomain.add_need_dynamic_type_specialization lambda astate in
        let astate =
          let unknown_effect = Attribute.UnknownEffect (Model desc, hist) in
          List.fold actuals ~init:astate ~f:(fun acc FuncArg.{arg_payload= actual, _} ->
              AddressAttributes.add_one actual unknown_effect acc )
        in
        ([Ok (ContinueProgram astate)], non_disj)


  let assign dest FuncArg.{arg_payload= src; typ= src_typ} ~desc : model_no_non_disj =
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
  let make_move_iterator vector : model_no_non_disj =
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


  let init_list_constructor this init_list ~desc : model_no_non_disj =
   fun {path; location} astate ->
    (* missing a more precise model for std::initializer_list *)
    let<**> astate =
      GenericArrayBackedCollection.assign_size_constant path location this ~constant:IntLit.zero
        ~desc astate
    in
    let<+> astate = shallow_copy_init_list path location this init_list ~desc astate in
    astate


  let init_copy_constructor this init_vector ~desc : model_no_non_disj =
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


  let invalidate_references vector_f vector : model_no_non_disj =
   fun {path; location} astate ->
    let event =
      Hist.call_event path location
        (Format.asprintf "%a()" Invalidation.pp_std_vector_function vector_f)
    in
    let<+> astate =
      reallocate_internal_array path (Hist.single_event path event) vector vector_f location astate
    in
    astate


  let invalidate_references_with_ret vector_f vector : model_no_non_disj =
   fun ({ret= ret_id, _} as model_data) astate ->
    PulseOperations.write_id ret_id vector astate
    |> invalidate_references vector_f vector model_data


  let at ~desc vector index : model_no_non_disj =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (addr, hist) =
      GenericArrayBackedCollection.element path location vector (fst index) astate
    in
    PulseOperations.write_id (fst ret) (addr, Hist.add_event path event hist) astate


  let vector_begin vector iter : model_no_non_disj =
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


  let vector_end vector iter : model_no_non_disj =
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


  let reserve vector : model_no_non_disj =
   fun {path; location} astate ->
    let hist = Hist.single_call path location "std::vector::reserve()" in
    let<+> astate =
      reallocate_internal_array path hist vector Reserve location astate
      >>| AddressAttributes.std_vector_reserve (fst vector)
    in
    astate


  let pop_back vector ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<++> astate = GenericArrayBackedCollection.decrease_size path location vector ~desc astate in
    astate


  let push_back_common vector ~vector_f ~desc : model_no_non_disj =
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

  let pair_access = Access.FieldAccess pair_field

  let pair_type key_t value_t =
    Typ.CppClass
      { name= QualifiedCppName.of_qual_string "std::pair"
      ; template_spec_info=
          Template {mangled= None; args= [TType (Typ.set_to_const key_t); TType value_t]}
      ; is_union= false }


  let pair_first_field key_t value_t = Fieldname.make (pair_type key_t value_t) "first"

  let pair_first_access key_t value_t = Access.FieldAccess (pair_first_field key_t value_t)

  let pair_second_field key_t value_t = Fieldname.make (pair_type key_t value_t) "second"

  let pair_second_access key_t value_t = Access.FieldAccess (pair_second_field key_t value_t)

  let extract_key_and_value_types {FuncArg.typ= map_typ} =
    let rec extract_helper {Typ.desc} =
      match desc with
      | Tstruct (CppClass {template_spec_info= Template {args= TType key_t :: TType value_t :: _}})
        ->
          Some (key_t, value_t)
      | Tptr (typ, _) ->
          (* We strip [Tptr] recursively because there can be multiple wrappings of it, e.g. an
             rvalue-ref parameter can be captured by reference in lambda.  Also, the stripping
             semantics is aligned with the [ProcnameDispatcher.match_typ] definition. *)
          extract_helper typ
      | _ ->
          (* This should never happen, as we already know from using [capt_arg_of_typ] that map
             is of some map type, hence it should have a first and second template arguments
             mapping to key and value types respectively. *)
          L.internal_error "Unexpected (key, value) template type: %a" (Typ.pp_full Pp.text) map_typ ;
          None
    in
    extract_helper map_typ


  let unwrap_pointer_to_struct_type (typ : Typ.t) =
    match typ.desc with Tptr ({desc= Tstruct pointee}, _) -> pointee | _ -> assert false


  let return_value_reference_dsl ?desc ({FuncArg.arg_payload} as map) :
      unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    Option.value_map (extract_key_and_value_types map) ~default:(ret ()) ~f:(fun (key_t, value_t) ->
        let* pair = eval_access Read arg_payload pair_access in
        let* value_ref = eval_access ?desc Read pair (pair_second_access key_t value_t) in
        assign_ret value_ref )


  let return_value_reference desc map =
    let open PulseModelsDSL.Syntax in
    start_model @@ return_value_reference_dsl ~desc map


  let return_it_dsl ?desc arg_payload it : unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    let* pair = eval_access ?desc Read arg_payload pair_access in
    write_deref ~ref:it ~obj:pair


  let return_it desc arg_payload it =
    let open PulseModelsDSL.Syntax in
    start_model @@ return_it_dsl ~desc arg_payload it


  (* A number of map functions return pair<iterator, bool>. *)
  (* Make an iterator the first field of a pair and return it. *)
  let return_it_pair_first_dsl desc map_payload FuncArg.{arg_payload; typ} :
      unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    let* it = mk_fresh ~model_desc:desc () in
    let* pair = eval_access Read map_payload pair_access in
    let* () = write_deref ~ref:it ~obj:pair in
    write_field ~ref:arg_payload ~obj:it
      (Fieldname.make (unwrap_pointer_to_struct_type typ) "first")


  let reset_backing_fields_dsl ({FuncArg.arg_payload} as map) desc : unit PulseModelsDSL.model_monad
      =
    let open PulseModelsDSL.Syntax in
    Option.value_map (extract_key_and_value_types map) ~default:(ret ()) ~f:(fun (key_t, value_t) ->
        let* first = mk_fresh ~model_desc:desc () in
        let* second = mk_fresh ~model_desc:desc () in
        let* pair = mk_fresh ~model_desc:desc () in
        let* () = write_field ~ref:pair ~obj:first (pair_first_field key_t value_t) in
        let* () = write_field ~ref:pair ~obj:second (pair_second_field key_t value_t) in
        write_field ~ref:arg_payload ~obj:pair pair_field )


  let update_last_dsl arg_payload key_payload : unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    AddressAttributes.add_one (fst arg_payload) (LastLookup (fst key_payload)) |> exec_command


  let update_last arg_payload key_payload =
    let open PulseModelsDSL.Syntax in
    start_model @@ update_last_dsl arg_payload key_payload


  let check_and_update_last_dsl arg_payload key_payload : bool PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    let* last_lookup = AddressAttributes.get_last_lookup (fst arg_payload) |> exec_pure_operation in
    let* () = update_last_dsl arg_payload key_payload in
    ret (Option.value_map last_lookup ~default:false ~f:(AbstractValue.equal (fst key_payload)))


  let constructor map_t classname map =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ reset_backing_fields_dsl map
         (Format.asprintf "%a::%s" Invalidation.pp_map_type map_t classname)


  let invalidate_references_dsl map_t map_f ({FuncArg.arg_payload} as map) :
      unit PulseModelsDSL.model_monad =
    let open PulseModelsDSL.Syntax in
    Option.value_map (extract_key_and_value_types map) ~default:(ret ()) ~f:(fun (key_t, value_t) ->
        let desc =
          Format.asprintf "%a::%a" Invalidation.pp_map_type map_t Invalidation.pp_map_function map_f
        in
        let cause = Invalidation.CppMap (map_t, map_f) in
        let* pair = eval_access NoAccess arg_payload pair_access in
        let* () = invalidate_access cause arg_payload pair_access in
        let* () = invalidate_access cause pair (pair_first_access key_t value_t) in
        let* () = invalidate_access cause pair (pair_second_access key_t value_t) in
        reset_backing_fields_dsl map desc )


  let invalidate_references map_t map_f map =
    let open PulseModelsDSL.Syntax in
    start_model @@ invalidate_references_dsl map_t map_f map


  let operator_bracket map_t ({FuncArg.arg_payload} as map) key_payload =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* double_lookup = check_and_update_last_dsl arg_payload key_payload in
       let* () =
         if double_lookup then ret () else invalidate_references_dsl map_t OperatorBracket map
       in
       return_value_reference_dsl map


  let emplace_hint map_t map_f ({FuncArg.arg_payload} as map) args =
    let open PulseModelsDSL.Syntax in
    start_model
    @@
    (* We expect the last argument to be the returned iterator. *)
    (* This will only throw if SIL intermediate representation changes. *)
    let it = (List.last_exn args).FuncArg.arg_payload in
    let* () = invalidate_references_dsl map_t map_f map in
    return_it_dsl arg_payload it


  let emplace map_t map_f ({FuncArg.arg_payload} as map) args =
    let open PulseModelsDSL.Syntax in
    start_model
    @@
    let desc =
      Format.asprintf "%a::%a" Invalidation.pp_map_type map_t Invalidation.pp_map_function map_f
    in
    let last_arg = List.last_exn args in
    let* () = invalidate_references_dsl map_t map_f map in
    return_it_pair_first_dsl desc arg_payload last_arg


  let try_emplace ~hinted map_t map_f map args =
    (* The hinted version returns an iterator, other overloads a pair<iterator, bool>. *)
    if hinted then emplace_hint map_t map_f map args else emplace map_t map_f map args


  let insert ~hinted map_t map_f map return_arg = try_emplace ~hinted map_t map_f map [return_arg]

  let find map_t arg_payload key_payload it =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* () = update_last_dsl arg_payload key_payload in
       return_it_dsl
         ~desc:(Format.asprintf "%a::find" Invalidation.pp_map_type map_t)
         arg_payload it


  let swap map_t arg_payload other_payload =
    let open PulseModelsDSL.Syntax in
    start_model
    @@
    let desc = Format.asprintf "%a::swap" Invalidation.pp_map_type map_t in
    let* arg_pair = eval_access ~desc Read arg_payload pair_access in
    let* other_pair = eval_access ~desc Read other_payload pair_access in
    let* () = write_field ~ref:arg_payload ~obj:other_pair pair_field in
    write_field ~ref:other_payload ~obj:arg_pair pair_field


  let iterator_star desc it =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* pair = eval_access ~desc Read it Dereference in
       assign_ret pair


  let iterator_copy desc it other =
    let open PulseModelsDSL.Syntax in
    start_model
    @@ let* pair = eval_access ~desc Read other Dereference in
       write_deref ~ref:it ~obj:pair
end

let get_cpp_matchers =
  let cpp_separator_regex = Str.regexp_string "::" in
  fun config ~model ->
    let open ProcnameDispatcher.Call in
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
  |> List.map ~f:(ProcnameDispatcher.Call.map_matcher ~f:lift_model)


module Pair = struct
  let make_pair type1 type2 value1 value2 (return_param, _) : model_no_non_disj =
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

let folly_co_yield_co_error : model =
  let open PulseModelsDSL.Syntax in
  start_model @@ throw


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ +BuiltinDecl.(match_builtin __delete) <>$ capt_arg $--> delete
  ; +BuiltinDecl.(match_builtin __delete_array) <>$ capt_arg $--> delete_array ]


let map_matchers =
  let open ProcnameDispatcher.Call in
  let value_it_matcher = -"folly" <>:: "f14" <>:: "detail" <>:: "ValueContainerIterator" in
  let vector_it_matcher = -"folly" <>:: "f14" <>:: "detail" <>:: "VectorContainerIterator" in
  let basic_string_matchers =
    let char_ptr_typ = Typ.mk (Tptr (Typ.mk (Tint IChar), Pk_pointer)) in
    [ -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload
      $+ capt_arg_payload_of_prim_typ char_ptr_typ
      $+...$--> BasicString.constructor_from_constant ~desc:"std::basic_string::basic_string()"
    ; -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.copy_constructor ~desc:"std::basic_string::basic_string()"
    ; -"std" &:: "basic_string" &:: "basic_string" $ capt_arg_payload
      $--> BasicString.default_constructor ~desc:"std::basic_string::basic_string()"
    ; -"std" &:: "basic_string" &:: "operator_basic_string_view" $ capt_arg_payload
      $+ capt_arg_payload
      $--> BasicString.constructor_rev ~desc:"std::basic_string::operator_basic_string_view()"
    ; -"std" &:: "basic_string" &:: "data" <>$ capt_arg_payload
      $--> BasicString.data ~desc:"std::basic_string::data()"
    ; -"std" &:: "basic_string_view" &:: "basic_string_view" $ capt_arg_payload
      $+ capt_arg_payload_of_prim_typ char_ptr_typ
      $--> BasicString.constructor_from_constant ~desc:"std::basic_string_view::basic_string_view()"
    ; -"std" &:: "basic_string_view" &:: "basic_string_view" $ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.copy_constructor ~desc:"std::basic_string_view::basic_string_view()"
    ; -"std" &:: "basic_string_view" &:: "data" <>$ capt_arg_payload
      $--> BasicString.data ~desc:"std::basic_string_view::data()"
    ; -"std" &:: "basic_string" &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.begin_ ~desc:"std::basic_string::begin()"
    ; -"std" &:: "basic_string" &:: "end" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.end_ ~desc:"std::basic_string::end()"
    ; -"std" &:: "basic_string" &:: "empty" <>$ capt_arg_payload $--> BasicString.empty
    ; -"std" &:: "basic_string" &:: "length" <>$ capt_arg_payload $--> BasicString.length
    ; -"std" &:: "basic_string" &:: "~basic_string" <>$ capt_arg_payload $--> BasicString.destructor
    ]
  in
  let folly_matchers =
    List.concat_map
      [ ("F14ValueMap", Invalidation.FollyF14Value, [value_it_matcher])
      ; ("F14VectorMap", Invalidation.FollyF14Vector, [vector_it_matcher])
      ; ("F14FastMap", Invalidation.FollyF14Fast, [value_it_matcher; vector_it_matcher]) ]
      ~f:(fun (map_s, map_t, it_matchers) ->
        [ -"folly" <>:: map_s &:: map_s
          $ capt_arg_of_typ (-"folly" <>:: map_s)
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
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+...$--> GenericMapCollection.return_value_reference
                      (Format.asprintf "folly::%s::at" map_s)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "emplace_hint"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $++$--> GenericMapCollection.emplace_hint map_t EmplaceHint
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "emplace"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $++$--> GenericMapCollection.emplace map_t Emplace
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "try_emplace_token"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $++$--> GenericMapCollection.emplace map_t TryEmplaceToken
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "operator[]"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload
          $+...$--> GenericMapCollection.operator_bracket map_t
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "find"
          $ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload $+ capt_arg_payload $--> GenericMapCollection.find map_t
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "begin"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload
          $--> GenericMapCollection.return_it (Format.asprintf "folly::%s::begin" map_s)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "cbegin"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload
          $--> GenericMapCollection.return_it (Format.asprintf "folly::%s::cbegin" map_s)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "contains"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload $--> GenericMapCollection.update_last
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "count"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload $--> GenericMapCollection.update_last
        ; -"folly" <>:: map_s &:: "swap"
          <>$ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $+ capt_arg_payload_of_typ (-"folly" <>:: map_s)
          $--> GenericMapCollection.swap map_t
          (* Order matters for the next matchers in this list. *)
          (* insert_or_assign:
              1. Two arguments only: non-hinted case.
              2. Three arguments with the first being a token: non-hinted case.
              3. Else: hinted case.
          *)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert_or_assign"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg $+ any_arg $+ capt_arg
          $--> GenericMapCollection.insert ~hinted:false map_t InsertOrAssign
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert_or_assign"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg_of_typ (-"folly" <>:: "F14HashToken")
          $+ any_arg $+ any_arg $+ capt_arg
          $--> GenericMapCollection.insert ~hinted:false map_t InsertOrAssign
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert_or_assign"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg $+ any_arg $+ any_arg $+ capt_arg
          $--> GenericMapCollection.insert ~hinted:true map_t InsertOrAssign
          (* try_emplace:
              1. First argument is an iterator: hinted case.
              2. Else: non-hinted case.
          *)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "try_emplace"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg_of_typ_exists it_matchers
          $++$--> GenericMapCollection.try_emplace ~hinted:true map_t TryEmplace
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "try_emplace"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $++$--> GenericMapCollection.try_emplace ~hinted:false map_t TryEmplace
          (* insert:
              1. One argument and it's std::initializer_list: return void.
              2. Three arguments (including SIL return): return iterator.
              3. Two arguments (including SIL return matching std::pair type):
                   return pair<iterator, bool>.
              4. Else if two arguments: return void.
          *)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg_of_typ (-"std" &:: "initializer_list")
          $--> GenericMapCollection.invalidate_references map_t Insert
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg $+ any_arg $+ capt_arg
          $--> GenericMapCollection.insert ~hinted:true map_t Insert
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg
          $+ capt_arg_of_typ (-"std" &:: "pair")
          $--> GenericMapCollection.insert ~hinted:false map_t Insert
        ; -"folly" <>:: "f14" <>:: "detail" <>:: "F14BasicMap" &:: "insert"
          $ capt_arg_of_typ (-"folly" <>:: map_s)
          $+ any_arg $+ any_arg
          $--> GenericMapCollection.invalidate_references map_t Insert ] )
  in
  let folly_iterator_matchers =
    List.concat_map ["ValueContainerIterator"; "VectorContainerIterator"] ~f:(fun it ->
        [ -"folly" <>:: "f14" <>:: "detail" <>:: it &:: it <>$ capt_arg_payload $+ capt_arg_payload
          $--> GenericMapCollection.iterator_copy
                 (Format.asprintf "folly::f14::detail::%s::%s" it it)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator=" <>$ capt_arg_payload
          $+ capt_arg_payload
          $--> GenericMapCollection.iterator_copy
                 (Format.asprintf "folly::f14::detail::%s::operator=" it)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator->" <>$ capt_arg_payload
          $--> GenericMapCollection.iterator_star
                 (Format.asprintf "folly::f14::detail::%s::operator->" it)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator*" <>$ capt_arg_payload
          $--> GenericMapCollection.iterator_star
                 (Format.asprintf "folly::f14::detail::%s::operator*" it)
        ; -"folly" <>:: "f14" <>:: "detail" <>:: it &:: "operator++"
          &++> Basic.unknown_call "folly::f14::detail::it::operator++"
          |> with_non_disj ] )
  in
  let folly_concurrent_hash_map_matchers =
    (* We ignore all [folly::ConcurrentHashMap] methods as of now, because the summaries from the
       actual source code are too imprecise. *)
    [ -"folly" <>:: "ConcurrentHashMap"
      &::+ (fun _ _ -> true)
      &++> Basic.unknown_call "folly::ConcurrentHashMap" ]
    |> List.map ~f:with_non_disj
  in
  let folly_coro =
    [ -"folly" &:: "coro" &:: "detail" &:: "TaskPromise" &:: "yield_value" $ any_arg
      $+ any_arg_of_typ (-"folly" &:: "coro" &:: "co_error")
      $+ any_arg $--> folly_co_yield_co_error ]
  in
  basic_string_matchers @ folly_matchers @ folly_iterator_matchers
  @ folly_concurrent_hash_map_matchers @ folly_coro


let simple_matchers =
  let open ProcnameDispatcher.Call in
  map_matchers
  @ [ +BuiltinDecl.(match_builtin __builtin_add_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> add_overflow
      |> with_non_disj
    ; +BuiltinDecl.(match_builtin __builtin_mul_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> mul_overflow
      |> with_non_disj
    ; +BuiltinDecl.(match_builtin __builtin_sub_overflow)
      <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> sub_overflow
      |> with_non_disj
    ; +BuiltinDecl.(match_builtin __infer_skip) &--> Basic.skip |> with_non_disj
    ; +BuiltinDecl.(match_builtin __infer_structured_binding)
      <>$ capt_exp $+ capt_arg $--> infer_structured_binding |> with_non_disj
    ; +BuiltinDecl.(match_builtin __new) <>$ capt_exp $--> new_
    ; +BuiltinDecl.(match_builtin __new_array) <>$ capt_exp $--> new_array |> with_non_disj
    ; +BuiltinDecl.(match_builtin __placement_new) &++> placement_new |> with_non_disj
    ; -"std" &:: "basic_string" &:: "substr"
      &--> Basic.nondet ~desc:"std::basic_string::substr"
      |> with_non_disj
    ; -"std" &:: "basic_string" &:: "size"
      &--> Basic.nondet ~desc:"std::basic_string::size"
      |> with_non_disj
    ; -"std" &:: "basic_string" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
      $--> BasicString.address ~desc:"std::basic_string::operator[]"
      |> with_non_disj
    ; -"std" &:: "function" &:: "function" $ capt_arg_payload $+ capt_arg
      $--> Function.assign ~desc:"std::function::function"
      |> with_non_disj
    ; -"std" &:: "function" &:: "operator()" $ capt_arg $++$--> Function.operator_call
    ; -"std" &:: "function" &:: "operator=" $ capt_arg_payload $+ capt_arg
      $--> Function.assign ~desc:"std::function::operator="
      |> with_non_disj
    ; -"std" &:: "atomic" &:: "atomic" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.constructor |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "fetch_add" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.fetch_add |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "fetch_sub" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.fetch_sub |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "exchange" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.exchange |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "load" <>$ capt_arg_payload $+? capt_arg_payload
      $--> AtomicInteger.load |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "store" <>$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> AtomicInteger.store |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload
      $--> AtomicInteger.operator_plus_plus_pre |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "operator++" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.operator_plus_plus_post |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload
      $--> AtomicInteger.operator_minus_minus_pre |> with_non_disj
    ; -"std" &:: "__atomic_base" &:: "operator--" <>$ capt_arg_payload $+ capt_arg_payload
      $--> AtomicInteger.operator_minus_minus_post |> with_non_disj
    ; -"std" &:: "__atomic_base"
      &::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
      <>$ capt_arg_payload $+? capt_arg_payload $--> AtomicInteger.operator_t |> with_non_disj
    ; -"std" &:: "make_move_iterator" $ capt_arg_payload $+...$--> Std.make_move_iterator
      |> with_non_disj
    ; -"std" &:: "make_pair" < capt_typ &+ capt_typ >$ capt_arg_payload $+ capt_arg_payload
      $+ capt_arg_payload $--> Pair.make_pair |> with_non_disj
    ; -"std" &:: "vector" &:: "vector" $ capt_arg_payload
      $--> GenericArrayBackedCollection.default_constructor ~desc:"std::vector::vector()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "vector" <>$ capt_arg_payload
      $+ capt_arg_payload_of_typ (-"std" &:: "initializer_list")
      $+...$--> Vector.init_list_constructor ~desc:"std::vector::vector()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "vector" <>$ capt_arg_payload
      $+ capt_arg_payload_of_typ (-"std" &:: "vector")
      $+...$--> Vector.init_copy_constructor ~desc:"std::vector::vector()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "assign" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references Assign
      |> with_non_disj
    ; -"std" &:: "vector" &:: "at" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.at ~desc:"std::vector::at()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "begin" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.vector_begin |> with_non_disj
    ; -"std" &:: "vector" &:: "end" <>$ capt_arg_payload $+ capt_arg_payload $--> Vector.vector_end
      |> with_non_disj
    ; -"std" &:: "vector" &:: "clear" <>$ capt_arg_payload
      $--> Vector.invalidate_references Clear
      |> with_non_disj
    ; -"std" &:: "vector" &:: "emplace" $ capt_arg_payload
      $+...$--> Vector.invalidate_references Emplace
      |> with_non_disj
    ; -"std" &:: "vector" &:: "emplace_back" $ capt_arg_payload
      $+...$--> Vector.push_back_cpp ~vector_f:EmplaceBack ~desc:"std::vector::emplace_back()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "insert" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references Insert
      |> with_non_disj
    ; -"std" &:: "vector" &:: "operator=" <>$ capt_arg_payload
      $+...$--> Vector.invalidate_references_with_ret Assign
      |> with_non_disj
    ; -"std" &:: "vector" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
      $--> Vector.at ~desc:"std::vector::at()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "shrink_to_fit" <>$ capt_arg_payload
      $--> Vector.invalidate_references ShrinkToFit
      |> with_non_disj
    ; -"std" &:: "vector" &:: "push_back" <>$ capt_arg_payload
      $+...$--> Vector.push_back_cpp ~vector_f:PushBack ~desc:"std::vector::push_back()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "pop_back" <>$ capt_arg_payload
      $+...$--> Vector.pop_back ~desc:"std::vector::pop_back()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "empty" <>$ capt_arg_payload
      $--> GenericArrayBackedCollection.empty ~desc:"std::vector::is_empty()"
      |> with_non_disj
    ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> Vector.reserve
      |> with_non_disj
    ; -"std" &:: "vector" &:: "size" $ capt_arg_payload
      $--> GenericArrayBackedCollection.size ~desc:"std::vector::size()"
      |> with_non_disj
    ; -"std" &:: "distance" &--> Basic.nondet ~desc:"std::distance" |> with_non_disj
    ; -"std" &:: "integral_constant" < any_typ &+ capt_int
      >::+ (fun _ name -> String.is_prefix ~prefix:"operator_" name)
      <>--> Basic.return_int ~desc:"std::integral_constant"
      |> with_non_disj
    ; (* consider that all fbstrings are small strings to avoid false positives due to manual
         ref-counting *)
      -"folly" &:: "fbstring_core" &:: "category"
      &--> Basic.return_int Int64.zero ~desc:"folly::fbstring_core::category"
      |> with_non_disj
    ; -"folly" &:: "DelayedDestruction" &:: "destroy"
      &++> Basic.unknown_call "folly::DelayedDestruction::destroy is modelled as skip"
      |> with_non_disj
    ; -"folly" &:: "SocketAddress" &:: "~SocketAddress"
      &++> Basic.unknown_call "folly::SocketAddress's destructor is modelled as skip"
      |> with_non_disj ]


let matchers =
  matchers
  @ List.map simple_matchers
      ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
