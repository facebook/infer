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
open PulseOperationResult.Import
open PulseModelsImport

let value = Fieldname.make PulseOperations.pulse_model_type "__infer_backing_pointer"

let value_access = Access.FieldAccess value

let to_internal_value ?(access = value_access) path mode location value astate =
  PulseOperations.eval_access path mode location value access astate


let to_internal_value_deref ?access path mode location value astate =
  let* astate, pointer = to_internal_value ?access path Read location value astate in
  PulseOperations.eval_access path mode location pointer Dereference astate


let write_value ?access path location this ~value ~desc astate =
  let* astate, pointer = to_internal_value ?access path Read location this astate in
  let value_hist = (fst value, Hist.add_call path location desc (snd value)) in
  let+ astate = PulseOperations.write_deref path location ~ref:pointer ~obj:value_hist astate in
  (astate, (pointer, value_hist))


let assign_value_nullptr ?access path location this ~desc astate =
  let=* astate, (pointer, value) =
    write_value ?access path location this
      ~value:(AbstractValue.mk_fresh (), ValueHistory.epoch)
      ~desc astate
  in
  let++ astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
  PulseOperations.invalidate path
    (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
    location (ConstantDereference IntLit.zero) value astate


let dereference this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, ((value_addr, value_hist) as value) =
    to_internal_value_deref path Write location this astate
  in
  (* Check dereference to show an error at the callsite of `operator*()` or `operator->()` *)
  let<+> astate, _ = PulseOperations.eval_access path Write location value Dereference astate in
  PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


let get this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (value_addr, value_hist) =
    to_internal_value_deref path Read location this astate
  in
  PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


let at this index ~desc : model_no_non_disj =
 fun {path; location; ret} astate ->
  let event = Hist.call_event path location desc in
  let<*> astate, internal_array =
    PulseOperations.eval_deref_access path Read location this value_access astate
  in
  let<+> astate, (addr, hist) =
    PulseOperations.eval_access path Read location internal_array
      (ArrayAccess (StdTyp.void, fst index))
      astate
  in
  PulseOperations.write_id (fst ret) (addr, Hist.add_event event hist) astate


let swap this other ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<*> astate, this_value = to_internal_value_deref path Read location this astate in
  let<*> astate, other_value = to_internal_value_deref path Read location other astate in
  let<*> astate, _ = write_value path location this ~value:other_value ~desc astate in
  let<+> astate, _ = write_value path location other ~value:this_value ~desc astate in
  astate


let operator_bool this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (value_addr, _) = to_internal_value_deref path Write location this astate in
  PulseOperations.write_id ret_id (value_addr, Hist.single_call path location desc) astate


let find_element_type_common matchers tenv typ =
  match (Typ.strip_ptr typ).desc with
  | Tstruct name ->
      Tenv.find_map_supers tenv name ~f:(fun name _ -> matchers () name)
  | _ ->
      None


(* A custom deleter is a function pointer or an object of a class with a call operator. A
   [unique_ptr] keeps a function pointer in its [deleter] field, which stands for the deleter object
   otherwise. A [shared_ptr] keeps its deleter in the [deleter] field of its control block, i.e. of
   the cell holding its reference count, so that copies share it. *)
let deleter_access =
  Access.FieldAccess (Fieldname.make PulseOperations.pulse_model_type "__infer_backing_deleter")


let write_deleter path location this deleter astate =
  let* astate, field = PulseOperations.eval_access path Read location this deleter_access astate in
  PulseOperations.write_deref path location ~ref:field ~obj:deleter astate


let read_deleter path location this astate =
  PulseOperations.eval_deref_access path Read location this deleter_access astate


(** the type of the pointer managed by a smart pointer to [elem_typ], and how [std::default_delete]
    disposes of it *)
let pointer_type_and_delete (elem_typ : Typ.t) =
  match elem_typ.desc with
  | Tarray {elt} ->
      (Typ.mk_ptr elt, Invalidation.CppDeleteArray)
  | _ ->
      (Typ.mk_ptr elem_typ, Invalidation.CppDelete)


(** the call operator of [class_name], or of one of its superclasses, to call on a pointer of type
    [pointer_typ]: the only one, or the one taking exactly that type if it is overloaded *)
let find_call_operator tenv class_name ~pointer_typ =
  Tenv.find_map_supers tenv class_name ~f:(fun _ struct_opt ->
      let call_operators =
        Option.value_map struct_opt ~default:[] ~f:(fun {Struct.methods} ->
            List.filter_map methods ~f:(fun {Struct.name} ->
                if String.equal (Procname.get_method name) "operator()" then
                  match IRAttributes.load_formal_types name with
                  | [_this; formal] ->
                      Some (name, formal)
                  | _ ->
                      None
                else None ) )
      in
      match call_operators with
      | [(call_operator, _)] ->
          Some call_operator
      | _ ->
          List.find_map call_operators ~f:(fun (call_operator, formal) ->
              Option.some_if (Typ.equal_ignore_quals formal pointer_typ) call_operator ) )


let type_erased_function =
  QualifiedCppName.Match.of_fuzzy_qual_names ["folly::Function"; "std::function"]


(** the call operator to run a deleter of class [class_name] on a pointer of type [pointer_typ], if
    the deleter has no state: our deleter objects do not keep track of fields, of lambda captures,
    or of the callable wrapped by a type-erased function *)
let find_deleter_call_operator tenv class_name ~pointer_typ =
  let is_type_erased =
    match class_name with
    | Typ.CppClass {name} ->
        QualifiedCppName.Match.match_qualifiers type_erased_function name
    | _ ->
        false
  in
  let has_fields () =
    Tenv.mem_supers tenv class_name ~f:(fun _ struct_opt ->
        Option.exists struct_opt ~f:(fun {Struct.fields} -> not (List.is_empty fields)) )
  in
  if is_type_erased || has_fields () then None
  else
    find_call_operator tenv class_name ~pointer_typ
    |> Option.filter ~f:(fun call_operator ->
        Option.for_all (IRAttributes.load call_operator) ~f:(fun {ProcAttributes.captured} ->
            List.is_empty captured ) )


(* We need expressions corresponding to the values we pass to [free_or_delete] or to deleters. We
   don't have them so we create fake ones from fresh variables instead.

   One day we may need to store the [fake_ident] -> [value] into the stack of the abstract state too
   so that if something tries to re-evaluate the fake exp they will get the right value. *)
let mk_fake_arg value typ =
  { FuncArg.arg_payload= ValueOrigin.unknown value
  ; exp= Exp.Var (Ident.create_fresh Ident.kprimed)
  ; typ }


(** call [deleter] on [pointer]: either the function it points to or the call operator of its class.
    A deleter we cannot resolve is an unknown call, which may release the pointer. *)
let apply_custom_deleter ~desc deleter (pointer, pointer_typ) : model =
 fun {analysis_data; dispatch_call_eval_args; path; location; ret} astate non_disj ->
  let call_operator class_name obj =
    find_deleter_call_operator analysis_data.tenv class_name ~pointer_typ
    |> Option.map ~f:(fun callee ->
        (callee, [mk_fake_arg obj (Typ.mk_ptr (Typ.mk_struct class_name))]) )
  in
  let callee_and_args, astate =
    match deleter with
    | `Value ((value, _) as deleter) -> (
      match PulseArithmetic.get_dynamic_type value astate with
      | Some {typ= {desc= Tstruct (CFunction csig)}} ->
          (Some (Procname.C csig, []), astate)
      | Some {typ= {desc= Tstruct class_name}} ->
          (call_operator class_name deleter, astate)
      | _ ->
          (* callers may know what a function pointer coming from them points to *)
          (None, AbductiveDomain.add_need_dynamic_type_specialization value astate) )
    | `Object (class_name, obj) ->
        (call_operator class_name obj, astate)
    | `Unknown ->
        (None, astate)
  in
  match callee_and_args with
  | Some (callee, args) ->
      let pointer_arg =
        match List.last (IRAttributes.load_formal_types callee) with
        | Some formal_typ when Typ.is_reference formal_typ ->
            (* the deleter expects the address of a cell holding the pointer *)
            let copy = (AbstractValue.mk_fresh (), snd pointer) in
            let=* astate =
              PulseOperations.write_deref path location ~ref:copy ~obj:pointer astate
            in
            let++ astate = PulseArithmetic.and_positive (fst copy) astate in
            (astate, mk_fake_arg copy formal_typ)
        | _ ->
            Sat (Ok (astate, mk_fake_arg pointer pointer_typ))
      in
      bind_sat_result non_disj pointer_arg (fun (astate, pointer_arg) ->
          dispatch_call_eval_args analysis_data path ret (Const (Cfun callee))
            (args @ [pointer_arg]) location CallFlags.default astate non_disj (Some callee) )
  | None ->
      let astates =
        let<++> astate =
          PulseCallOperations.unknown_call analysis_data.tenv path location (Model desc) None ~ret
            ~actuals:[(pointer, pointer_typ)]
            ~formals_opt:None astate
        in
        astate
      in
      (astates, non_disj)


module SharedPtr = struct
  let count = Fieldname.make PulseOperations.pulse_model_type "__infer_backing_count"

  let count_access = Access.FieldAccess count

  let to_internal_count ?(access = count_access) path mode location value astate =
    PulseOperations.eval_access path mode location value access astate


  let to_internal_count_deref ?access path mode location value astate =
    let* astate, pointer = to_internal_count ?access path Read location value astate in
    PulseOperations.eval_access path mode location pointer Dereference astate


  let delete_internal_count path location this ~desc astate =
    let call_event = Hist.call_event path location desc in
    let+ astate, (value_addr, value_hist) = to_internal_count path Read location this astate in
    let value_addr_hist = (value_addr, Hist.add_event call_event value_hist) in
    PulseOperations.invalidate_access path location CppDelete value_addr_hist Dereference astate


  let write_count ?access path location this ~value ~desc astate =
    let* astate, pointer = to_internal_count ?access path Read location this astate in
    let value_hist = (fst value, Hist.add_call path location desc (snd value)) in
    let+ astate = PulseOperations.write_deref path location ~ref:pointer ~obj:value_hist astate in
    (astate, (pointer, value_hist))


  let assign_constant path location ~ref ~constant ~desc astate =
    let value = (AbstractValue.mk_fresh (), Hist.add_call path location desc ValueHistory.epoch) in
    let=* astate = PulseOperations.write_deref path location ~ref ~obj:value astate in
    PulseArithmetic.and_eq_int (fst value) constant astate


  let fresh_value_from_constant path location ~constant ~desc astate =
    let address =
      (AbstractValue.mk_fresh (), Hist.add_call path location desc ValueHistory.epoch)
    in
    let** astate = PulseArithmetic.and_positive (fst address) astate in
    let++ astate = assign_constant path location ~ref:address ~constant ~desc astate in
    (astate, address)


  let assign_count path location this ~constant ~desc astate =
    let+* astate, value = fresh_value_from_constant path location ~constant ~desc astate in
    let+ astate, _ = write_count path location this ~value ~desc astate in
    astate


  (* weak pointers sharing the count see that the object has expired *)
  let expire_count path location this ~desc astate =
    let=* astate, pointer = to_internal_count_deref path Read location this astate in
    assign_constant path location ~ref:pointer ~constant:IntLit.zero ~desc astate


  let decrease_count path location this ~desc astate =
    let=* astate, (pointer, int_hist) = to_internal_count_deref path Read location this astate in
    let hist = Hist.add_call path location desc int_hist in
    let=* astate, (count_addr, _) =
      PulseOperations.eval_access path Read location (pointer, hist) Dereference astate
    in
    let bop_addr = AbstractValue.mk_fresh () in
    let+* astate, bop_addr =
      PulseArithmetic.eval_binop bop_addr (MinusA None) (AbstractValueOperand count_addr)
        (ConstOperand (Cint (IntLit.of_int 1)))
        astate
    in
    PulseOperations.write_deref path location ~ref:(pointer, int_hist) ~obj:(bop_addr, int_hist)
      astate


  let default_constructor this ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<**> astate = assign_value_nullptr path location this ~desc astate in
    let<++> astate = assign_count path location this ~constant:IntLit.zero ~desc astate in
    astate


  let is_shared_ptr _context s =
    String.equal s "shared_ptr" || String.equal s "__shared_ptr"
    || String.equal s "__shared_ptr_access"


  let find_element_type =
    let matchers : (unit, Typ.t, unit) ProcnameDispatcher.TypName.dispatcher =
      let open ProcnameDispatcher.TypName in
      make_dispatcher [-"std" &::+ is_shared_ptr < capt_typ &+...>--> Fn.id]
    in
    fun tenv typ -> find_element_type_common matchers tenv typ


  let destructor FuncArg.{arg_payload= this; typ} ~desc : model =
   fun ({analysis_data= {tenv}; path; location} as model_data) astate non_disj ->
    let ( let<*!> ) x f = bind_sat_result non_disj (Sat x) f in
    let<*!> astate, pointer = to_internal_count_deref path Read location this astate in
    let<*!> astate, count =
      PulseOperations.eval_access path Read location pointer Dereference astate
    in
    let<*!> astate, (value_addr, value_hist) =
      to_internal_value_deref path Read location this astate
    in
    let value = (value_addr, Hist.add_call path location desc value_hist) in
    (* ref_count greater than one: decrement ref_count *)
    let ref_count_gt_one =
      PulseArithmetic.prune_gt_one (fst count) astate
      >>== decrease_count path location this ~desc
      >>|| ExecutionDomain.continue
    in
    (* ref_count is zero: deallocate ref_count *)
    let ref_count_zero =
      PulseArithmetic.prune_eq_zero (fst count) astate
      >>|= delete_internal_count path location this ~desc
      >>|| ExecutionDomain.continue
    in
    (* ref_count is one: dispose of the backing_pointer and set the ref_count to zero *)
    let dispose astate non_disj =
      match find_element_type tenv typ with
      | Some elem_typ ->
          let pointer_typ, invalidation = pointer_type_and_delete elem_typ in
          (* only the constructors taking a deleter record it in the control block *)
          let has_deleter =
            Option.is_some (Memory.find_edge_opt (fst pointer) deleter_access astate)
          in
          let<*!> astate, deleter = read_deleter path location pointer astate in
          if has_deleter || Option.is_some (PulseArithmetic.get_dynamic_type (fst deleter) astate)
          then
            (* unlike [unique_ptr], also on a null pointer *)
            apply_custom_deleter ~desc (`Value deleter) (value, pointer_typ) model_data astate
              non_disj
          else
            (* callers may know the deleter of a control block coming from them *)
            let astate =
              AbductiveDomain.add_need_dynamic_type_specialization (fst deleter) astate
            in
            Basic.free_or_delete `Delete invalidation (mk_fake_arg value pointer_typ) model_data
              astate non_disj
      | None ->
          L.die InternalError "Cannot find template arguments"
    in
    let ref_count_one, non_disj =
      bind_sat_result non_disj
        (PulseArithmetic.prune_eq_one (fst count) astate)
        (fun astate ->
          let astates, non_disj = dispose astate non_disj in
          ( List.concat_map astates ~f:(fun exec_state_result ->
                let<*> exec_state = exec_state_result in
                match exec_state with
                | ContinueProgram astate ->
                    let<++> astate = expire_count path location this ~desc astate in
                    astate
                | _ ->
                    [Ok exec_state] )
          , non_disj ) )
    in
    (SatUnsat.to_list ref_count_gt_one @ SatUnsat.to_list ref_count_zero @ ref_count_one, non_disj)


  let assign_pointer this value ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, _ = write_value path location this ~value ~desc astate in
    (* set ref_count to *)
    let astate_not_nullptr =
      PulseArithmetic.prune_positive (fst value) astate
      >>== assign_count path location this ~constant:IntLit.one ~desc
      >>|| ExecutionDomain.continue
    in
    let astate_nullptr =
      PulseArithmetic.prune_eq_zero (fst value) astate
      >>== assign_count path location this ~constant:IntLit.zero ~desc
      >>|| ExecutionDomain.continue
    in
    SatUnsat.to_list astate_not_nullptr @ SatUnsat.to_list astate_nullptr


  (* old versions of libc++ add a defaulted parameter of type [__nat] to some constructors *)
  let libcxx_nat = QualifiedCppName.Match.of_fuzzy_qual_names ["std::shared_ptr::__nat"]

  (** [shared_ptr(Y* p, D d, ...)]: the last owner calls [d(p)] instead of [delete p] *)
  let assign_pointer_with_deleter this FuncArg.{arg_payload= value}
      FuncArg.{arg_payload= deleter; typ= deleter_typ} ~desc : model_no_non_disj =
   fun ({path; location} as model_data) astate ->
    let deleter_opt =
      if Typ.is_pointer_to_function deleter_typ then Some (astate, deleter)
      else
        (* class arguments are passed by reference *)
        let class_typ = match deleter_typ.desc with Tptr (typ, _) -> typ | _ -> deleter_typ in
        match class_typ.desc with
        | Tstruct (CppClass {name}) when QualifiedCppName.Match.match_qualifiers libcxx_nat name ->
            None
        | _ ->
            (* the copy of the deleter owned by the control block *)
            let obj = (AbstractValue.mk_fresh (), Hist.single_call path location desc) in
            let astate =
              match class_typ.desc with
              | Tstruct class_name ->
                  PulseArithmetic.and_dynamic_type_is_unsafe (fst obj) (Typ.mk_struct class_name)
                    location astate
              | _ ->
                  astate
            in
            Some (astate, obj)
    in
    match deleter_opt with
    | None ->
        assign_pointer this value ~desc model_data astate
    | Some (astate, deleter) ->
        (* unlike [shared_ptr(p)], this owns [p] even if it is null *)
        let<*> astate, _ = write_value path location this ~value ~desc astate in
        let<**> astate = assign_count path location this ~constant:IntLit.one ~desc astate in
        let<*> astate, control_block = to_internal_count_deref path Read location this astate in
        let<+> astate = write_deleter path location control_block deleter astate in
        astate


  let copy_constructor FuncArg.{arg_payload= this} other ~desc : model_no_non_disj =
   fun {path; location} astate ->
    (* copy value pointer*)
    let<*> astate, value = to_internal_value_deref path Read location other astate in
    let<*> astate, _ = write_value path location this ~value ~desc astate in
    (* if 'other' is managing an object *)
    let astate_not_nullptr =
      let** astate = PulseArithmetic.prune_positive (fst value) astate in
      let=* astate, (this_internal_count, _) = to_internal_count path Read location this astate in
      let=* astate, (pointer, int_hist) = to_internal_count_deref path Read location other astate in
      let=* astate, count =
        PulseOperations.eval_access path Read location (pointer, int_hist) Dereference astate
      in
      let** astate = PulseArithmetic.and_positive (fst count) astate in
      (* copy the pointer to ref count*)
      let=* astate =
        PulseOperations.write_deref path location ~ref:(this_internal_count, int_hist)
          ~obj:(pointer, int_hist) astate
      in
      let hist = Hist.add_call path location desc int_hist in
      (* compute the increased ref count *)
      let=* astate, (count_addr, _) =
        PulseOperations.eval_access path Read location (pointer, hist) Dereference astate
      in
      let incremented_count = AbstractValue.mk_fresh () in
      let+* astate, incremented_count =
        PulseArithmetic.eval_binop incremented_count (PlusA None) (AbstractValueOperand count_addr)
          (ConstOperand (Cint (IntLit.of_int 1)))
          astate
      in
      (* update the ref count *)
      PulseOperations.write_deref path location ~ref:(pointer, int_hist)
        ~obj:(incremented_count, int_hist) astate
      |> Basic.map_continue
    in
    (* if 'other' is not managing an object *)
    let astate_nullptr =
      PulseArithmetic.prune_eq_zero (fst value) astate
      >>== assign_count path location this ~constant:IntLit.zero ~desc
      >>|| ExecutionDomain.continue
    in
    SatUnsat.to_list astate_not_nullptr @ SatUnsat.to_list astate_nullptr


  let copy_assignment (FuncArg.{arg_payload= this} as arg) other ~desc : model =
   fun model_data astate non_disj ->
    let ( let<**> ) x f = bind_sat_result non_disj x f in
    let op1 = Formula.AbstractValueOperand (fst this) in
    let op2 = Formula.AbstractValueOperand (fst other) in
    (* self-assignment *)
    let astate_equals = PulseArithmetic.and_equal op1 op2 astate >>|| ExecutionDomain.continue in
    let<**> astate_not_equals = PulseArithmetic.and_not_equal op1 op2 astate in
    let astate_not_equals, non_disj = destructor arg ~desc model_data astate_not_equals non_disj in
    let astate_not_equals =
      List.concat_map astate_not_equals ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate_not_equals ->
              copy_constructor arg other ~desc model_data astate_not_equals
          | _ ->
              [Ok exec_state] )
    in
    (SatUnsat.to_list astate_equals @ astate_not_equals, non_disj)


  let move_assignment this other ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, value = to_internal_value_deref path Read location other astate in
    let<*> astate, count = to_internal_count_deref path Read location other astate in
    let<*> astate, _ = write_value path location (fst this, snd value) ~value ~desc astate in
    let<*> astate, _ = write_count path location (fst this, snd count) ~value:count ~desc astate in
    let<**> astate = assign_value_nullptr path location other ~desc astate in
    let<++> astate = assign_count path location other ~constant:IntLit.zero ~desc astate in
    astate


  let reset (FuncArg.{arg_payload= this} as arg) value ~desc : model =
   fun ({path; location} as model_data) astate non_disj ->
    let astates, non_disj = destructor arg ~desc model_data astate non_disj in
    let astates =
      List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              let<*> astate, _ = write_value path location this ~value ~desc astate in
              let<++> astate = assign_count path location this ~constant:IntLit.one ~desc astate in
              astate
          | _ ->
              [Ok exec_state] )
    in
    (astates, non_disj)


  let is_rvalue callee_procname arg_index =
    let formals = IRAttributes.load_formal_types callee_procname in
    let other_typ = List.nth formals arg_index in
    match other_typ with Some {Typ.desc= Tptr (_, Pk_rvalue_reference)} -> true | _ -> false


  let copy_move_assignment (FuncArg.{arg_payload= this} as arg) other ~desc : model =
   fun ({callee_procname} as model_data) astate non_disj ->
    if is_rvalue callee_procname 1 then
      (move_assignment this other ~desc:(desc ^ " (move)") model_data astate, non_disj)
    else copy_assignment arg other ~desc:(desc ^ " (copy)") model_data astate non_disj


  let copy_move_constructor (FuncArg.{arg_payload= this} as arg) other ~desc : model_no_non_disj =
   fun ({callee_procname} as model_data) ->
    if is_rvalue callee_procname 1 then
      move_assignment this other ~desc:(desc ^ " (move)") model_data
    else copy_constructor arg other ~desc:(desc ^ " (copy)") model_data


  let swap this other ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, this_value = to_internal_value_deref path Read location this astate in
    let<*> astate, other_value = to_internal_value_deref path Read location other astate in
    let<*> astate, this_count = to_internal_count_deref path Read location this astate in
    let<*> astate, other_count = to_internal_count_deref path Read location other astate in
    let<*> astate, _ = write_value path location this ~value:other_value ~desc astate in
    let<*> astate, _ = write_value path location other ~value:this_value ~desc astate in
    let<*> astate, _ = write_count path location this ~value:other_count ~desc astate in
    let<+> astate, _ = write_count path location other ~value:this_count ~desc astate in
    astate


  let use_count ~access this ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, pointer = to_internal_count_deref ~access path Read location this astate in
    let<+> astate, (value_addr, value_hist) =
      PulseOperations.eval_access path Read location pointer Dereference astate
    in
    PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


  let default_reset (FuncArg.{arg_payload= this} as arg) ~desc : model =
   fun ({path; location} as model_data) astate non_disj ->
    let astates, non_disj = destructor arg ~desc model_data astate non_disj in
    let astates =
      List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              let<**> astate = assign_value_nullptr path location this ~desc astate in
              let<++> astate = assign_count path location this ~constant:IntLit.zero ~desc astate in
              astate
          | _ ->
              [Ok exec_state] )
    in
    (astates, non_disj)


  let make_shared (args : (AbstractValue.t * ValueHistory.t) FuncArg.t list) ~desc : model =
   fun ({callee_procname; path; location} as model_data) astate non_disj ->
    let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
    let ( let<**> ) x f = bind_sat_result non_disj x f in
    let this, args_without_this, actuals =
      match
        ( args |> List.last
        , args |> List.drop_last
        , IRAttributes.load_formal_types callee_procname |> List.drop_last )
      with
      | Some this, Some args_without_this, Some formals ->
          (this, args_without_this, formals)
      | _ ->
          L.die InternalError "Not enough arguments to call make_shared"
    in
    let typ =
      match (Typ.strip_ptr this.typ).desc with
      | Typ.Tstruct (CppClass {template_spec_info= Template {args= TType t :: _}}) ->
          t
      | _ ->
          L.die InternalError "Cannot find template arguments of make_shared"
    in
    let this = this.arg_payload in
    let<**> astate = assign_count path location this ~constant:IntLit.one ~desc astate in
    match typ.desc with
    | Tstruct class_name ->
        (* assign the value pointer to the field of the shared_ptr *)
        let<**> astate, value_address = Basic.alloc_value_address ~desc typ model_data astate in
        let<*> astate, _ = write_value path location this ~value:value_address ~desc astate in
        let typ = Typ.mk (Tptr (typ, Typ.Pk_pointer)) in
        (* dereferences the actual arguments if they are primitive types.
           In fact, when primitive types are passed in a function call, the frontend seems to dereference them.
           We have to simulate this part when we call the constructor.
        *)
        let<*> astate, args_without_this =
          PulseResult.list_fold (List.rev args_without_this) ~init:(astate, [])
            ~f:(fun (astate, rev_func_args) (FuncArg.{exp; typ; arg_payload= value} as arg) ->
              let+ astate, new_payload =
                PulseOperations.eval_access path Read location value Dereference astate
              in
              match typ.desc with
              | Tptr ({Typ.desc= Tstruct _}, _) ->
                  (astate, arg :: rev_func_args)
              | _ ->
                  (astate, {FuncArg.exp; arg_payload= new_payload; typ} :: rev_func_args) )
        in
        (* We need an expression corresponding to the value of the argument we pass to
           the constructor. See e.g. the comment on [mk_fake_arg] as well. *)
        let fake_exp = Exp.Var (Ident.create_fresh Ident.kprimed) in
        let args = {FuncArg.typ; exp= fake_exp; arg_payload= value_address} :: args_without_this in
        let args = List.map args ~f:(FuncArg.map_payload ~f:ValueOrigin.unknown) in
        (* create the list of types of the actual arguments of the constructor
           Note that these types are the formal arguments of make_shared *)
        let actuals = typ :: actuals in
        Basic.call_constructor class_name actuals args fake_exp model_data astate non_disj
    | _ -> (
        L.d_printfln "std::make_shared called on non-class type, assuming primitive type" ;
        match args_without_this with
        | [] ->
            (* assign the value pointer to the field of the shared_ptr *)
            let<**> astate, value_address = Basic.alloc_value_address ~desc typ model_data astate in
            let<*> astate, _ = write_value path location this ~value:value_address ~desc astate in
            let<**> astate =
              assign_constant path location ~ref:value_address ~constant:IntLit.zero ~desc astate
            in
            (Basic.ok_continue astate, non_disj)
        | first_arg :: _ ->
            let<**> astate, address =
              Basic.deep_copy path location ~value:first_arg.arg_payload ~desc astate
            in
            let<*> astate, _ = write_value path location this ~value:address ~desc astate in
            (Basic.ok_continue astate, non_disj) )


  let pointer_cast src tgt ~desc = copy_move_constructor tgt src ~desc
end

(* A [std::weak_ptr] points to the object and to the reference count of the [std::shared_ptr]s it
   was created from, but never changes the count. It has its own fields so that changes to the object
   or to the count are not seen as modifications of the [weak_ptr] by the checks for unnecessary
   copies and const-refable parameters (see [PulseNonDisjunctiveOperations]). *)
module WeakPtr = struct
  let pointer_field = Access.FieldAccess PulseOperations.ModeledField.weak_ptr_pointer

  let count_field = Access.FieldAccess PulseOperations.ModeledField.weak_ptr_count

  let is_weak_ptr _context s = String.equal s "weak_ptr" || String.equal s "__weak_ptr"

  let read path location this astate =
    let* astate, value =
      to_internal_value_deref ~access:pointer_field path Read location this astate
    in
    let+ astate, count =
      SharedPtr.to_internal_count_deref ~access:count_field path Read location this astate
    in
    (astate, value, count)


  let write path location this ~value ~count ~desc astate =
    let* astate, _ = write_value ~access:pointer_field path location this ~value ~desc astate in
    let+ astate, _ =
      SharedPtr.write_count ~access:count_field path location this ~value:count ~desc astate
    in
    astate


  let default_constructor this ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<**> astate = assign_value_nullptr ~access:pointer_field path location this ~desc astate in
    let<**> astate, count =
      SharedPtr.fresh_value_from_constant path location ~constant:IntLit.zero ~desc astate
    in
    let<+> astate, _ =
      SharedPtr.write_count ~access:count_field path location this ~value:count ~desc astate
    in
    astate


  (* Moving is modelled as copying: emptying the source would make moving a copy, e.g. when
     returning it, look like a modification of that copy. *)
  let copy this other ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, value, count = read path location other astate in
    let<+> astate = write path location this ~value ~count ~desc astate in
    astate


  let from_shared this shared ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, value = to_internal_value_deref path Read location shared astate in
    let<*> astate, count = SharedPtr.to_internal_count_deref path Read location shared astate in
    let astate_not_nullptr =
      let** astate = PulseArithmetic.prune_positive (fst value) astate in
      let=* astate, (count_value, _) =
        PulseOperations.eval_access path Read location count Dereference astate
      in
      (* a non-null [shared_ptr] owns its object *)
      let+* astate = PulseArithmetic.and_positive count_value astate in
      write path location this ~value ~count ~desc astate
    in
    let astate_nullptr =
      let** astate = PulseArithmetic.prune_eq_zero (fst value) astate in
      let+* astate, count =
        SharedPtr.fresh_value_from_constant path location ~constant:IntLit.zero ~desc astate
      in
      write path location this ~value ~count ~desc astate
    in
    SatUnsat.to_list (astate_not_nullptr >>|| ExecutionDomain.continue)
    @ SatUnsat.to_list (astate_nullptr >>|| ExecutionDomain.continue)


  let swap this other ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<*> astate, this_value, this_count = read path location this astate in
    let<*> astate, other_value, other_count = read path location other astate in
    let<*> astate = write path location this ~value:other_value ~count:other_count ~desc astate in
    let<+> astate = write path location other ~value:this_value ~count:this_count ~desc astate in
    astate


  (* split on whether the object observed by [this] has expired (an empty weak pointer has a count
     of zero) or is still owned, in which case [this] is not null *)
  let case_split path location this astate ~expired ~alive =
    let<*> astate, value, count = read path location this astate in
    let<*> astate, ((count_value, _) as count_value_hist) =
      PulseOperations.eval_access path Read location count Dereference astate
    in
    let expired_states =
      let<**> astate =
        PulseArithmetic.prune_binop ~negated:false Le (AbstractValueOperand count_value)
          (ConstOperand (Cint IntLit.zero)) astate
      in
      expired astate
    in
    let alive_states =
      let<**> astate =
        let** astate = PulseArithmetic.prune_positive count_value astate in
        PulseArithmetic.and_positive (fst value) astate
      in
      alive ~value ~count:(count, count_value_hist) astate
    in
    expired_states @ alive_states


  let share_ownership path location ~result ~desc ~value ~count:(count, (count_value, count_hist))
      astate =
    let<*> astate, _ = write_value path location result ~value ~desc astate in
    let<*> astate, _ = SharedPtr.write_count path location result ~value:count ~desc astate in
    let<**> astate, incremented_count =
      PulseArithmetic.eval_binop (AbstractValue.mk_fresh ()) (PlusA None)
        (AbstractValueOperand count_value) (ConstOperand (Cint IntLit.one)) astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:count ~obj:(incremented_count, count_hist)
        astate
    in
    astate


  let lock FuncArg.{arg_payload= this; typ} result ~desc : model_no_non_disj =
   fun ({path; location; callee_procname} as model_data) astate ->
    (* the result is usually a temporary, which the decompiler cannot name: without this, messages
       name whichever model field was last read with the same value, e.g. a count of zero *)
    let astate =
      Decompiler.add_call_source (fst result) (Call callee_procname) [(this, typ)] astate
    in
    case_split path location this astate
      ~expired:(SharedPtr.default_constructor result ~desc model_data)
      ~alive:(share_ownership path location ~result ~desc)


  let expired this ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let return_int i astate =
      let astate, ret_value = PulseArithmetic.absval_of_int astate (IntLit.of_int i) in
      PulseOperations.write_id ret_id (ret_value, Hist.single_call path location desc) astate
      |> Basic.ok_continue
    in
    case_split path location this astate ~expired:(return_int 1) ~alive:(fun ~value:_ ~count:_ ->
        return_int 0 )


  (* [std::shared_ptr(const std::weak_ptr&)] throws [std::bad_weak_ptr] instead of returning a null
     pointer *)
  let to_shared this weak ~desc : model_no_non_disj =
   fun ({path; location} as model_data) astate ->
    case_split path location weak astate ~expired:(Basic.early_exit model_data)
      ~alive:(share_ownership path location ~result:this ~desc)
end

module UniquePtr = struct
  (** what the deleter of a [unique_ptr] does with a non-null pointer *)
  type deleter_kind =
    | DefaultDelete of Invalidation.t  (** [std::default_delete]: [delete] or [delete[]] it *)
    | FunctionPointer  (** call the function pointer stored in the [deleter] field on it *)
    | Functor of Typ.Name.t  (** call the call operator of the deleter class on it *)
    | UnknownDeleter  (** e.g. a reference or a deleter with state: an unknown call *)

  let default_delete = QualifiedCppName.Match.of_fuzzy_qual_names ["std::default_delete"]

  (** the type of the pointer managed by a [unique_ptr], and the kind of its deleter *)
  let get_pointer_type_and_deleter_kind =
    let matchers : (unit, Typ.t * Typ.t, unit) ProcnameDispatcher.TypName.dispatcher =
      let open ProcnameDispatcher.TypName in
      make_dispatcher
        [ ( -"std" &:: "unique_ptr" < capt_typ &+ capt_typ
          &+...>--> fun elem_typ deleter_typ -> (elem_typ, deleter_typ) ) ]
    in
    fun tenv typ ->
      find_element_type_common matchers tenv typ
      |> Option.map ~f:(fun (elem_typ, (deleter_typ : Typ.t)) ->
          let pointer_typ, delete = pointer_type_and_delete elem_typ in
          let kind =
            match deleter_typ.desc with
            | Tstruct (CppClass {name})
              when QualifiedCppName.Match.match_qualifiers default_delete name ->
                DefaultDelete delete
            | _ when Typ.is_pointer_to_function deleter_typ ->
                FunctionPointer
            | Tstruct class_name
              when Option.is_some (find_deleter_call_operator tenv class_name ~pointer_typ) ->
                Functor class_name
            | _ ->
                UnknownDeleter
          in
          (pointer_typ, kind) )


  let has_function_pointer_deleter tenv typ =
    match get_pointer_type_and_deleter_kind tenv typ with
    | Some (_, FunctionPointer) ->
        true
    | _ ->
        false


  let default_constructor this ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<++> astate = assign_value_nullptr path location this ~desc astate in
    astate


  let assign_pointer this value ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let<+> astate, _ = write_value path location this ~value ~desc astate in
    astate


  (** [unique_ptr(T*, D)]: the deleter is passed by reference *)
  let assign_pointer_and_deleter FuncArg.{arg_payload= this; typ} value deleter ~desc :
      model_no_non_disj =
   fun ({analysis_data= {tenv}; path; location} as model_data) astate ->
    let<*> astate =
      if has_function_pointer_deleter tenv typ then
        let* astate, function_pointer =
          PulseOperations.eval_access path Read location deleter Dereference astate
        in
        write_deleter path location this function_pointer astate
      else Ok astate
    in
    assign_pointer this value ~desc model_data astate


  (** the deleter is only called on non-null pointers *)
  let if_non_null pointer (model : model) : model =
   fun model_data astate non_disj ->
    let null_case =
      PulseArithmetic.prune_eq_zero pointer astate >>|| ExecutionDomain.continue |> SatUnsat.to_list
    in
    let non_null_case, non_disj =
      bind_sat_result non_disj (PulseArithmetic.prune_positive pointer astate) (fun astate ->
          model model_data astate non_disj )
    in
    (null_case @ non_null_case, non_disj)


  (** dispose of the pointer managed by [this] like its deleter does *)
  let destructor FuncArg.{arg_payload= this; typ} ~desc : model =
   fun ({analysis_data= {tenv}; path; location} as model_data) astate non_disj ->
    let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
    let<*> astate, (value_addr, value_hist) =
      to_internal_value_deref path Read location this astate
    in
    let value = (value_addr, Hist.add_call path location desc value_hist) in
    match get_pointer_type_and_deleter_kind tenv typ with
    | Some (pointer_typ, DefaultDelete invalidation) ->
        Basic.free_or_delete `Delete invalidation (mk_fake_arg value pointer_typ) model_data astate
          non_disj
    | Some (pointer_typ, FunctionPointer) ->
        let<*> astate, function_pointer = read_deleter path location this astate in
        if_non_null value_addr
          (apply_custom_deleter ~desc (`Value function_pointer) (value, pointer_typ))
          model_data astate non_disj
    | Some (pointer_typ, Functor class_name) ->
        let<*> astate, deleter =
          PulseOperations.eval_access path Read location this deleter_access astate
        in
        if_non_null value_addr
          (apply_custom_deleter ~desc (`Object (class_name, deleter)) (value, pointer_typ))
          model_data astate non_disj
    | Some (pointer_typ, UnknownDeleter) ->
        if_non_null value_addr
          (apply_custom_deleter ~desc `Unknown (value, pointer_typ))
          model_data astate non_disj
    | None ->
        L.internal_error "Cannot find template arguments@." ;
        (Basic.ok_continue astate, non_disj)


  let default_reset (FuncArg.{arg_payload= this} as arg) ~desc : model =
   fun ({path; location} as model_data) astate non_disj ->
    let astates, non_disj = destructor arg ~desc model_data astate non_disj in
    ( List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              let<++> astate = assign_value_nullptr path location this ~desc astate in
              astate
          | _ ->
              [Ok exec_state] )
    , non_disj )


  let reset (FuncArg.{arg_payload= this} as arg) value ~desc : model =
   fun ({path; location} as model_data) astate non_disj ->
    let astates, non_disj = destructor arg ~desc model_data astate non_disj in
    ( List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              let<+> astate, _ = write_value path location this ~value ~desc astate in
              astate
          | _ ->
              [Ok exec_state] )
    , non_disj )


  let release this ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, (old_value_addr, old_value_hist) =
      to_internal_value_deref path Read location this astate
    in
    let<++> astate = assign_value_nullptr path location this ~desc astate in
    PulseOperations.write_id ret_id
      (old_value_addr, Hist.add_call path location desc old_value_hist)
      astate


  let get_deleter this ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let<+> astate, (deleter_addr, deleter_hist) =
      PulseOperations.eval_access path Read location this deleter_access astate
    in
    PulseOperations.write_id ret_id
      (deleter_addr, Hist.add_call path location desc deleter_hist)
      astate


  let move_deleter tenv path location ~this_typ this other astate =
    if has_function_pointer_deleter tenv this_typ then
      let* astate, deleter = read_deleter path location other astate in
      write_deleter path location this deleter astate
    else Ok astate


  let move_constructor FuncArg.{arg_payload= this; typ} other ~desc : model_no_non_disj =
   fun {analysis_data= {tenv}; path; location} astate ->
    let<*> astate, value = to_internal_value_deref path Read location other astate in
    let<*> astate = move_deleter tenv path location ~this_typ:typ this other astate in
    let<**> astate = assign_value_nullptr path location other ~desc astate in
    let<+> astate, _ = write_value path location this ~value ~desc astate in
    astate


  (** [operator=(unique_ptr&& other)] is [reset(other.release())] followed by moving the deleter *)
  let move_assignment (FuncArg.{arg_payload= this; typ} as arg) other ~desc : model =
   fun ({analysis_data= {tenv}; path; location} as model_data) astate non_disj ->
    let ( let<**> ) x f = bind_sat_result non_disj x f in
    let<**> astate, value = Sat (to_internal_value_deref path Read location other astate) in
    let<**> astate = assign_value_nullptr path location other ~desc astate in
    let astates, non_disj = reset arg value ~desc model_data astate non_disj in
    ( List.concat_map astates ~f:(fun exec_state_result ->
          let<*> exec_state = exec_state_result in
          match exec_state with
          | ContinueProgram astate ->
              let<+> astate = move_deleter tenv path location ~this_typ:typ this other astate in
              astate
          | _ ->
              [Ok exec_state] )
    , non_disj )


  let swap FuncArg.{arg_payload= this; typ} other ~desc : model_no_non_disj =
   fun ({analysis_data= {tenv}; path; location} as model_data) astate ->
    let<*> astate =
      if has_function_pointer_deleter tenv typ then
        let* astate, this_deleter = read_deleter path location this astate in
        let* astate, other_deleter = read_deleter path location other astate in
        let* astate = write_deleter path location this other_deleter astate in
        write_deleter path location other this_deleter astate
      else Ok astate
    in
    swap this other ~desc model_data astate
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ (* matchers for unique_ptr *)
    -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg_payload
    $--> UniquePtr.default_constructor ~desc:"std::unique_ptr::unique_ptr()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_ptr")
    $--> UniquePtr.move_constructor ~desc:"std::unique_ptr::unique_ptr(std::unique_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "operator=" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_ptr")
    $--> UniquePtr.move_assignment ~desc:"std::unique_ptr::operator=(std::unique_ptr<T>)"
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg_payload $+ capt_arg_payload
    $--> UniquePtr.assign_pointer ~desc:"std::unique_ptr::unique_ptr(T*)"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> UniquePtr.assign_pointer_and_deleter ~desc:"std::unique_ptr::unique_ptr(T*, D)"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "~unique_ptr" $ capt_arg
    $--> UniquePtr.destructor ~desc:"std::unique_ptr::~unique_ptr()"
  ; -"std" &:: "unique_ptr" &:: "reset" $ capt_arg
    $--> UniquePtr.default_reset ~desc:"std::unique_ptr::reset()"
  ; -"std" &:: "unique_ptr" &:: "reset" $ capt_arg $+ capt_arg_payload
    $--> UniquePtr.reset ~desc:"std::unique_ptr::reset(T*)"
  ; -"std" &:: "unique_ptr" &:: "release" $ capt_arg_payload
    $--> UniquePtr.release ~desc:"std::unique_ptr::release()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "get_deleter" $ capt_arg_payload
    $--> UniquePtr.get_deleter ~desc:"std::unique_ptr::get_deleter()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "operator[]" $ capt_arg_payload $+ capt_arg_payload
    $--> at ~desc:"std::unique_ptr::operator[]()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "get" $ capt_arg_payload
    $--> get ~desc:"std::unique_ptr::get()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "operator*" $ capt_arg_payload
    $--> dereference ~desc:"std::unique_ptr::operator*()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "operator->" <>$ capt_arg_payload
    $--> dereference ~desc:"std::unique_ptr::operator->()"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "swap" $ capt_arg $+ capt_arg_payload
    $--> UniquePtr.swap ~desc:"std::unique_ptr::swap(std::unique_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "unique_ptr" &:: "operator_bool" <>$ capt_arg_payload
    $--> operator_bool ~desc:"std::unique_ptr::operator_bool()"
    |> with_non_disj
    (* matchers for shared_ptr *)
  ; -"std" &:: "__shared_ptr" &:: "__shared_ptr" $ capt_arg_payload
    $--> SharedPtr.default_constructor ~desc:"std::shared_ptr::shared_ptr()"
    |> with_non_disj
  ; -"std" &:: "shared_ptr" &:: "shared_ptr" $ capt_arg_payload
    $--> SharedPtr.default_constructor ~desc:"std::shared_ptr::shared_ptr()"
    |> with_non_disj
  ; -"std" &:: "__shared_ptr" &:: "__shared_ptr" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "__shared_ptr")
    $+...$--> SharedPtr.copy_move_constructor
                ~desc:"std::shared_ptr::shared_ptr(std::shared_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "shared_ptr" &:: "shared_ptr" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "shared_ptr")
    $+...$--> SharedPtr.copy_move_constructor
                ~desc:"std::shared_ptr::shared_ptr(std::shared_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "__shared_ptr" &:: "operator=" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "__shared_ptr")
    $--> SharedPtr.copy_move_assignment ~desc:"std::shared_ptr::operator=(std::shared_ptr<T>)"
  ; -"std" &:: "shared_ptr" &:: "operator=" $ capt_arg
    $+ capt_arg_payload_of_typ (-"std" &:: "shared_ptr")
    $--> SharedPtr.copy_move_assignment ~desc:"std::shared_ptr::operator=(std::shared_ptr<T>)"
  ; -"std" &:: "__shared_ptr" &:: "__shared_ptr" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &::+ WeakPtr.is_weak_ptr)
    $--> WeakPtr.to_shared ~desc:"std::shared_ptr::shared_ptr(std::weak_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "shared_ptr" &:: "shared_ptr" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &::+ WeakPtr.is_weak_ptr)
    $--> WeakPtr.to_shared ~desc:"std::shared_ptr::shared_ptr(std::weak_ptr<T>)"
    |> with_non_disj
  ; -"std" &:: "__shared_ptr" &:: "__shared_ptr" $ capt_arg_payload $+ capt_arg $+ capt_arg
    $+...$--> SharedPtr.assign_pointer_with_deleter ~desc:"std::shared_ptr::shared_ptr(T*, D)"
    |> with_non_disj
  ; -"std" &:: "shared_ptr" &:: "shared_ptr" $ capt_arg_payload $+ capt_arg $+ capt_arg
    $+...$--> SharedPtr.assign_pointer_with_deleter ~desc:"std::shared_ptr::shared_ptr(T*, D)"
    |> with_non_disj
  ; -"std" &:: "__shared_ptr" &:: "__shared_ptr" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> SharedPtr.assign_pointer ~desc:"std::shared_ptr::shared_ptr(T*)"
    |> with_non_disj
  ; -"std" &:: "shared_ptr" &:: "shared_ptr" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> SharedPtr.assign_pointer ~desc:"std::shared_ptr::shared_ptr(T*)"
    |> with_non_disj
  ; -"std" &:: "__shared_ptr" &:: "~__shared_ptr" $ capt_arg
    $--> SharedPtr.destructor ~desc:"std::shared_ptr::~shared_ptr()"
  ; -"std" &:: "shared_ptr" &:: "~shared_ptr" $ capt_arg
    $--> SharedPtr.destructor ~desc:"std::shared_ptr::~shared_ptr()"
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "use_count" $ capt_arg_payload
    $--> SharedPtr.use_count ~access:SharedPtr.count_access ~desc:"std::shared_ptr::use_count()"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "reset" $ capt_arg $+ capt_arg_payload
    $--> SharedPtr.reset ~desc:"std::shared_ptr::reset(T*)"
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "reset" $ capt_arg
    $--> SharedPtr.default_reset ~desc:"std::shared_ptr::reset()"
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "operator[]" $ capt_arg_payload $+ capt_arg_payload
    $--> at ~desc:"std::shared_ptr::operator[]()"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "get" $ capt_arg_payload
    $--> get ~desc:"std::shared_ptr::get()"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "operator*" $ capt_arg_payload
    $--> dereference ~desc:"std::shared_ptr::operator*()"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "operator->" <>$ capt_arg_payload
    $--> dereference ~desc:"std::shared_ptr::operator->()"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "swap" $ capt_arg_payload $+ capt_arg_payload
    $--> SharedPtr.swap ~desc:"std::shared_ptr::swap(std::shared_ptr<T>)"
    |> with_non_disj
  ; -"std" &::+ SharedPtr.is_shared_ptr &:: "operator_bool" <>$ capt_arg_payload
    $--> operator_bool ~desc:"std::shared_ptr::operator_bool()"
    |> with_non_disj
  ; -"std" &:: "make_shared" &++> SharedPtr.make_shared ~desc:"std::make_shared()"
  ; -"std" &:: "static_pointer_cast" $ capt_arg_payload $+ capt_arg
    $--> SharedPtr.pointer_cast ~desc:"std::static_pointer_cast"
    |> with_non_disj
  ; -"std" &:: "dynamic_pointer_cast" $ capt_arg_payload $+ capt_arg
    $--> SharedPtr.pointer_cast ~desc:"std::static_pointer_cast"
    |> with_non_disj
  ; -"std" &:: "const_pointer_cast" $ capt_arg_payload $+ capt_arg
    $--> SharedPtr.pointer_cast ~desc:"std::static_pointer_cast"
    |> with_non_disj
  ; -"std" &:: "reinterpret_pointer_cast" $ capt_arg_payload $+ capt_arg
    $--> SharedPtr.pointer_cast ~desc:"std::static_pointer_cast"
    |> with_non_disj ]
  @
  (* matchers for weak_ptr and for its libstdc++ base class *)
  List.concat_map ["weak_ptr"; "__weak_ptr"] ~f:(fun weak_ptr ->
      let shared_ptr_arg = capt_arg_payload_of_typ (-"std" &::+ SharedPtr.is_shared_ptr) in
      let weak_ptr_arg = capt_arg_payload_of_typ (-"std" &::+ WeakPtr.is_weak_ptr) in
      [ -"std" &:: weak_ptr &:: weak_ptr $ capt_arg_payload
        $--> WeakPtr.default_constructor ~desc:"std::weak_ptr::weak_ptr()"
      ; -"std" &:: weak_ptr &:: weak_ptr $ capt_arg_payload $+ shared_ptr_arg
        $--> WeakPtr.from_shared ~desc:"std::weak_ptr::weak_ptr(std::shared_ptr<T>)"
      ; -"std" &:: weak_ptr &:: weak_ptr $ capt_arg_payload $+ weak_ptr_arg
        $--> WeakPtr.copy ~desc:"std::weak_ptr::weak_ptr(std::weak_ptr<T>)"
      ; -"std" &:: weak_ptr &:: "operator=" $ capt_arg_payload $+ shared_ptr_arg
        $--> WeakPtr.from_shared ~desc:"std::weak_ptr::operator=(std::shared_ptr<T>)"
      ; -"std" &:: weak_ptr &:: "operator=" $ capt_arg_payload $+ weak_ptr_arg
        $--> WeakPtr.copy ~desc:"std::weak_ptr::operator=(std::weak_ptr<T>)"
      ; -"std" &:: weak_ptr &:: "~" ^ weak_ptr &--> Basic.skip
      ; -"std" &:: weak_ptr &:: "reset" $ capt_arg_payload
        $--> WeakPtr.default_constructor ~desc:"std::weak_ptr::reset()"
      ; -"std" &:: weak_ptr &:: "swap" $ capt_arg_payload $+ capt_arg_payload
        $--> WeakPtr.swap ~desc:"std::weak_ptr::swap(std::weak_ptr<T>)"
      ; -"std" &:: weak_ptr &:: "use_count" $ capt_arg_payload
        $--> SharedPtr.use_count ~access:WeakPtr.count_field ~desc:"std::weak_ptr::use_count()"
      ; -"std" &:: weak_ptr &:: "expired" $ capt_arg_payload
        $--> WeakPtr.expired ~desc:"std::weak_ptr::expired()"
      ; -"std" &:: weak_ptr &:: "lock" $ capt_arg $+ capt_arg_payload
        $--> WeakPtr.lock ~desc:"std::weak_ptr::lock()" ]
      |> List.map ~f:with_non_disj )
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
