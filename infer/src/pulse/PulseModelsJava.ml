(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module Cplusplus = PulseModelsCpp
module GenericArrayBackedCollection = PulseModelsGenericArrayBackedCollection
module StringSet = Caml.Set.Make (String)

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
 fun {location; path; ret= ret_id, _} astate ->
  let event = Hist.call_event path location "Java.instanceof" in
  let res_addr = AbstractValue.mk_fresh () in
  match typeexpr with
  | Exp.Sizeof {typ} ->
      let<++> astate = PulseArithmetic.and_equal_instanceof res_addr argv typ astate in
      PulseOperations.write_id ret_id (res_addr, Hist.add_event path event hist) astate
  (* The type expr is sometimes a Var expr but this is not expected.
     This seems to be introduced by inline mechanism of Java synthetic methods during preanalysis *)
  | _ ->
      astate |> Basic.ok_continue


let call_may_throw_exception (exn : JavaClassName.t) : model =
 fun {location; path; analysis_data} astate ->
  let ret_addr = AbstractValue.mk_fresh () in
  let exn_name = JavaClassName.to_string exn in
  let desc = Printf.sprintf "throw_%s" exn_name in
  let ret_alloc_hist = Hist.single_alloc path location desc in
  let typ = Typ.mk_struct (Typ.JavaClass exn) in
  let astate = PulseOperations.add_dynamic_type typ ret_addr astate in
  let ret_var = Procdesc.get_ret_var analysis_data.proc_desc in
  let astate, ref = PulseOperations.eval_var path location ret_var astate in
  let obj = (ret_addr, ret_alloc_hist) in
  let<*> astate = PulseOperations.write_deref path location ~ref ~obj astate in
  [Ok (ExceptionRaised astate)]


let throw : model = fun _ astate -> [Ok (ExceptionRaised astate)]
(* Note that the Java frontend should have inserted an assignment of the
   thrown object to the `ret` variable just before this instruction. *)

module Object = struct
  (* naively modeled as shallow copy. *)
  let clone src_pointer_hist : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "Object.clone" in
    let<*> astate, obj =
      PulseOperations.eval_access path Read location src_pointer_hist Dereference astate
    in
    let<+> astate, obj_copy = PulseOperations.shallow_copy path location obj astate in
    PulseOperations.write_id ret_id (fst obj_copy, Hist.add_event path event (snd obj_copy)) astate
end

module Iterator = struct
  let constructor ~desc init : model =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let ref = (AbstractValue.mk_fresh (), Hist.single_event path event) in
    let<+> astate =
      GenericArrayBackedCollection.Iterator.construct path location event ~init ~ref astate
    in
    PulseOperations.write_id (fst ret) ref astate


  (* {curr -> v_c} is modified to {curr -> v_fresh} and returns array[v_c] *)
  let next ~desc iter : model =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let new_index = AbstractValue.mk_fresh () in
    let<*> astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollection.Iterator.to_internal_pointer path Read location iter astate
    in
    let<*> astate, (curr_elem_val, curr_elem_hist) =
      GenericArrayBackedCollection.element path location iter curr_index astate
    in
    let<+> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollection.Iterator.internal_pointer
        ~obj:(new_index, Hist.add_event path event curr_index_hist)
        astate
    in
    PulseOperations.write_id (fst ret)
      (curr_elem_val, Hist.add_event path event curr_elem_hist)
      astate


  (* {curr -> v_c } is modified to {curr -> v_fresh} and writes to array[v_c] *)
  let remove ~desc iter : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let new_index = AbstractValue.mk_fresh () in
    let<*> astate, (curr_index, curr_index_hist) =
      GenericArrayBackedCollection.Iterator.to_internal_pointer path Read location iter astate
    in
    let<*> astate =
      PulseOperations.write_field path location ~ref:iter
        GenericArrayBackedCollection.Iterator.internal_pointer
        ~obj:(new_index, Hist.add_event path event curr_index_hist)
        astate
    in
    let new_elem = AbstractValue.mk_fresh () in
    let<*> astate, arr = GenericArrayBackedCollection.eval path Read location iter astate in
    let<+> astate =
      PulseOperations.write_arr_index path location ~ref:arr ~index:curr_index
        ~obj:(new_elem, Hist.add_event path event curr_index_hist)
        astate
    in
    astate
end

module Resource = struct
  let allocate_aux ~exn_class_name ((this, _) as this_obj) delegation_opt : model =
   fun ({location; callee_procname; path; analysis_data= {tenv}} as model_data) astate ->
    let[@warning "-partial-match"] (Some (Typ.JavaClass class_name)) =
      Procname.get_class_type_name callee_procname
    in
    let allocator = Attribute.JavaResource class_name in
    let astate =
      PulseTaintOperations.taint_allocation tenv path location
        ~typ_desc:(Typ.Tstruct (Typ.JavaClass class_name)) ~alloc_desc:"Java resource"
        ~allocator:(Some allocator) this_obj astate
    in
    let post = PulseOperations.allocate allocator location this astate in
    let delegated_state =
      let<+> post =
        Option.value_map delegation_opt ~default:(Ok post) ~f:(fun obj ->
            write_field path PulseOperations.ModeledField.delegated_release obj location this_obj
              post )
      in
      post
    in
    let exn_state =
      Option.value_map exn_class_name ~default:[] ~f:(fun cn ->
          call_may_throw_exception (JavaClassName.from_string cn) model_data astate )
    in
    delegated_state @ exn_state


  let allocate ?exn_class_name this_arg : model = allocate_aux ~exn_class_name this_arg None

  let allocate_with_delegation ?exn_class_name () this_arg delegation : model =
    allocate_aux ~exn_class_name this_arg (Some delegation)


  let inputstream_resource_usage_modeled_throws_IOException =
    StringSet.of_list ["available"; "read"; "reset"; "skip"]


  let inputstream_resource_usage_modeled_do_not_throws = StringSet.of_list ["mark"; "markSupported"]

  let reader_resource_usage_modeled_throws_IOException =
    StringSet.of_list ["read"; "ready"; "reset"; "skip"]


  let reader_resource_usage_modeled_do_not_throws = StringSet.of_list ["mark"; "markSupported"]

  let writer_resource_usage_modeled = StringSet.of_list ["flush"; "write"]

  let use ~exn_class_name : model =
    let exn = JavaClassName.from_string exn_class_name in
    fun model_data astate ->
      Ok (ContinueProgram astate) :: call_may_throw_exception exn model_data astate


  let writer_append this : model =
    let exn = JavaClassName.from_string "java.IO.IOException" in
    fun model_data astate ->
      let return_this_state =
        Basic.id_first_arg ~desc:"java.io.PrintWriter.append" this model_data astate
      in
      let throw_IOException_state = call_may_throw_exception exn model_data astate in
      return_this_state @ throw_IOException_state


  let release this : model =
   fun _ astate ->
    PulseOperations.java_resource_release ~recursive:true (fst this) astate |> Basic.ok_continue


  let release_this_only this : model =
   fun _ astate ->
    PulseOperations.java_resource_release ~recursive:false (fst this) astate |> Basic.ok_continue
end

module Collection = struct
  let pkg_name = "java.util"

  let class_name = "Collection"

  let fst_field = mk_java_field pkg_name class_name "__infer_model_backing_collection_fst"

  let snd_field = mk_java_field pkg_name class_name "__infer_model_backing_collection_snd"

  let is_empty_field = mk_java_field pkg_name class_name "__infer_model_backing_collection_empty"

  let init ~desc this : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let fresh_val = (AbstractValue.mk_fresh (), Hist.single_event path event) in
    let is_empty_value = AbstractValue.mk_fresh () in
    let init_value = AbstractValue.mk_fresh () in
    (* The two internal fields are initially set to null *)
    let<*> astate =
      write_field path fst_field
        (init_value, Hist.single_event path event)
        location fresh_val astate
    in
    let<*> astate =
      write_field path snd_field
        (init_value, Hist.single_event path event)
        location fresh_val astate
    in
    (* The empty field is initially set to true *)
    let<*> astate =
      write_field path is_empty_field
        (is_empty_value, Hist.single_event path event)
        location fresh_val astate
    in
    let<**> astate =
      PulseOperations.write_deref path location ~ref:this ~obj:fresh_val astate
      >>>= PulseArithmetic.and_eq_int init_value IntLit.zero
      >>== PulseArithmetic.and_eq_int is_empty_value IntLit.one
    in
    astate |> Basic.ok_continue


  let add_common ~desc coll new_elem ?new_val : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let ret_value = AbstractValue.mk_fresh () in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    (* reads fst_field from collection *)
    let<*> astate, _, (fst_val, _) = load_field path fst_field location coll_val astate in
    (* mark the remove element as always reachable *)
    let astate = PulseOperations.always_reachable fst_val astate in
    (* in maps, every new value is marked as always reachable *)
    let astate =
      Option.value_map new_val ~default:astate ~f:(fun (obj, _) ->
          PulseOperations.always_reachable obj astate )
    in
    (* reads snd_field from collection *)
    let<*> astate, snd_addr, snd_val = load_field path snd_field location coll_val astate in
    (* fst_field takes value stored in snd_field *)
    let<*> astate = write_field path fst_field snd_val location coll_val astate in
    (* snd_field takes new value given *)
    let<*> astate = PulseOperations.write_deref path location ~ref:snd_addr ~obj:new_elem astate in
    (* Collection.add returns a boolean, in this case the return always has value one *)
    let<**> astate =
      PulseArithmetic.and_eq_int ret_value IntLit.one astate
      >>|| PulseOperations.write_id ret_id (ret_value, Hist.single_event path event)
    in
    (* empty field set to false if the collection was empty *)
    let<*> astate, _, (is_empty_val, hist) =
      load_field path is_empty_field location coll_val astate
    in
    if PulseArithmetic.is_known_zero astate is_empty_val then astate |> Basic.ok_continue
    else
      let is_empty_new_val = AbstractValue.mk_fresh () in
      let<**> astate =
        write_field path is_empty_field
          (is_empty_new_val, Hist.add_event path event hist)
          location coll_val astate
        >>>= PulseArithmetic.and_eq_int is_empty_new_val IntLit.zero
      in
      astate |> Basic.ok_continue


  let add ~desc coll new_elem : model = add_common ~desc coll new_elem

  let put ~desc coll new_elem new_val : model = add_common ~desc coll new_elem ~new_val

  let update path coll new_val new_val_hist event location ret_id astate =
    (* case0: element not present in collection *)
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, fst_val = load_field path fst_field location coll_val astate in
    let<*> astate, _, snd_val = load_field path snd_field location coll_val astate in
    let is_empty_val = AbstractValue.mk_fresh () in
    let<*> astate' =
      write_field path is_empty_field
        (is_empty_val, Hist.single_event path event)
        location coll_val astate
    in
    (* case1: fst_field is updated *)
    let astate1 =
      write_field path fst_field
        (new_val, Hist.add_event path event new_val_hist)
        location coll astate'
      >>| PulseOperations.write_id ret_id fst_val
      |> Basic.map_continue
    in
    (* case2: snd_field is updated *)
    let astate2 =
      write_field path snd_field
        (new_val, Hist.add_event path event new_val_hist)
        location coll astate'
      >>| PulseOperations.write_id ret_id snd_val
      |> Basic.map_continue
    in
    [Ok (Basic.continue astate); astate1; astate2]


  let set coll (new_val, new_val_hist) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "Collection.set()" in
    update path coll new_val new_val_hist event location ret_id astate


  let remove_at path ~desc coll location ret_id astate =
    let event = Hist.call_event path location desc in
    let new_val = AbstractValue.mk_fresh () in
    let<**> astate = PulseArithmetic.and_eq_int new_val IntLit.zero astate in
    update path coll new_val ValueHistory.epoch event location ret_id astate


  (* Auxiliary function that updates the state by:
     (1) assuming that value to be removed is equal to field value
     (2) assigning field to null value
     Collection.remove should return a boolean. In this case, the return val is one *)
  let remove_elem_found path coll_val elem field_addr field_val ret_id location event astate =
    let null_val = AbstractValue.mk_fresh () in
    let ret_val = AbstractValue.mk_fresh () in
    let is_empty_val = AbstractValue.mk_fresh () in
    let=* astate =
      write_field path is_empty_field
        (is_empty_val, Hist.single_event path event)
        location coll_val astate
    in
    let** astate =
      PulseArithmetic.and_eq_int null_val IntLit.zero astate
      >>== PulseArithmetic.and_eq_int ret_val IntLit.one
    in
    let+* astate =
      PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand elem)
        (AbstractValueOperand field_val) astate
    in
    let+ astate =
      PulseOperations.write_deref path location ~ref:field_addr
        ~obj:(null_val, Hist.single_event path event)
        astate
    in
    PulseOperations.write_id ret_id (ret_val, Hist.single_event path event) astate


  let remove_obj path ~desc coll (elem, _) location ret_id astate =
    let event = Hist.call_event path location desc in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, fst_addr, (fst_val, _) = load_field path fst_field location coll_val astate in
    let<*> astate, snd_addr, (snd_val, _) = load_field path snd_field location coll_val astate in
    (* case1: given element is equal to fst_field *)
    let astate1 =
      remove_elem_found path coll_val elem fst_addr fst_val ret_id location event astate
      >>|| ExecutionDomain.continue
    in
    (* case2: given element is equal to snd_field *)
    let astate2 =
      remove_elem_found path coll_val elem snd_addr snd_val ret_id location event astate
      >>|| ExecutionDomain.continue
    in
    (* case 3: given element is not equal to the fst AND not equal to the snd *)
    let astate3 =
      PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand elem)
        (AbstractValueOperand fst_val) astate
      >>== PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand elem)
             (AbstractValueOperand snd_val)
      >>|| ExecutionDomain.continue
    in
    SatUnsat.to_list astate1 @ SatUnsat.to_list astate2 @ SatUnsat.to_list astate3


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
        astate |> Basic.ok_continue


  let is_empty ~desc coll : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, (is_empty_val, hist) =
      load_field path is_empty_field location coll_val astate
    in
    PulseOperations.write_id ret_id (is_empty_val, Hist.add_event path event hist) astate
    |> Basic.ok_continue


  let clear ~desc coll : model =
   fun {path; location} astate ->
    let hist = Hist.single_call path location desc in
    let null_val = AbstractValue.mk_fresh () in
    let is_empty_val = AbstractValue.mk_fresh () in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate = write_field path fst_field (null_val, hist) location coll_val astate in
    let<*> astate = write_field path snd_field (null_val, hist) location coll_val astate in
    let<*> astate = write_field path is_empty_field (is_empty_val, hist) location coll_val astate in
    let<++> astate =
      PulseArithmetic.and_eq_int null_val IntLit.zero astate
      >>== PulseArithmetic.and_eq_int is_empty_val IntLit.one
    in
    astate


  (* Auxiliary function that changes the state by
     (1) assuming that internal is_empty field has value one
     (2) in such case we can return 0 *)
  let get_elem_coll_is_empty path is_empty_val is_empty_expected_val event location ret_id astate =
    let not_found_val = AbstractValue.mk_fresh () in
    let++ astate =
      PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand is_empty_val)
        (AbstractValueOperand is_empty_expected_val) astate
      >>== PulseArithmetic.and_eq_int not_found_val IntLit.zero
      >>== PulseArithmetic.and_eq_int is_empty_expected_val IntLit.one
    in
    let hist = Hist.single_event path event in
    let astate = PulseOperations.write_id ret_id (not_found_val, hist) astate in
    PulseOperations.invalidate path
      (StackAddress (Var.of_id ret_id, hist))
      location (ConstantDereference IntLit.zero)
      (not_found_val, Hist.single_event path event)
      astate


  (* Auxiliary function that splits the state into three, considering the case that
     the internal is_empty field is not known to have value 1 *)
  let get_elem_coll_not_known_empty elem found_val fst_val snd_val astate =
    (* case 1: given element is not equal to the fst AND not equal to the snd *)
    let astate1 =
      PulseArithmetic.prune_binop ~negated:true Eq (AbstractValueOperand elem)
        (AbstractValueOperand fst_val) astate
      >>== PulseArithmetic.prune_binop ~negated:true Eq (AbstractValueOperand elem)
             (AbstractValueOperand snd_val)
      >>|| ExecutionDomain.continue
    in
    (* case 2: given element is equal to fst_field *)
    let astate2 =
      PulseArithmetic.and_positive found_val astate
      >>== PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand elem)
             (AbstractValueOperand fst_val)
      >>|| ExecutionDomain.continue
    in
    (* case 3: given element is equal to snd_field *)
    let astate3 =
      PulseArithmetic.and_positive found_val astate
      >>== PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand elem)
             (AbstractValueOperand snd_val)
      >>|| ExecutionDomain.continue
    in
    SatUnsat.to_list astate1 @ SatUnsat.to_list astate2 @ SatUnsat.to_list astate3


  let get ~desc coll (elem, _) : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, coll_val =
      PulseOperations.eval_access path Read location coll Dereference astate
    in
    let<*> astate, _, (is_empty_val, _) = load_field path is_empty_field location coll_val astate in
    (* case 1: collection is empty *)
    let true_val = AbstractValue.mk_fresh () in
    let astate1 =
      get_elem_coll_is_empty path is_empty_val true_val event location ret_id astate
      >>|| ExecutionDomain.continue
    in
    (* case 2: collection is not known to be empty *)
    let found_val = AbstractValue.mk_fresh () in
    let astates2 =
      let<*> astate2, _, (fst_val, _) = load_field path fst_field location coll_val astate in
      let<*> astate2, _, (snd_val, _) = load_field path snd_field location coll_val astate2 in
      let<**> astate2 =
        PulseArithmetic.prune_binop ~negated:true Binop.Eq (AbstractValueOperand is_empty_val)
          (AbstractValueOperand true_val) astate2
        >>== PulseArithmetic.and_eq_int true_val IntLit.one
        >>|| PulseOperations.write_id ret_id (found_val, Hist.single_event path event)
      in
      get_elem_coll_not_known_empty elem found_val fst_val snd_val astate2
    in
    SatUnsat.to_list astate1 @ astates2
end

module Integer = struct
  let internal_int = mk_java_field "java.lang" "Integer" "__infer_model_backing_int"

  let load_backing_int path location this astate =
    let* astate, obj = PulseOperations.eval_access path Read location this Dereference astate in
    load_field path internal_int location obj astate


  let construct path this_address init_value event location astate =
    let this = (AbstractValue.mk_fresh (), Hist.single_event path event) in
    let* astate, int_field =
      PulseOperations.eval_access path Write location this (FieldAccess internal_int) astate
    in
    let* astate = PulseOperations.write_deref path location ~ref:int_field ~obj:init_value astate in
    PulseOperations.write_deref path location ~ref:this_address ~obj:this astate


  let init this_address init_value : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location "Integer.init" in
    let<+> astate = construct path this_address init_value event location astate in
    astate


  let equals this arg : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, _int_addr1, (int1, hist1) = load_backing_int path location this astate in
    let<*> astate, _int_addr2, (int2, hist2) = load_backing_int path location arg astate in
    let binop_addr = AbstractValue.mk_fresh () in
    let<++> astate, binop_addr =
      PulseArithmetic.eval_binop binop_addr Eq (AbstractValueOperand int1)
        (AbstractValueOperand int2) astate
    in
    PulseOperations.write_id ret_id (binop_addr, Hist.binop path Eq hist1 hist2) astate


  let int_val this : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, _int_addr1, (int_value, hist) = load_backing_int path location this astate in
    PulseOperations.write_id ret_id (int_value, Hist.hist path hist) astate |> Basic.ok_continue


  let value_of init_value : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "Integer.valueOf" in
    let new_alloc = (AbstractValue.mk_fresh (), Hist.single_event path event) in
    let<+> astate = construct path new_alloc init_value event location astate in
    PulseOperations.write_id ret_id new_alloc astate
end

module Preconditions = struct
  let check_not_null (address, hist) : model =
   fun {location; path; ret= ret_id, _} astate ->
    let event = Hist.call_event path location "Preconditions.checkNotNull" in
    let<++> astate = PulseArithmetic.prune_positive address astate in
    PulseOperations.write_id ret_id (address, Hist.add_event path event hist) astate


  let check_state_argument (address, _) : model =
   fun _ astate ->
    let<++> astate = PulseArithmetic.prune_positive address astate in
    astate
end

let non_static_method name1 (_, procname) name2 =
  (not (Procname.is_java_static_method procname)) && String.equal name1 name2


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let pushback_modeled =
    StringSet.of_list
      ["add"; "addAll"; "append"; "delete"; "remove"; "replace"; "poll"; "put"; "putAll"]
  in
  let cpp_push_back_without_desc vector : model =
   fun ({callee_procname} as model_data) astate ->
    Cplusplus.Vector.push_back vector ~desc:(Procname.to_string callee_procname) model_data astate
  in
  let map_context_tenv f (x, _) = f x in
  [ +BuiltinDecl.(match_builtin __java_throw) <>--> throw
  ; +BuiltinDecl.(match_builtin __unwrap_exception)
    <>$ capt_arg_payload
    $--> Basic.id_first_arg ~desc:"unwrap_exception"
  ; +BuiltinDecl.(match_builtin __set_file_attribute) <>$ any_arg $--> Basic.skip
  ; +BuiltinDecl.(match_builtin __set_mem_attribute) <>$ any_arg $--> Basic.skip
  ; +map_context_tenv
       (PatternMatch.Java.implements_one_of
          ["java.io.FileInputStream"; "java.io.FileOutputStream"] )
    &:: "<init>" <>$ capt_arg_payload
    $+...$--> Resource.allocate ~exn_class_name:"java.io.FileNotFoundException"
  ; +map_context_tenv
       (PatternMatch.Java.implements_one_of
          [ "java.io.ObjectInputStream"
          ; "java.io.ObjectOutputStream"
          ; "java.util.jar.JarInputStream"
          ; "java.util.jar.JarOutputStream"
          ; "java.util.zip.GZIPInputStream"
          ; "java.util.zip.GZIPOutputStream" ] )
    &:: "<init>" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation ~exn_class_name:"java.io.IOException" ()
  ; +map_context_tenv (* <init> that does not throw exceptions *)
       (PatternMatch.Java.implements_one_of
          [ "java.io.BufferedInputStream"
          ; "java.io.BufferedOutputStream"
          ; "java.io.DataInputStream"
          ; "java.io.DataOutputStream"
          ; "java.io.FilterInputStream"
          ; "java.io.FilterOutputStream"
          ; "java.io.PushbackInputStream"
          ; "java.io.Reader"
          ; "java.io.Writer"
          ; "java.security.DigestInputStream"
          ; "java.security.DigestOutputStream"
          ; "java.util.Scanner"
          ; "java.util.zip.CheckedInputStream"
          ; "java.util.zip.CheckedOutputStream"
          ; "java.util.zip.DeflaterInputStream"
          ; "java.util.zip.DeflaterOutputStream"
          ; "java.util.zip.InflaterInputStream"
          ; "java.util.zip.InflaterOutputStream"
          ; "javax.crypto.CipherInputStream"
          ; "javax.crypto.CipherOutputStream" ] )
    &:: "<init>" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation ()
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.OutputStream")
    &::+ non_static_method "write" <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"java.io.IOException"
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.OutputStream")
    &:: "flush" <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"java.io.IOException"
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.InputStream")
    &::+ (fun _ proc_name_str ->
           StringSet.mem proc_name_str
             Resource.inputstream_resource_usage_modeled_throws_IOException )
    <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"java.io.IOException"
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.InputStream")
    &::+ (fun _ proc_name_str ->
           StringSet.mem proc_name_str Resource.inputstream_resource_usage_modeled_do_not_throws )
    <>$ any_arg $+...$--> Basic.skip
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.Reader")
    &::+ (fun _ proc_name_str ->
           StringSet.mem proc_name_str Resource.reader_resource_usage_modeled_throws_IOException )
    <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"java.io.IOException"
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.Reader")
    &::+ (fun _ proc_name_str ->
           StringSet.mem proc_name_str Resource.reader_resource_usage_modeled_do_not_throws )
    <>$ any_arg $+...$--> Basic.skip
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.Writer")
    &::+ (fun _ proc_name_str -> StringSet.mem proc_name_str Resource.writer_resource_usage_modeled)
    <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"java.io.IOException"
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.Writer")
    &:: "append" <>$ capt_arg_payload $+...$--> Resource.writer_append
  ; +map_context_tenv (PatternMatch.Java.implements "java.io.Closeable")
    &:: "close" <>$ capt_arg_payload $--> Resource.release
  ; +map_context_tenv (PatternMatch.Java.implements "com.google.common.io.Closeables")
    &:: "close" <>$ capt_arg_payload $+...$--> Resource.release
  ; +map_context_tenv (PatternMatch.Java.implements "com.google.common.io.Closeables")
    &:: "closeQuietly" <>$ capt_arg_payload $+...$--> Resource.release
  ; +map_context_tenv (PatternMatch.Java.implements "java.util.zip.DeflaterOutputStream")
    &:: "finish" <>$ capt_arg_payload $+...$--> Resource.release_this_only
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Object")
    &:: "clone" $ capt_arg_payload $--> Object.clone
  ; ( +map_context_tenv (PatternMatch.Java.implements_lang "System")
    &:: "arraycopy" $ capt_arg_payload $+ any_arg $+ capt_arg_payload
    $+...$--> fun src dest -> Basic.shallow_copy_model "System.arraycopy" dest src )
  ; +map_context_tenv (PatternMatch.Java.implements_lang "System") &:: "exit" <>--> Basic.early_exit
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "<init>" <>$ capt_arg_payload
    $--> Collection.init ~desc:"Collection.init()"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "add" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Collection.add ~desc:"Collection.add"
  ; +map_context_tenv PatternMatch.Java.implements_list
    &:: "add" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
    $--> Collection.add ~desc:"Collection.add()"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "remove"
    &++> Collection.remove ~desc:"Collection.remove"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "isEmpty" <>$ capt_arg_payload
    $--> Collection.is_empty ~desc:"Collection.isEmpty()"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "clear" <>$ capt_arg_payload
    $--> Collection.clear ~desc:"Collection.clear()"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &::+ (fun _ proc_name_str -> StringSet.mem proc_name_str pushback_modeled)
    <>$ capt_arg_payload $+...$--> cpp_push_back_without_desc
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "<init>" <>$ capt_arg_payload
    $--> Collection.init ~desc:"Map.init()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "put" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> Collection.put ~desc:"Map.put()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "remove"
    &++> Collection.remove ~desc:"Map.remove()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "get" <>$ capt_arg_payload $+ capt_arg_payload $--> Collection.get ~desc:"Map.get()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "containsKey" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Collection.get ~desc:"Map.containsKey()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "isEmpty" <>$ capt_arg_payload
    $--> Collection.is_empty ~desc:"Map.isEmpty()"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "clear" <>$ capt_arg_payload
    $--> Collection.clear ~desc:"Map.clear()"
  ; +map_context_tenv PatternMatch.Java.implements_queue
    &::+ (fun _ proc_name_str -> StringSet.mem proc_name_str pushback_modeled)
    <>$ capt_arg_payload $+...$--> cpp_push_back_without_desc
  ; +map_context_tenv (PatternMatch.Java.implements_lang "StringBuilder")
    &::+ (fun _ proc_name_str -> StringSet.mem proc_name_str pushback_modeled)
    <>$ capt_arg_payload $+...$--> cpp_push_back_without_desc
  ; +map_context_tenv (PatternMatch.Java.implements_lang "StringBuilder")
    &:: "setLength" <>$ capt_arg_payload
    $+...$--> Cplusplus.Vector.invalidate_references ShrinkToFit
  ; +map_context_tenv (PatternMatch.Java.implements_lang "String")
    &::+ (fun _ proc_name_str -> StringSet.mem proc_name_str pushback_modeled)
    <>$ capt_arg_payload $+...$--> cpp_push_back_without_desc
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
    &:: "<init>" $ capt_arg_payload $+ capt_arg_payload $--> Integer.init
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
    &:: "equals" $ capt_arg_payload $+ capt_arg_payload $--> Integer.equals
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
    &:: "intValue" <>$ capt_arg_payload $--> Integer.int_val
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Integer")
    &:: "valueOf" <>$ capt_arg_payload $--> Integer.value_of
  ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
    &:: "checkNotNull" $ capt_arg_payload $+...$--> Preconditions.check_not_null
  ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
    &:: "checkState" $ capt_arg_payload $+...$--> Preconditions.check_state_argument
  ; +map_context_tenv (PatternMatch.Java.implements_google "common.base.Preconditions")
    &:: "checkArgument" $ capt_arg_payload $+...$--> Preconditions.check_state_argument
  ; +map_context_tenv PatternMatch.Java.implements_iterator
    &:: "remove" <>$ capt_arg_payload $+...$--> Iterator.remove ~desc:"remove"
  ; +map_context_tenv PatternMatch.Java.implements_map
    &:: "putAll" <>$ capt_arg_payload $+...$--> cpp_push_back_without_desc
  ; -"std" &:: "vector" &:: "reserve" <>$ capt_arg_payload $+...$--> Cplusplus.Vector.reserve
  ; -"std" &:: "vector" &:: "size" &--> Basic.nondet ~desc:"std::vector::size"
  ; +map_context_tenv PatternMatch.Java.implements_collection
    &:: "get" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Cplusplus.Vector.at ~desc:"Collection.get()"
  ; +map_context_tenv PatternMatch.Java.implements_list
    &:: "set" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload $--> Collection.set
  ; +map_context_tenv PatternMatch.Java.implements_iterator
    &:: "hasNext"
    &--> Basic.nondet ~desc:"Iterator.hasNext()"
  ; +map_context_tenv PatternMatch.Java.implements_enumeration
    &:: "hasMoreElements"
    &--> Basic.nondet ~desc:"Enumeration.hasMoreElements()"
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Object")
    &:: "equals"
    &--> Basic.nondet ~desc:"Object.equals"
  ; +map_context_tenv (PatternMatch.Java.implements_lang "Iterable")
    &:: "iterator" <>$ capt_arg_payload
    $+...$--> Iterator.constructor ~desc:"Iterable.iterator"
  ; +map_context_tenv PatternMatch.Java.implements_iterator
    &:: "next" <>$ capt_arg_payload
    $!--> Iterator.next ~desc:"Iterator.next()"
  ; +BuiltinDecl.(match_builtin __instanceof) <>$ capt_arg_payload $+ capt_exp $--> instance_of
  ; ( +map_context_tenv PatternMatch.Java.implements_enumeration
    &:: "nextElement" <>$ capt_arg_payload
    $!--> fun x ->
    Cplusplus.Vector.at ~desc:"Enumeration.nextElement" x (AbstractValue.mk_fresh (), []) ) ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValuePath.addr_hist)
