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
module StringSet = Caml.Set.Make (String)

let mk_csharp_field namespace clazz field =
  Fieldname.make
    (Typ.CSharpClass (CSharpClassName.make ~namespace:(Some namespace) ~classname:clazz))
    field


let write_field path field new_val location addr astate =
  let* astate, field_addr =
    PulseOperations.eval_access path Write location addr (FieldAccess field) astate
  in
  PulseOperations.write_deref path location ~ref:field_addr ~obj:new_val astate


let load_field path field location obj astate =
  let* astate, field_addr =
    PulseOperations.eval_access path Read location obj (FieldAccess field) astate
  in
  let+ astate, field_val =
    PulseOperations.eval_access path Read location field_addr Dereference astate
  in
  (astate, field_addr, field_val)


let call_may_throw_exception (exn : CSharpClassName.t) : model_no_non_disj =
 fun {location; path; analysis_data} astate ->
  let ret_addr = AbstractValue.mk_fresh () in
  let exn_name = CSharpClassName.to_string exn in
  let desc = Printf.sprintf "throw_%s" exn_name in
  let ret_alloc_hist = Hist.single_alloc path location desc in
  let typ = Typ.mk_struct (Typ.CSharpClass exn) in
  let astate = PulseArithmetic.and_dynamic_type_is_unsafe ret_addr typ location astate in
  let ret_var = Procdesc.get_ret_var analysis_data.proc_desc in
  let astate, ref = PulseOperations.eval_var path location ret_var astate in
  let obj = (ret_addr, ret_alloc_hist) in
  let<*> astate = PulseOperations.write_deref path location ~ref ~obj astate in
  [Ok (ExceptionRaised astate)]


(* let throw : model = fun _ astate -> [Ok (ExceptionRaised astate)] *)

module Collection = struct
  let namespace_name = "System.Collections.Generic"

  let class_name = "ICollection"

  let fst_field = mk_csharp_field namespace_name class_name "__infer_model_backing_collection_fst"

  let snd_field = mk_csharp_field namespace_name class_name "__infer_model_backing_collection_snd"

  let is_empty_field =
    mk_csharp_field namespace_name class_name "__infer_model_backing_collection_empty"


  let init ~desc this : model_no_non_disj =
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


  let add_common ~desc coll new_elem ?new_val : model_no_non_disj =
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


  let add ~desc coll new_elem : model_no_non_disj = add_common ~desc coll new_elem

  let put ~desc coll new_elem new_val : model_no_non_disj = add_common ~desc coll new_elem ~new_val

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


  let _set coll (new_val, new_val_hist) : model_no_non_disj =
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


  let remove ~desc args : model_no_non_disj =
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


  let is_empty ~desc coll : model_no_non_disj =
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


  let clear ~desc coll : model_no_non_disj =
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


  let get ~desc coll (elem, _) : model_no_non_disj =
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

module Resource = struct
  let allocate_state (this, _) {location; callee_procname} astate : AbductiveDomain.t =
    let[@warning "-partial-match"] (Some (Typ.CSharpClass class_name)) =
      Procname.get_class_type_name callee_procname
    in
    let allocator = Attribute.CSharpResource class_name in
    let post = PulseOperations.allocate allocator location this astate in
    post


  let allocate_aux ~exn_class_name this_obj delegation_opt : model_no_non_disj =
   fun ({location; path} as model_data) astate ->
    let post = allocate_state this_obj model_data astate in
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
          call_may_throw_exception (CSharpClassName.from_string cn) model_data astate )
    in
    delegated_state @ exn_state


  let _allocate ~exn_class_name this_arg : model_no_non_disj =
    allocate_aux ~exn_class_name this_arg None


  let allocate_with_delegation ~exn_class_name () this_arg delegation : model_no_non_disj =
    allocate_aux ~exn_class_name this_arg (Some delegation)


  let _update_result_ok_state ~(f : AbductiveDomain.t -> AbductiveDomain.t)
      (results : ExecutionDomain.t AccessResult.t list) : ExecutionDomain.t AccessResult.t list =
    List.map results
      ~f:
        (PulseResult.map ~f:(function
          | ContinueProgram astate ->
              ContinueProgram (f astate)
          | result ->
              result ) )


  (* I think this function needs to match all the cases *)

  let model_with_analysis args {analysis_data; path; callee_procname; location; ret} astate non_disj
      =
    let actuals =
      List.map args ~f:(fun ProcnameDispatcher.Call.FuncArg.{arg_payload; typ} ->
          (arg_payload, typ) )
    in
    let res, non_disj, _, is_known_call =
      PulseCallOperations.call analysis_data path location callee_procname ~ret ~actuals
        ~formals_opt:None ~call_kind:`ResolvedProcname astate non_disj
    in
    match is_known_call with
    | `KnownCall ->
        (* if we have what we need for a callee match, use it *)
        (res, non_disj)
    | `UnknownCall ->
        (Basic.ok_continue astate, non_disj (* is this too generic? *))


  let allocate_with_analysis
      (ProcnameDispatcher.Call.FuncArg.{arg_payload= this_arg_payload} as this_arg) arguments :
      model =
   fun model_data astate non_disj ->
    (* this (probably) marks the this_arg as allocated, and is passed to the calls *)
    let allocated_astate = allocate_state this_arg_payload model_data astate in
    model_with_analysis (this_arg :: arguments) model_data allocated_astate non_disj


  (* Doesn't use allocate_aux, but given the parameters allocate_aux would have been given,
     it reduces to the same thing. *)

  (* this doesn't even check if the resource is allocated!? *)
  let use ~exn_class_name : model_no_non_disj =
    let exn = CSharpClassName.from_string exn_class_name in
    fun model_data astate ->
      Ok (ContinueProgram astate) :: call_may_throw_exception exn model_data astate


  let _release ~exn_class_name this : model_no_non_disj =
   fun model_data astate ->
    let ok_state =
      PulseOperations.csharp_resource_release ~recursive:true (fst this) astate |> Basic.ok_continue
    in
    let exn_state =
      Option.value_map exn_class_name ~default:[] ~f:(fun cn ->
          call_may_throw_exception (CSharpClassName.from_string cn) model_data astate )
    in
    ok_state @ exn_state


  let release_with_analysis
      (ProcnameDispatcher.Call.FuncArg.{arg_payload= this_arg_payload} as this_arg) : model =
   fun model_data astate non_disj ->
    let released_astate =
      PulseOperations.csharp_resource_release ~recursive:true (fst this_arg_payload) astate
    in
    model_with_analysis [this_arg] model_data released_astate non_disj


  let _release_this_only this : model_no_non_disj =
   fun _ astate ->
    PulseOperations.csharp_resource_release ~recursive:false (fst this) astate |> Basic.ok_continue
end

let string_length_access = Access.FieldAccess PulseOperations.ModeledField.string_length

let string_is_null_or_whitespace ~desc ((addr, hist) as addr_hist) : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let ret_val = AbstractValue.mk_fresh () in
  let astate_null =
    PulseArithmetic.prune_eq_zero addr astate
    >>== PulseArithmetic.and_eq_int ret_val IntLit.one
    >>|| PulseOperations.write_id ret_id (ret_val, Hist.add_event path event hist)
    >>|| ExecutionDomain.continue |> SatUnsat.to_list
  in
  let astate_not_null =
    let<**> astate = PulseArithmetic.prune_positive addr astate in
    let<*> astate, (len_addr, hist) =
      PulseOperations.eval_access path Read location addr_hist string_length_access astate
    in
    let astate = PulseOperations.write_id ret_id (ret_val, Hist.add_event path event hist) astate in
    let astate_empty =
      PulseArithmetic.prune_eq_zero len_addr astate
      >>== PulseArithmetic.and_eq_int ret_val IntLit.one
      >>|| ExecutionDomain.continue
    in
    let astate_not_empty =
      PulseArithmetic.prune_positive len_addr astate
      >>== PulseArithmetic.and_eq_int ret_val IntLit.zero
      >>|| ExecutionDomain.continue
    in
    [astate_empty; astate_not_empty] |> SatUnsat.filter
  in
  astate_null @ astate_not_null


let iDisposablesIgnore =
  ["System.IO.MemoryStream"; "System.IO.StringReader"; "System.IO.StringWriter"]


let input_resource_usage_modeled = StringSet.of_list ["WriteByte"; "Write"; "Read"]

let implements_dictionary tenv name =
  let collection_type = "System.Collections.IDictionary" in
  PatternMatch.CSharp.implements collection_type tenv name


let implements_collection tenv name =
  let collection_type = "System.Collections.Generic.ICollection`1<!0>" in
  PatternMatch.CSharp.implements collection_type tenv name


let implements_disposable_not_enumerator tenv name =
  PatternMatch.CSharp.implements "System.IDisposable" tenv name
  && not (PatternMatch.CSharp.implements "System.Collections.Generic.IEnumerator`1<!0>" tenv name)


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let map_context_tenv f (x, _) = f x in
  [ +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrWhiteSpace" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrWhiteSpace"
    |> with_non_disj
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrEmpty" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrEmpty"
    |> with_non_disj
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.Diagnostics.Debug")
    &:: "Assert" <>$ capt_arg $--> Basic.assert_ |> with_non_disj
    (* IDisposables that take care of passed resource (stream) *)
  ; +map_context_tenv
       (PatternMatch.CSharp.implements_one_of
          [ "System.IO.StreamReader"
          ; "System.IO.StreamWriter"
          ; "System.IO.BinaryReader"
          ; "System.IO.BinaryWriter"
          ; "System.Net.Http.HttpClient" ] )
    &:: ".ctor" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation ~exn_class_name:None ()
    |> with_non_disj
    (* Things that may throw an exception *)
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.Net.Sockets.NetworkStream")
    &:: ".ctor" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Resource.allocate_with_delegation ~exn_class_name:(Some "System.IO.IOException") ()
    |> with_non_disj
    (* If a stream function is used, like Read, model the possibility for an exception *)
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IO.Stream")
    &::+ (fun _ function_name -> StringSet.mem function_name input_resource_usage_modeled)
    <>$ any_arg
    $+...$--> Resource.use ~exn_class_name:"System.IO.IOException"
    |> with_non_disj
    (* Some IDisposables that don't _need_ to be disposed, so we treat them as nops *)
  ; +map_context_tenv (PatternMatch.CSharp.implements_one_of iDisposablesIgnore)
    &:: ".ctor" <>$ any_arg $+...$--> Basic.skip |> with_non_disj
    (* Base case for IDisposables:
        model the allocation and deallocation and if code exists analyze it.
        we don't model enumerators, as they are an exception to the general IDisposable rule,
          instead we want to analyze the code for the enumerator *)
  ; +map_context_tenv implements_disposable_not_enumerator
    &:: ".ctor" <>$ capt_arg $++$--> Resource.allocate_with_analysis
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IDisposable")
    &:: "Close" <>$ capt_arg $--> Resource.release_with_analysis
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IDisposable")
    &:: "Dispose" <>$ capt_arg $--> Resource.release_with_analysis
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.IAsyncDisposable")
    &:: "DisposeAsync" <>$ capt_arg $--> Resource.release_with_analysis
    (* Models for collections and dictionaries *)
  ; +map_context_tenv implements_collection
    &:: ".ctor" $ capt_arg_payload
    $--> Collection.init ~desc:"Collection..ctor()"
    |> with_non_disj
  ; +map_context_tenv implements_dictionary
    &:: "Add" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> Collection.put ~desc:"Dictionary.Add()"
    |> with_non_disj
  ; +map_context_tenv implements_dictionary
    &:: "get_Item" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Collection.get ~desc:"Dictionary.[]"
    |> with_non_disj
  ; +map_context_tenv implements_collection
    &:: "Add" $ capt_arg_payload $+ capt_arg_payload
    $--> Collection.add ~desc:"Collection.Add"
    |> with_non_disj
  ; +map_context_tenv implements_collection
    &:: "Remove"
    &++> Collection.remove ~desc:"Collection.Remove"
    |> with_non_disj
  ; +map_context_tenv implements_collection
    &:: "IsEmpty" <>$ capt_arg_payload
    $--> Collection.is_empty ~desc:"Collection.IsEmpty()"
    |> with_non_disj
  ; +map_context_tenv implements_collection
    &:: "Clear" $ capt_arg_payload
    $--> Collection.clear ~desc:"Collection.Clear()"
    |> with_non_disj
    (* This is needed for constructors that call out to the base constructor for objects. *)
    (* In particular, it was added for analyzing the constructors of IDisposables,
       otherwise the allocation of the IDisposable is lost at the call to the Object..ctor *)
  ; +map_context_tenv (fun _ -> String.equal "System.Object")
    &:: ".ctor" <>$ any_arg $--> Basic.skip |> with_non_disj ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
