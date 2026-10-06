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

let field = Fieldname.make PulseOperations.pulse_model_type "__infer_backing_array"

let last_field = Fieldname.make PulseOperations.pulse_model_type "__infer_past_the_end"

let size_field = Fieldname.make PulseOperations.pulse_model_type "__infer_size"

let size_access = Access.FieldAccess size_field

let to_internal_size path mode location value astate =
  PulseOperations.eval_access path mode location value size_access astate


let to_internal_size_deref path mode location value astate =
  let* astate, pointer = to_internal_size path Read location value astate in
  PulseOperations.eval_access path mode location pointer Dereference astate


let assign_size_constant path location this ~constant ~desc astate =
  let value = (AbstractValue.mk_fresh (), Hist.single_call path location desc) in
  let=* astate, size_pointer = to_internal_size path Read location this astate in
  let=* astate = PulseOperations.write_deref path location ~ref:size_pointer ~obj:value astate in
  PulseArithmetic.and_eq_int (fst value) constant astate


let assign_size path location this (size, size_hist) ~desc astate =
  let* astate, size_pointer = to_internal_size path Read location this astate in
  PulseOperations.write_deref path location ~ref:size_pointer
    ~obj:(size, Hist.add_call path location desc size_hist)
    astate


let access = Access.FieldAccess field

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
    PulseOperations.eval_deref_access path Write location collection (FieldAccess last_field) astate
  in
  let astate = AddressAttributes.mark_as_end_of_collection (fst pointer) astate in
  (astate, pointer)


let binop_size path location this ~desc binop astate =
  let new_size = AbstractValue.mk_fresh () in
  let=* astate, (size_addr, hist) = to_internal_size path Read location this astate in
  let=* astate, (size_value, _) = to_internal_size_deref path Read location this astate in
  let hist = Hist.add_call path location desc hist in
  let+* astate, new_size =
    PulseArithmetic.eval_binop new_size binop (AbstractValueOperand size_value)
      (ConstOperand (Cint (IntLit.of_int 1)))
      astate
  in
  (* update the size count *)
  PulseOperations.write_deref path location ~ref:(size_addr, hist) ~obj:(new_size, hist) astate


let increase_size path location this ~desc astate =
  binop_size path location this ~desc (PlusA None) astate


let decrease_size path location this ~desc astate =
  binop_size path location this ~desc (MinusA None) astate


let default_constructor this ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<++> astate = assign_size_constant path location this ~constant:IntLit.zero ~desc astate in
  astate


let empty this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let ret_addr = AbstractValue.mk_fresh () in
  let<*> astate, (value_addr, _) = to_internal_size_deref path Read location this astate in
  let result_non_empty =
    PulseArithmetic.prune_positive value_addr astate
    >>== PulseArithmetic.prune_eq_zero ret_addr
    >>|| PulseOperations.write_id ret_id
           (ret_addr, Hist.single_call path location ~more:"non-empty case" desc)
    >>|| ExecutionDomain.continue
  in
  let result_empty =
    PulseArithmetic.prune_eq_zero value_addr astate
    >>== PulseArithmetic.prune_positive ret_addr
    >>|| PulseOperations.write_id ret_id
           (ret_addr, Hist.single_call path location ~more:"empty case" desc)
    >>|| ExecutionDomain.continue
  in
  SatUnsat.to_list result_non_empty @ SatUnsat.to_list result_empty


let size this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (value_addr, value_hist) = to_internal_size_deref path Read location this astate in
  PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


module Iterator = struct
  let internal_pointer = Fieldname.make PulseOperations.pulse_model_type "__infer_backing_pointer"

  let internal_pointer_access = Access.FieldAccess internal_pointer

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
    let is_end =
      AddressAttributes.is_end_of_collection (fst pointer) astate
      || AddressAttributes.is_end_of_collection (fst index) astate
    in
    let* astate =
      if is_end && not is_minus_minus then
        let invalidation_trace = Trace.Immediate {location; history= ValueHistory.epoch} in
        let access_trace = Trace.Immediate {location; history= snd pointer} in
        FatalError
          ( ReportableError
              { diagnostic=
                  Diagnostic.AccessToInvalidAddress
                    { calling_context= []
                    ; invalid_address= Decompiler.find (fst pointer) astate
                    ; invalidation= EndIterator
                    ; invalidation_trace
                    ; access_trace
                    ; may_depend_on_an_unknown_value= astate.AbductiveDomain.unknown_values
                    ; must_be_valid_reason= None }
              ; astate }
          , [] )
      else Ok astate
    in
    (* We do not want to create internal array if iterator pointer has an invalid value *)
    let* astate = PulseOperations.check_addr_access path Read location index astate in
    let+ astate, elem = element path location iterator (fst index) astate in
    (astate, pointer, index, elem)


  let construct path location event ~init ~ref astate =
    let* astate, (arr_addr, arr_hist) = eval path Read location init astate in
    let* astate =
      PulseOperations.write_deref_field path location ~ref field
        ~obj:(arr_addr, Hist.add_event event arr_hist)
        astate
    in
    let* astate, (p_addr, p_hist) = to_internal_pointer path Read location init astate in
    PulseOperations.write_field path location ~ref internal_pointer
      ~obj:(p_addr, Hist.add_event event p_hist)
      astate


  let constructor ~desc this init : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = construct path location event ~init ~ref:this astate in
    astate


  let write_position path location ~iter ~arr ~position pointer_hist astate =
    let pointer = (AbstractValue.mk_fresh (), pointer_hist) in
    PulseOperations.write_deref_field path location ~ref:iter field ~obj:(arr, pointer_hist) astate
    >>= PulseOperations.write_field path location ~ref:iter internal_pointer ~obj:pointer
    >>= PulseOperations.write_deref path location ~ref:pointer ~obj:position


  let point_into path location event ~collection ~iter ?index astate =
    let pointer_hist = Hist.add_event event (snd iter) in
    let* astate, (arr_addr, _arr_hist) = eval path Read location collection astate in
    let* astate, position =
      match index with
      | Some index ->
          eval_element path location (arr_addr, pointer_hist) index astate
      | None ->
          (* as in [operator_step], an array element at a fresh index would be added to the
             pre-condition of every caller when the collection is a parameter *)
          Ok (astate, (AbstractValue.mk_fresh (), pointer_hist))
    in
    write_position path location ~iter ~arr:arr_addr ~position pointer_hist astate


  (* string iterators carry the taint of their string, see [BasicString.iterator_common] in
     [PulseModelsCpp] *)
  let propagate_taint ~src ~dst astate =
    if AbstractValue.equal (fst src) (fst dst) then astate
    else
      AddressAttributes.add_one (fst dst)
        (PropagateTaintFrom (InternalModel, [{v= fst src; history= snd src}]))
        astate


  (** copy the fields of the library's iterator class from [src] to [dst], shifted by [offset] if
      given, for models that replace its functions, e.g. for [base()] *)
  let write_own_fields tenv path location event iterator_class ?offset ~src ~dst astate =
    let fields =
      Option.bind iterator_class ~f:(Tenv.lookup tenv)
      |> Option.value_map ~default:[] ~f:(fun {Struct.fields} -> fields)
    in
    PulseOperationResult.list_fold fields ~init:astate ~f:(fun astate {Struct.name= field; typ} ->
        match typ.Typ.desc with
        | Tstruct _ ->
            (* not needed for the iterator classes modelled here, which wrap a pointer *)
            SatUnsat.Sat (Ok astate)
        | _ ->
            let=* astate, (value, hist) =
              PulseOperations.eval_deref_access path Read location src (FieldAccess field) astate
            in
            let+* astate, value =
              match offset with
              | None ->
                  SatUnsat.Sat (Ok (astate, value))
              | Some (binop, n) ->
                  PulseArithmetic.eval_binop (AbstractValue.mk_fresh ()) binop
                    (AbstractValueOperand value) n astate
            in
            PulseOperations.write_deref_field path location ~ref:dst field
              ~obj:(value, Hist.add_event event hist)
              astate )


  let assign ~desc this other : model_no_non_disj =
   fun {analysis_data= {tenv}; path; location; callee_procname; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate = construct path location event ~init:other ~ref:this astate in
    let<++> astate =
      write_own_fields tenv path location event
        (Procname.get_class_type_name callee_procname)
        ~src:other ~dst:this astate
    in
    PulseOperations.write_id ret_id this astate


  let class_of_arg {FuncArg.typ} =
    match typ.Typ.desc with Tptr (typ, _) -> Typ.name typ | _ -> Typ.name typ


  (** make [dst] point [n] elements after ([PlusPI]) or before ([MinusPI]) [src] *)
  let apply_offset {analysis_data= {tenv}; path; location} ~desc binop
      (FuncArg.{arg_payload= src} as src_arg) n ~dst astate =
    let event = Hist.call_event path location desc in
    let=* astate, pointer, (index, _) = to_internal_pointer_deref path Read location src astate in
    (* as in [operator_step], so that a position computed back to the one of [end()], e.g. by
       [v.end() - 1 + 1], is still detected *)
    let astate =
      if AddressAttributes.is_end_of_collection (fst pointer) astate then
        AddressAttributes.mark_as_end_of_collection index astate
      else astate
    in
    let** astate, position =
      PulseArithmetic.eval_binop (AbstractValue.mk_fresh ()) binop (AbstractValueOperand index) n
        astate
    in
    let=* astate, (arr_addr, _) = eval path Read location src astate in
    let pointer_hist = Hist.add_event event (snd dst) in
    let=* astate =
      write_position path location ~iter:dst ~arr:arr_addr ~position:(position, pointer_hist)
        pointer_hist astate
    in
    let astate = propagate_taint ~src ~dst astate in
    write_own_fields tenv path location event (class_of_arg src_arg) ~offset:(binop, n) ~src ~dst
      astate


  (** [operator+=] and [operator-=] *)
  let operator_offset_assign binop ~desc (FuncArg.{arg_payload= this} as this_arg) n :
      model_no_non_disj =
   fun ({ret= ret_id, _} as model_data) astate ->
    let<++> astate =
      apply_offset model_data ~desc binop this_arg (AbstractValueOperand (fst n)) ~dst:this astate
    in
    PulseOperations.write_id ret_id this astate


  let offset ~desc binop iter n ~dst : model_no_non_disj =
   fun model_data astate ->
    let<++> astate = apply_offset model_data ~desc binop iter n ~dst astate in
    astate


  (** [operator+], [operator-], [std::next] and [std::prev] *)
  let operator_offset binop ~desc iter (n, _) ret =
    offset ~desc binop iter (AbstractValueOperand n) ~dst:ret


  (** libc++'s overload for [std::prev(it)] *)
  let prev_one ~desc iter ret = offset ~desc MinusPI iter (ConstOperand (Cint IntLit.one)) ~dst:ret

  let advance ~desc (FuncArg.{arg_payload= iter} as iter_arg) (n, _) =
    offset ~desc PlusPI iter_arg (AbstractValueOperand n) ~dst:iter


  (* no "not found" case for search functions since callers often know that the element is there,
     as in the model of [folly::F14FastMap::find] *)
  let position_in_range ~desc first args : model_no_non_disj =
   fun {path; location} astate ->
    (* the returned iterator is passed as the last argument *)
    match List.last args with
    | None ->
        [Ok (ContinueProgram astate)]
    | Some ret ->
        let event = Hist.call_event path location desc in
        let<+> astate = point_into path location event ~collection:first ~iter:ret astate in
        propagate_taint ~src:first ~dst:ret astate


  let operator_compare comparison ~desc iter_lhs iter_rhs : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, _, (index_lhs, _) =
      to_internal_pointer_deref path Read location iter_lhs astate
    in
    let<*> astate, _, (index_rhs, _) =
      to_internal_pointer_deref path Read location iter_rhs astate
    in
    let ret_val = AbstractValue.mk_fresh () in
    let astate = PulseOperations.write_id ret_id (ret_val, Hist.single_event event) astate in
    let ret_val_equal, ret_val_notequal =
      match comparison with
      | `Equal ->
          (IntLit.one, IntLit.zero)
      | `NotEqual ->
          (IntLit.zero, IntLit.one)
    in
    let astate_equal =
      PulseArithmetic.and_eq_int ret_val ret_val_equal astate
      >>== PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand index_lhs)
             (AbstractValueOperand index_rhs)
      >>|| ExecutionDomain.continue
    in
    let astate_notequal =
      PulseArithmetic.and_eq_int ret_val ret_val_notequal astate
      >>== PulseArithmetic.prune_binop ~negated:false Ne (AbstractValueOperand index_lhs)
             (AbstractValueOperand index_rhs)
      >>|| ExecutionDomain.continue
    in
    SatUnsat.to_list astate_equal @ SatUnsat.to_list astate_notequal


  let operator_star ~desc iter : model_no_non_disj =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, pointer, _, (elem, _) =
      to_elem_pointed_by_iterator path Read location iter astate
    in
    PulseOperations.write_id (fst ret) (elem, Hist.add_event event (snd pointer)) astate


  let operator_step step ~desc iter : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, pointer, (index, _), _ =
      to_elem_pointed_by_iterator path Read ~step:(Some step) location iter astate
    in
    (* positions are related by the steps, so marking the position of [end()] detects stepping
       back to it, as in [--it; ++it] *)
    let astate =
      if AddressAttributes.is_end_of_collection (fst pointer) astate then
        AddressAttributes.mark_as_end_of_collection index astate
      else astate
    in
    let binop =
      match step with `PlusPlus -> Binop.PlusA None | `MinusMinus -> Binop.MinusA None
    in
    let<**> astate, index_new =
      PulseArithmetic.eval_binop (AbstractValue.mk_fresh ()) binop (AbstractValueOperand index)
        (ConstOperand (Cint IntLit.one)) astate
    in
    (* the internal pointer is shared with the copies of the iterator, and with the other [end()]
       iterators of the collection, so we replace it instead of writing through it *)
    let pointer_new = (AbstractValue.mk_fresh (), snd pointer) in
    let<+> astate =
      PulseOperations.write_field path location ~ref:iter internal_pointer ~obj:pointer_new astate
      >>= PulseOperations.write_deref path location ~ref:pointer_new
            ~obj:(index_new, Hist.add_event event (snd pointer))
    in
    astate
end

(* functions of [<algorithm>] that return an iterator into the range starting at their first
   argument; the ones that reorder the range, e.g. [std::remove_if], are left as unknown calls, which
   havoc the range *)
let algorithm_matchers =
  let open ProcnameDispatcher.Call in
  let position_in_range name =
    -"std" &:: name
    $ capt_arg_payload_of_typ_exists [-"std" &:: "__wrap_iter"; -"__gnu_cxx" &:: "__normal_iterator"]
    $+++$--> Iterator.position_in_range ~desc:(Printf.sprintf "std::%s()" name)
  in
  List.map ~f:position_in_range
    [ "adjacent_find"
    ; "find"
    ; "find_end"
    ; "find_first_of"
    ; "find_if"
    ; "find_if_not"
    ; "is_heap_until"
    ; "is_sorted_until"
    ; "lower_bound"
    ; "max_element"
    ; "min_element"
    ; "partition_point"
    ; "search"
    ; "search_n"
    ; "upper_bound" ]


let arithmetic_matchers namespace class_name =
  let open ProcnameDispatcher.Call in
  [ -namespace &:: class_name &:: "operator+" <>$ capt_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.operator_offset PlusPI ~desc:"iterator operator+"
  ; -namespace &:: class_name &:: "operator-" <>$ capt_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.operator_offset MinusPI ~desc:"iterator operator-"
  ; -namespace &:: class_name &:: "operator+=" <>$ capt_arg $+ capt_arg_payload
    $--> Iterator.operator_offset_assign PlusPI ~desc:"iterator operator+="
  ; -namespace &:: class_name &:: "operator-=" <>$ capt_arg $+ capt_arg_payload
    $--> Iterator.operator_offset_assign MinusPI ~desc:"iterator operator-=" ]


let iterator_function_matchers =
  let open ProcnameDispatcher.Call in
  let iterator () =
    capt_arg_of_typ_exists [-"std" &:: "__wrap_iter"; -"__gnu_cxx" &:: "__normal_iterator"]
  in
  [ -"std" &:: "advance" $ iterator () $+ capt_arg_payload
    $--> Iterator.advance ~desc:"std::advance()"
  ; -"std" &:: "next" $ iterator () $+ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.operator_offset PlusPI ~desc:"std::next()"
  ; -"std" &:: "prev" $ iterator () $+ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.operator_offset MinusPI ~desc:"std::prev()"
  ; -"std" &:: "prev" $ iterator () $+ capt_arg_payload $--> Iterator.prev_one ~desc:"std::prev()"
  ]


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"std" &:: "__wrap_iter" &:: "__wrap_iter" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> Iterator.constructor ~desc:"iterator constructor"
  ; -"std" &:: "__wrap_iter" &:: "operator=" $ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.assign ~desc:"iterator operator="
  ; -"std" &:: "__wrap_iter" &:: "operator*" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator*"
  ; -"std" &:: "__wrap_iter" &:: "operator->" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator->"
  ; -"std" &:: "__wrap_iter" &:: "operator++" <>$ capt_arg_payload
    $--> Iterator.operator_step `PlusPlus ~desc:"iterator operator++"
  ; -"std" &:: "__wrap_iter" &:: "operator--" <>$ capt_arg_payload
    $--> Iterator.operator_step `MinusMinus ~desc:"iterator operator--"
  ; -"std" &:: "operator=="
    $ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
    $+ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
    $--> Iterator.operator_compare `Equal ~desc:"iterator operator=="
  ; -"std" &:: "operator!="
    $ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
    $+ capt_arg_payload_of_typ (-"std" &:: "__wrap_iter")
    $--> Iterator.operator_compare `NotEqual ~desc:"iterator operator!="
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "__normal_iterator" $ capt_arg_payload
    $+ capt_arg_payload
    $+...$--> Iterator.constructor ~desc:"iterator constructor"
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator=" $ capt_arg_payload $+ capt_arg_payload
    $--> Iterator.assign ~desc:"iterator operator="
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator*" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator*"
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator->" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator->"
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator++" <>$ capt_arg_payload
    $--> Iterator.operator_step `PlusPlus ~desc:"iterator operator++"
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator--" <>$ capt_arg_payload
    $--> Iterator.operator_step `MinusMinus ~desc:"iterator operator--"
  ; -"__gnu_cxx" &:: "operator=="
    $ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
    $+ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
    $--> Iterator.operator_compare `Equal ~desc:"iterator operator=="
  ; -"__gnu_cxx" &:: "operator!="
    $ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
    $+ capt_arg_payload_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
    $--> Iterator.operator_compare `NotEqual ~desc:"iterator operator!=" ]
  @ arithmetic_matchers "std" "__wrap_iter"
  @ arithmetic_matchers "__gnu_cxx" "__normal_iterator"
  @ iterator_function_matchers @ algorithm_matchers
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
  |> List.map ~f:(ProcnameDispatcher.Call.map_matcher ~f:lift_model)
