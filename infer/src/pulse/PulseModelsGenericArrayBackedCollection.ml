(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperations.Import
open PulseModelsImport

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
    PulseOperations.eval_deref_access path Write location collection (FieldAccess last_field) astate
  in
  let astate = AddressAttributes.mark_as_end_of_collection (fst pointer) astate in
  (astate, pointer)


let eval_is_empty path location collection astate =
  PulseOperations.eval_deref_access path Write location collection (FieldAccess is_empty) astate


module Iterator = struct
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
        let invalidation_trace = Trace.Immediate {location; history= Epoch} in
        let access_trace = Trace.Immediate {location; history= snd pointer} in
        FatalError
          ( ReportableError
              { diagnostic=
                  Diagnostic.AccessToInvalidAddress
                    { calling_context= []
                    ; invalidation= EndIterator
                    ; invalidation_trace
                    ; access_trace
                    ; must_be_valid_reason= None }
              ; astate }
          , [] )
      else Ok astate
    in
    (* We do not want to create internal array if iterator pointer has an invalid value *)
    let* astate = PulseOperations.check_addr_access path Read location index astate in
    let+ astate, elem = element path location iterator (fst index) astate in
    (astate, pointer, elem)


  let construct path location event ~init ~ref astate =
    let* astate, (arr_addr, arr_hist) = eval path Read location init astate in
    let* astate =
      PulseOperations.write_deref_field path location ~ref field
        ~obj:(arr_addr, Hist.add_event path event arr_hist)
        astate
    in
    let* astate, (p_addr, p_hist) = to_internal_pointer path Read location init astate in
    PulseOperations.write_field path location ~ref internal_pointer
      ~obj:(p_addr, Hist.add_event path event p_hist)
      astate


  let constructor ~desc this init : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = construct path location event ~init ~ref:this astate in
    astate


  let operator_compare comparison ~desc iter_lhs iter_rhs : model =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<*> astate, _, (index_lhs, _) =
      to_internal_pointer_deref path Read location iter_lhs astate
    in
    let<*> astate, _, (index_rhs, _) =
      to_internal_pointer_deref path Read location iter_rhs astate
    in
    let ret_val = AbstractValue.mk_fresh () in
    let astate = PulseOperations.write_id ret_id (ret_val, Hist.single_event path event) astate in
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
      |> Basic.map_continue
    in
    let astate_notequal =
      PulseArithmetic.and_eq_int ret_val ret_val_notequal astate
      >>= PulseArithmetic.prune_binop ~negated:false Ne (AbstractValueOperand index_lhs)
            (AbstractValueOperand index_rhs)
      |> Basic.map_continue
    in
    [astate_equal; astate_notequal]


  let operator_star ~desc iter : model =
   fun {path; location; ret} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, pointer, (elem, _) =
      to_elem_pointed_by_iterator path Read location iter astate
    in
    PulseOperations.write_id (fst ret) (elem, Hist.add_event path event (snd pointer)) astate


  let operator_step step ~desc iter : model =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let index_new = AbstractValue.mk_fresh () in
    let<*> astate, pointer, _ =
      to_elem_pointed_by_iterator path Read ~step:(Some step) location iter astate
    in
    let<+> astate =
      PulseOperations.write_deref path location ~ref:pointer
        ~obj:(index_new, Hist.add_event path event (snd pointer))
        astate
    in
    astate
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"std" &:: "__wrap_iter" &:: "__wrap_iter" <>$ capt_arg_payload $+ capt_arg_payload
    $+...$--> Iterator.constructor ~desc:"iterator constructor"
  ; -"std" &:: "__wrap_iter" &:: "operator*" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator*"
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
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "__normal_iterator" <>$ capt_arg_payload
    $+ capt_arg_payload
    $+...$--> Iterator.constructor ~desc:"iterator constructor"
  ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator*" <>$ capt_arg_payload
    $--> Iterator.operator_star ~desc:"iterator operator*"
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
