(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseOperations.Import
open PulseModelsImport

module SmartPointers = struct
  let value = Fieldname.make PulseOperations.pulse_model_type "backing_pointer"

  let value_access = HilExp.Access.FieldAccess value

  let to_internal_value path mode location value astate =
    PulseOperations.eval_access path mode location value value_access astate


  let to_internal_value_deref path mode location value astate =
    let* astate, pointer = to_internal_value path Read location value astate in
    PulseOperations.eval_access path mode location pointer Dereference astate


  let write_value path location this ~value ~desc astate =
    let* astate, pointer = to_internal_value path Read location this astate in
    let value_hist = (fst value, Hist.add_call path location desc (snd value)) in
    let+ astate = PulseOperations.write_deref path location ~ref:pointer ~obj:value_hist astate in
    (astate, (pointer, value_hist))


  let assign_value_nullptr path location this ~desc astate =
    let* astate, (pointer, value) =
      write_value path location this
        ~value:(AbstractValue.mk_fresh (), ValueHistory.epoch)
        ~desc astate
    in
    let* astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
    PulseOperations.invalidate path
      (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
      location (ConstantDereference IntLit.zero) value astate


  let delete_internal access path location this ~desc astate =
    let call_event = Hist.call_event path location desc in
    let* astate, (value_addr, value_hist) =
      PulseOperations.eval_access path Read location this access astate
    in
    let value_addr_hist = (value_addr, Hist.add_event path call_event value_hist) in
    PulseOperations.invalidate_access path location CppDelete value_addr_hist Dereference astate


  let delete_internal_value = delete_internal value_access

  let dereference this ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<*> astate, ((value_addr, value_hist) as value) =
      to_internal_value_deref path Write location this astate
    in
    (* Check dereference to show an error at the callsite of `operator*()` or `operator->()` *)
    let<+> astate, _ = PulseOperations.eval_access path Write location value Dereference astate in
    PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


  let get this ~desc : model =
   fun {path; location; ret= ret_id, _} astate ->
    let<+> astate, (value_addr, value_hist) =
      to_internal_value_deref path Read location this astate
    in
    PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


  let at this index ~desc : model =
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
    PulseOperations.write_id (fst ret) (addr, Hist.add_event path event hist) astate


  module UniquePtr = struct
    let default_constructor this ~desc : model =
     fun {path; location} astate ->
      let<+> astate = assign_value_nullptr path location this ~desc astate in
      astate


    let assign_pointer this value ~desc : model =
     fun {path; location} astate ->
      let<+> astate, _ = write_value path location this ~value ~desc astate in
      astate


    let destructor this ~desc : model =
     fun {path; location} astate ->
      let<+> astate = delete_internal_value path location this ~desc astate in
      astate


    let default_reset this ~desc : model =
     fun {path; location} astate ->
      let<*> astate = delete_internal_value path location this ~desc astate in
      let<+> astate = assign_value_nullptr path location this ~desc astate in
      astate


    let reset this value ~desc : model =
     fun {path; location} astate ->
      let<*> astate = delete_internal_value path location this ~desc astate in
      let<+> astate, _ = write_value path location this ~value ~desc astate in
      astate


    let release this ~desc : model =
     fun {path; location; ret= ret_id, _} astate ->
      let<*> astate, (old_value_addr, old_value_hist) =
        to_internal_value_deref path Read location this astate
      in
      let<+> astate = assign_value_nullptr path location this ~desc astate in
      PulseOperations.write_id ret_id
        (old_value_addr, Hist.add_call path location desc old_value_hist)
        astate


    let make_unique value this ~desc : model =
     fun {path; location} astate ->
      let<+> astate, _ = write_value path location this ~value ~desc astate in
      astate


    let move this ~desc : model =
     fun {path; location; ret= ret_id, _} astate ->
      let<*> astate, value = to_internal_value_deref path Read location this astate in
      let ret_addr, ret_hist = (AbstractValue.mk_fresh (), snd value) in
      let<*> astate, _ = write_value path location (ret_addr, ret_hist) ~value ~desc astate in
      let<+> astate = assign_value_nullptr path location this ~desc astate in
      PulseOperations.write_id ret_id (ret_addr, Hist.add_call path location desc ret_hist) astate


    let assignment this other ~desc : model =
     fun {path; location} astate ->
      let<*> astate, value = to_internal_value_deref path Read location other astate in
      let<*> astate, _ = write_value path location this ~value ~desc astate in
      let<+> astate = assign_value_nullptr path location other ~desc astate in
      astate
  end
end

let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ (* matchers for unique_ptr *)
    -"std" &:: "move"
    $ capt_arg_payload_of_typ (-"std" &:: "unique_ptr")
    $--> SmartPointers.UniquePtr.move ~desc:"std::move"
  ; -"std" &:: "make_unique" $ capt_arg_payload $+ capt_arg_payload
    $--> SmartPointers.UniquePtr.make_unique ~desc:"std::make_unique()"
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg_payload
    $--> SmartPointers.UniquePtr.default_constructor ~desc:"std::unique_ptr::unique_ptr()"
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_ptr")
    $--> SmartPointers.UniquePtr.assignment
           ~desc:"std::unique_ptr::unique_ptr(std::unique_ptr<T> arg)"
  ; -"std" &:: "unique_ptr" &:: "operator=" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_ptr")
    $--> SmartPointers.UniquePtr.assignment
           ~desc:"std::unique_ptr::operator=(std::unique_ptr<T> arg)"
  ; -"std" &:: "unique_ptr" &:: "unique_ptr" $ capt_arg_payload $+ capt_arg_payload
    $--> SmartPointers.UniquePtr.assign_pointer ~desc:"std::unique_ptr::unique_ptr(T* arg)"
  ; -"std" &:: "unique_ptr" &:: "~unique_ptr" $ capt_arg_payload
    $--> SmartPointers.UniquePtr.destructor ~desc:"std::unique_ptr::~unique_ptr()"
  ; -"std" &:: "unique_ptr" &:: "reset" $ capt_arg_payload
    $--> SmartPointers.UniquePtr.default_reset ~desc:"std::unique_ptr::reset()"
  ; -"std" &:: "unique_ptr" &:: "reset" $ capt_arg_payload $+ capt_arg_payload
    $--> SmartPointers.UniquePtr.reset ~desc:"std::unique_ptr::reset(T* arg)"
  ; -"std" &:: "unique_ptr" &:: "release" $ capt_arg_payload
    $--> SmartPointers.UniquePtr.release ~desc:"std::unique_ptr::release()"
  ; -"std" &:: "unique_ptr" &:: "operator[]" $ capt_arg_payload $+ capt_arg_payload
    $--> SmartPointers.at ~desc:"std::unique_ptr::operator[]()"
  ; -"std" &:: "unique_ptr" &:: "get" $ capt_arg_payload
    $--> SmartPointers.get ~desc:"std::unique_ptr::get()"
  ; -"std" &:: "unique_ptr" &:: "operator*" $ capt_arg_payload
    $--> SmartPointers.dereference ~desc:"std::unique_ptr::operator*()"
  ; -"std" &:: "unique_ptr" &:: "operator->" <>$ capt_arg_payload
    $--> SmartPointers.dereference ~desc:"std::unique_ptr::operator->()" ]
