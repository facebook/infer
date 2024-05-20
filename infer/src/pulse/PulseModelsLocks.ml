(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseOperationResult.Import
open PulseModelsImport

let value = Fieldname.make PulseOperations.pulse_model_type "__infer_mutex"

let value_access = Access.FieldAccess value

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
  let=* astate, (pointer, value) =
    write_value path location this
      ~value:(AbstractValue.mk_fresh (), ValueHistory.epoch)
      ~desc astate
  in
  let++ astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
  PulseOperations.invalidate path
    (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
    location (ConstantDereference IntLit.zero) value astate


let mutex this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (value_addr, value_hist) =
    to_internal_value_deref path Read location this astate
  in
  PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate


let swap this other ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<*> astate, this_value = to_internal_value_deref path Read location this astate in
  let<*> astate, other_value = to_internal_value_deref path Read location other astate in
  let<*> astate, _ = write_value path location this ~value:other_value ~desc astate in
  let<+> astate, _ = write_value path location other ~value:this_value ~desc astate in
  astate


let default_constructor this ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<++> astate = assign_value_nullptr path location this ~desc astate in
  astate


let assign_mutex this value ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<+> astate, _ = write_value path location this ~value ~desc astate in
  astate


let release this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, (old_value_addr, old_value_hist) =
    to_internal_value_deref path Read location this astate
  in
  let<++> astate = assign_value_nullptr path location this ~desc astate in
  PulseOperations.write_id ret_id
    (old_value_addr, Hist.add_call path location desc old_value_hist)
    astate


let move_assignment this other ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let<*> astate, value = to_internal_value_deref path Read location other astate in
  let<**> astate = assign_value_nullptr path location other ~desc astate in
  let<+> astate, _ = write_value path location this ~value ~desc astate in
  astate


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ (* matchers for std::unique_lock *)
    -"std" &:: "unique_lock" &:: "unique_lock" $ capt_arg_payload
    $--> default_constructor ~desc:"std::unique_lock::unique_lock()"
  ; -"std" &:: "unique_lock" &:: "unique_lock" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_lock")
    $--> move_assignment ~desc:"std::unique_lock::unique_lock(std::unique_lock<T>)"
  ; -"std" &:: "unique_lock" &:: "operator=" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "unique_lock")
    $--> move_assignment ~desc:"std::unique_lock::operator=(std::unique_lock<T>)"
  ; -"std" &:: "unique_lock" &:: "unique_lock" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> assign_mutex ~desc:"std::unique_lock::unique_lock(Mutex)"
  ; -"std" &:: "unique_lock" &:: "release" $ capt_arg_payload
    $--> release ~desc:"std::unique_lock::release()"
  ; -"std" &:: "unique_lock" &:: "mutex" $ capt_arg_payload
    $--> mutex ~desc:"std::unique_lock::mutex()"
  ; -"std" &:: "unique_lock" &:: "swap" $ capt_arg_payload $+ capt_arg_payload
    $--> swap ~desc:"std::unique_lock::swap(std::unique_lock<Mutex>)"
  ; -"std" &:: "unique_lock" &:: "~unique_lock" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "lock" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "try_lock" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "try_lock_for" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "try_lock_until" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "unlock" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "owns_lock" &::.*--> Basic.skip
  ; -"std" &:: "unique_lock" &:: "operator_bool" &::.*--> Basic.skip
    (* matchers for std::shared_lock *)
  ; -"std" &:: "shared_lock" &:: "shared_lock" $ capt_arg_payload
    $--> default_constructor ~desc:"std::shared_lock::shared_lock()"
  ; -"std" &:: "shared_lock" &:: "shared_lock" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "shared_lock")
    $--> move_assignment ~desc:"std::shared_lock::shared_lock(std::shared_lock<T>)"
  ; -"std" &:: "shared_lock" &:: "operator=" $ capt_arg_payload
    $+ capt_arg_payload_of_typ (-"std" &:: "shared_lock")
    $--> move_assignment ~desc:"std::shared_lock::operator=(std::shared_lock<T>)"
  ; -"std" &:: "shared_lock" &:: "shared_lock" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> assign_mutex ~desc:"std::shared_lock::shared_lock(Mutex)"
  ; -"std" &:: "shared_lock" &:: "release" $ capt_arg_payload
    $--> release ~desc:"std::shared_lock::release()"
  ; -"std" &:: "shared_lock" &:: "mutex" $ capt_arg_payload
    $--> mutex ~desc:"std::shared_lock::mutex()"
  ; -"std" &:: "shared_lock" &:: "swap" $ capt_arg_payload $+ capt_arg_payload
    $--> swap ~desc:"std::shared_lock::swap(std::shared_lock<Mutex>)"
  ; -"std" &:: "shared_lock" &:: "~shared_lock" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "lock" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "try_lock" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "try_lock_for" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "try_lock_until" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "unlock" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "owns_lock" &::.*--> Basic.skip
  ; -"std" &:: "shared_lock" &:: "operator_bool" &::.*--> Basic.skip ]
  |> List.map ~f:(fun matcher ->
         matcher
         |> ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist
         |> ProcnameDispatcher.Call.map_matcher ~f:lift_model )
