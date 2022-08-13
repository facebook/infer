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

let string_length_access = HilExp.Access.FieldAccess PulseOperations.ModeledField.string_length

let string_is_null_or_whitespace ~desc ((addr, hist) as addr_hist) : model =
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


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let map_context_tenv f (x, _) = f x in
  [ +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrWhiteSpace" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrWhiteSpace"
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.String")
    &:: "IsNullOrEmpty" <>$ capt_arg_payload
    $--> string_is_null_or_whitespace ~desc:"String.IsNullOrEmpty"
  ; +map_context_tenv (PatternMatch.CSharp.implements "System.Diagnostics.Debug")
    &:: "Assert" <>$ capt_arg $--> Basic.assert_ ]
