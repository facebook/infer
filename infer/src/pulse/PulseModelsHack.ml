(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport

let read_boxed_string_value address astate =
  let open IOption.Let_syntax in
  let hackString = Typ.HackClass (HackClassName.make "HackString") in
  let field = Fieldname.make hackString "val" in
  let* box_val, _ = Memory.find_edge_opt address (FieldAccess field) astate in
  let* string_val, _ = Memory.find_edge_opt box_val Dereference astate in
  AddressAttributes.get_const_string string_val astate


let hack_dim_field_get this_obj (field_string_obj, _) : model =
 fun {path; location; ret} astate ->
  match read_boxed_string_value field_string_obj astate with
  | Some string_val ->
      (* TODO: add a move up in the class hierarchy to find the right field declaration *)
      let class_name = TextualSil.hack_mixed_type_name in
      let field = Fieldname.make class_name string_val in
      let<*> astate, this_val =
        PulseOperations.eval_access path Read location this_obj Dereference astate
      in
      let<+> astate, field_val =
        PulseOperations.eval_access path Read location this_val (FieldAccess field) astate
      in
      let ret_id, _ = ret in
      PulseOperations.write_id ret_id field_val astate
  | None ->
      (* TODO: invalidate the access if the string field is unknown *)
      Logging.d_printfln "reading field failed@." ;
      astate |> Basic.ok_continue


let hack_await (argv, hist) : model =
 fun {ret=ret_id, _} astate ->
  let astate = AddressAttributes.hack_async_await argv astate in
  PulseOperations.write_id ret_id (argv, hist) astate |> Basic.ok_continue


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "hack_dim_field_get" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hack_dim_field_get
  ; -"$builtins" &:: "hack_new_dict" <>$ any_arg $+...$--> Basic.skip
  ; -"$builtins" &:: "await" <>$ capt_arg_payload $--> hack_await ]
