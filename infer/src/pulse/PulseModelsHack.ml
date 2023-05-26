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

let read_boxed_string_value address astate =
  let open IOption.Let_syntax in
  let hackString = Typ.HackClass (HackClassName.make "HackString") in
  let field = Fieldname.make hackString "val" in
  let* box_val, _ = Memory.find_edge_opt address (FieldAccess field) astate in
  let* string_val, _ = Memory.find_edge_opt box_val Dereference astate in
  AddressAttributes.get_const_string string_val astate


let hack_dim_field_get this_obj_dbl_ptr (field_string_obj, _) : model =
 fun {path; location; ret} astate ->
  match read_boxed_string_value field_string_obj astate with
  | Some string_val ->
      (* TODO: add a move up in the class hierarchy to find the right field declaration *)
      let field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack string_val in
      let<*> astate, this_val =
        PulseOperations.eval_access path Read location this_obj_dbl_ptr Dereference astate
      in
      let<+> astate, field_val =
        PulseOperations.eval_access path Read location this_val (FieldAccess field) astate
      in
      let ret_id, _ = ret in
      PulseOperations.write_id ret_id field_val astate
  | None ->
      (* TODO: invalidate the access if the string field is unknown *)
      Logging.d_printfln "reading field failed" ;
      astate |> Basic.ok_continue


let hack_dim_field_get_or_null this_obj_dbl_ptr field : model =
 fun ({path; location; ret= ret_id, _} as model_data) astate ->
  let<*> astate, (this_obj, _) =
    PulseOperations.eval_access path Read location this_obj_dbl_ptr Dereference astate
  in
  (* Case 1: this_obj is a null ptr *)
  let astate1 =
    let<++> astate = PulseArithmetic.prune_eq_zero this_obj astate in
    let null_ret = (AbstractValue.mk_fresh (), Hist.single_call path location "null ptr case") in
    PulseOperations.write_id ret_id null_ret astate
  in
  (* Case 2: this_obj is not null in which case we call the regular hack_dim_field_get *)
  let astate2 =
    let<**> astate = PulseArithmetic.prune_ne_zero this_obj astate in
    hack_dim_field_get this_obj_dbl_ptr field model_data astate
  in
  astate1 @ astate2


let hack_dim_array_get this_obj (key_string_obj, _) : model =
 fun {path; location; ret} astate ->
  (* TODO: a key could be also a int *)
  match read_boxed_string_value key_string_obj astate with
  | Some string_val ->
      let field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack string_val in
      let<*> astate, this_val =
        PulseOperations.eval_access path Read location this_obj Dereference astate
      in
      let<+> astate, field_val =
        PulseOperations.eval_access path Read location this_val (FieldAccess field) astate
      in
      let ret_id, _ = ret in
      PulseOperations.write_id ret_id field_val astate
  | None ->
      (* TODO: invalidate the access if the string key is unknown (raise an exception in Hack)*)
      Logging.d_printfln "reading dict failed" ;
      astate |> Basic.ok_continue


let hack_await (argv, hist) : model =
 fun {ret= ret_id, _} astate ->
  let astate = AddressAttributes.hack_async_await argv astate in
  PulseOperations.write_id ret_id (argv, hist) astate |> Basic.ok_continue


let get_static_companion type_name astate =
  let pvar = Pvar.mk_global (Mangled.mangled (Typ.Name.name type_name) "STATIC") in
  let var = Var.of_pvar pvar in
  (* we chose on purpose to not abduce [pvar] because we don't want to make a disjunctive case
     if it is already assigned or not. This is problematic when the caller already defines the
     variable because the Pulse summary application will not detect that the variable is set
     both in the callee and the caller. But this is fine as long as both functions perform the
     same initialization of the variable. *)
  match AbductiveDomain.Stack.find_opt var astate with
  | Some (addr, _) ->
      (addr, astate)
  | None ->
      let addr = AbstractValue.mk_fresh () in
      let astate = AbductiveDomain.Stack.add var (addr, ValueHistory.epoch) astate in
      let static_type_name = Typ.Name.Hack.static_companion type_name in
      let typ = Typ.mk_struct static_type_name in
      let astate = PulseOperations.add_dynamic_type typ addr astate in
      (addr, astate)


let lazy_class_initialize size_exp : model =
 fun {path; location; ret= ret_id, _} astate ->
  let type_name =
    match size_exp with
    | Exp.Sizeof {typ= {desc= Typ.Tstruct type_name}} ->
        type_name
    | _ ->
        L.die InternalError
          "lazy_class_initialize: the Hack frontend should never generate such argument type"
  in
  let addr, astate = get_static_companion type_name astate in
  let hist = Hist.single_call path location "lazy_class_initialize" in
  PulseOperations.write_id ret_id (addr, hist) astate |> Basic.ok_continue


let get_static_class (addr, _) : model =
 fun {analysis_data; path; location; ret= ret_id, _} astate ->
  match AbductiveDomain.AddressAttributes.get_dynamic_type addr astate with
  | Some {desc= Tstruct type_name} ->
      let addr, astate = get_static_companion type_name astate in
      let hist = Hist.single_call path location "get_static_class" in
      PulseOperations.write_id ret_id (addr, hist) astate |> Basic.ok_continue
  | _ ->
      AbductiveDomain.add_need_dynamic_type_specialization analysis_data.proc_desc addr astate
      |> Basic.ok_continue


(* We model dict/shape keys as fields. This is a bit unorthodox in Pulse, but we need
   maximum precision on this ubiquitous Hack data structure. *)
let new_dict args : model =
 fun {path; location; ret= ret_id, _} astate ->
  let open IOption.Let_syntax in
  let bindings =
    args
    |> List.map ~f:(fun {ProcnameDispatcher.Call.FuncArg.arg_payload= addr, _} -> addr)
    |> List.chunks_of ~length:2
    |> List.filter_map ~f:(function
         | [string_addr; val_addr] ->
             let+ string_val = read_boxed_string_value string_addr astate in
             (string_val, val_addr)
         | _ ->
             None )
  in
  let addr = AbstractValue.mk_fresh () in
  let typ = Typ.mk_struct TextualSil.hack_dict_type_name in
  let astate = PulseOperations.add_dynamic_type typ addr astate in
  let hist = Hist.single_call path location "new_dict" in
  let open PulseResult.Let_syntax in
  let<+> astate =
    List.fold bindings ~init:(Ok astate) ~f:(fun acc (key, addr_val) ->
        let* astate = acc in
        let field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack key in
        let ref = (addr, hist) in
        let obj = (addr_val, hist) in
        PulseOperations.write_deref_field path location ~ref ~obj field astate )
  in
  PulseOperations.write_id ret_id (addr, hist) astate


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "nondet" <>$$--> Basic.nondet ~desc:"nondet"
  ; +BuiltinDecl.(match_builtin __lazy_class_initialize) <>$ capt_exp $--> lazy_class_initialize
  ; -"$builtins" &:: "hhbc_await" <>$ capt_arg_payload $--> hack_await
  ; -"$builtins" &:: "hack_dim_field_get" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hack_dim_field_get
  ; -"$builtins" &:: "hack_dim_field_get_or_null" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hack_dim_field_get_or_null
  ; -"$builtins" &:: "hack_dim_array_get" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hack_dim_array_get
  ; -"$builtins" &:: "hack_new_dict" &::.*++> new_dict
  ; -"$builtins" &:: "hack_get_class" <>$ capt_arg_payload
    $--> Basic.id_first_arg ~desc:"hack_get_class"
    (* not clear why HackC generate this builtin call *)
  ; -"$builtins" &:: "hhbc_class_get_c" <>$ capt_arg_payload $--> get_static_class
    (* we should be able to model that directly in Textual once specialization will be stronger *)
  ; -"$builtins" &:: "hack_get_static_class" <>$ capt_arg_payload $--> get_static_class ]
