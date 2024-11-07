(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseModelsImport
module DSL = PulseModelsDSL

let dict_tname = TextualSil.python_dict_type_name

let none_tname = TextualSil.python_none_type_name

let sil_fieldname_from_string_value_exn type_name ((address, _) : DSL.aval) :
    Fieldname.t DSL.model_monad =
  let f astate =
    match PulseArithmetic.as_constant_string astate address with
    | Some str ->
        (Fieldname.make type_name str, astate)
    | None ->
        L.die InternalError "expecting constant string value"
  in
  DSL.Syntax.exec_operation f


module Dict = struct
  let make keys args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    if not (Int.equal (List.length args) (List.length keys)) then
      L.die InternalError "Dict.make expects two list of same length@\n" ;
    let bindings = List.zip_exn keys args in
    let* dict = constructor dict_tname bindings in
    ret dict


  let get dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* field = sil_fieldname_from_string_value_exn dict_tname key in
    load_access dict (FieldAccess field)


  let set dict key value : unit DSL.model_monad =
    let open DSL.Syntax in
    let* field = sil_fieldname_from_string_value_exn dict_tname key in
    let* () = store_field ~ref:dict field value in
    ret ()
end

let call closure _arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let gen_closure_args {ProcAttributes.python_args} =
    let* locals = Dict.make python_args args in
    ret [locals]
  in
  let* value = apply_python_closure closure gen_closure_args in
  assign_ret value


let load_fast name locals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = Dict.get locals name in
  assign_ret value


let load_global name globals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = Dict.get globals name in
  (* TODO: decide what we do if the binding is missing in globals (for builtins) *)
  assign_ret value


let load_name name locals _globals : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = Dict.get locals name in
  (* TODO: decide what we do if the binding is missing in locals *)
  assign_ret value


let make_function closure _default_values _default_values_kw _annotations _cells_for_closure : model
    =
  let open DSL.Syntax in
  start_model @@ fun () -> assign_ret closure


let make_dictionary _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  (* TODO: take args into account *)
  let* dict = Dict.make [] [] in
  assign_ret dict


let store_fast name locals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let store_name name locals _globals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let make_none : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* none = constructor none_tname [] in
  assign_ret none


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "py_call" <>$ capt_arg_payload $+ capt_arg_payload $+++$--> call
  ; -"$builtins" &:: "py_make_dictionary" &::.*+++> make_dictionary
  ; -"$builtins" &:: "py_make_function" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $+ capt_arg_payload $--> make_function
  ; -"$builtins" &:: "py_load_fast" <>$ capt_arg_payload $+ capt_arg_payload $--> load_fast
  ; -"$builtins" &:: "py_load_global" <>$ capt_arg_payload $+ capt_arg_payload $--> load_global
  ; -"$builtins" &:: "py_load_name" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> load_name
  ; -"$builtins" &:: "py_make_none" <>--> make_none
  ; -"$builtins" &:: "py_store_fast" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> store_fast
  ; -"$builtins" &:: "py_store_name" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> store_name ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
