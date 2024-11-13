(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseModelsImport
module DSL = PulseModelsDSL

let dict_tname = TextualSil.python_dict_type_name

let int_tname = TextualSil.python_int_type_name

let none_tname = TextualSil.python_none_type_name

let tuple_tname = TextualSil.python_tuple_type_name

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

module Tuple = struct
  let str_field_of_int i = F.asprintf "#%d" i

  let make args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let bindings =
      List.mapi args ~f:(fun i aval ->
          let field = str_field_of_int i in
          (field, aval) )
    in
    let* dict = constructor tuple_tname bindings in
    ret dict


  let get tuple idx : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* opt_int = as_constant_int idx in
    match opt_int with
    | None ->
        fresh ()
    | Some i ->
        let field = Fieldname.make tuple_tname (str_field_of_int i) in
        load_access tuple (FieldAccess field)
end

let build_tuple args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* tuple = Tuple.make args in
  assign_ret tuple


let call_dsl ~closure ~globals ~arg_names:_ ~args : DSL.aval DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let gen_closure_args {ProcAttributes.python_args} =
    let* locals = Dict.make python_args args in
    match Config.python_globals with
    | OwnByClosures ->
        ret [locals]
    | OwnByModule ->
        ret [globals; locals]
  in
  apply_python_closure closure gen_closure_args


let call closure globals arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = call_dsl ~closure ~globals ~arg_names ~args in
  assign_ret value


let call_method name obj arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* closure = Dict.get obj name in
  (* TODO: for OO method, gives self argument *)
  let* value = call_dsl ~closure ~globals:obj ~arg_names ~args in
  assign_ret value


let import_from name module_ : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res = Dict.get module_ name in
  assign_ret res


let import_name name _fromlist _level : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_str = as_constant_string name in
  let function_name =
    Option.value_or_thunk opt_str ~default:(fun () ->
        L.die InternalError "frontend should always give a string here" )
  in
  let proc_name = Procname.make_python ~class_name:None ~function_name in
  let* globals = Dict.make [] [] in
  (* TODO: call it only once! *)
  let* _ = python_call proc_name [("globals", globals)] in
  assign_ret globals


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


let make_dictionary _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  (* TODO: take args into account *)
  let* dict = Dict.make [] [] in
  assign_ret dict


let make_function closure _default_values _default_values_kw _annotations _cells_for_closure : model
    =
  let open DSL.Syntax in
  start_model @@ fun () -> assign_ret closure


let make_int arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_int = as_constant_int arg in
  let* res =
    match opt_int with
    | None ->
        constructor int_tname []
    | Some i ->
        let* res = int i in
        let* () = and_dynamic_type_is res (Typ.mk_struct int_tname) in
        ret res
  in
  assign_ret res


let make_none : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* none = constructor none_tname [] in
  assign_ret none


let store_fast name locals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let store_global name globals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set globals name value


let store_name name locals _globals value : model =
  let open DSL.Syntax in
  start_model @@ fun () -> Dict.set locals name value


let subscript seq idx : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* res =
    dynamic_dispatch seq
      ~cases:[(tuple_tname, fun () -> Tuple.get seq idx)] (* TODO: other sequence types *)
      ~default:(fun () -> fresh ())
  in
  assign_ret res


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  [ -"$builtins" &:: "py_call" <>$ arg $+ arg $+ arg $+++$--> call
  ; -"$builtins" &:: "py_call_method" <>$ arg $+ arg $+ arg $+++$--> call_method
  ; -"$builtins" &:: "py_build_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_import_from" <>$ arg $+ arg $--> import_from
  ; -"$builtins" &:: "py_import_name" <>$ arg $+ arg $+ arg $--> import_name
  ; -"$builtins" &:: "py_make_dictionary" &::.*+++> make_dictionary
  ; -"$builtins" &:: "py_make_function" <>$ arg $+ arg $+ arg $+ arg $+ arg $--> make_function
  ; -"$builtins" &:: "py_make_int" <>$ arg $--> make_int
  ; -"$builtins" &:: "py_load_fast" <>$ arg $+ arg $--> load_fast
  ; -"$builtins" &:: "py_load_global" <>$ arg $+ arg $--> load_global
  ; -"$builtins" &:: "py_load_name" <>$ arg $+ arg $+ arg $--> load_name
  ; -"$builtins" &:: "py_make_none" <>--> make_none
  ; -"$builtins" &:: "py_subscript" <>$ arg $+ arg $--> subscript
  ; -"$builtins" &:: "py_store_fast" <>$ arg $+ arg $+ arg $--> store_fast
  ; -"$builtins" &:: "py_store_global" <>$ arg $+ arg $+ arg $--> store_global
  ; -"$builtins" &:: "py_store_name" <>$ arg $+ arg $+ arg $+ arg $--> store_name ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
