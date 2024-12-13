(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
open PulseModelsImport
module DSL = PulseModelsDSL

let dict_tname = TextualSil.python_dict_type_name

let int_tname = TextualSil.python_int_type_name

let none_tname = TextualSil.python_none_type_name

let tuple_tname = TextualSil.python_tuple_type_name

let module_tname module_name =
  let str = F.asprintf "%s%s" PythonClassName.globals_prefix module_name in
  Typ.PythonClass (PythonClassName.make str)


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
    let* dict = constructor ~deref:false dict_tname bindings in
    ret dict


  let propagate_static_type_on_load dict key load_res : unit DSL.model_monad =
    let open DSL.Syntax in
    let rec propagate_field_type tname key =
      let field = Fieldname.make tname key in
      let* opt_info = tenv_resolve_field_info tname field in
      option_iter opt_info ~f:(fun {Struct.typ= field_typ} ->
          let opt_static_tname = Typ.name (Typ.strip_ptr field_typ) in
          option_iter opt_static_tname ~f:(fun static_tname ->
              if Typ.Name.is_python_module_attribute static_tname then
                let tname, attr =
                  Typ.Name.get_python_module_attribute_infos static_tname |> Option.value_exn
                in
                propagate_field_type tname attr
              else add_static_type static_tname load_res ) )
    in
    let* opt_key = as_constant_string key in
    let key =
      Option.value_or_thunk opt_key ~default:(fun () ->
          L.die InternalError "expecting constant string value" )
    in
    let* opt_static_type = get_static_type dict in
    option_iter opt_static_type ~f:(fun tname -> propagate_field_type tname key)


  let get dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* field = sil_fieldname_from_string_value_exn dict_tname key in
    let* load_res = load_access ~deref:false dict (FieldAccess field) in
    let* () = propagate_static_type_on_load dict key load_res in
    ret load_res


  let set dict key value : unit DSL.model_monad =
    let open DSL.Syntax in
    let* field = sil_fieldname_from_string_value_exn dict_tname key in
    let* () = store_field ~deref:false ~ref:dict field value in
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
    let* dict = constructor ~deref:false tuple_tname bindings in
    ret dict


  let get tuple idx : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* opt_int = as_constant_int idx in
    match opt_int with
    | None ->
        fresh ()
    | Some i ->
        let field = Fieldname.make tuple_tname (str_field_of_int i) in
        load_access ~deref:false tuple (FieldAccess field)
end

module PyModule = struct
  let make name : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    constructor ~deref:false (module_tname name) []
end

let build_tuple args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* tuple = Tuple.make args in
  assign_ret tuple


let build_class closure _name _args : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* class_ = Dict.make [] [] in
  let gen_closure_args _ = ret [class_] in
  let* _ = apply_python_closure closure gen_closure_args in
  assign_ret class_


let call_dsl ~closure ~arg_names:_ ~args : DSL.aval DSL.model_monad =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  let gen_closure_args opt_proc_attrs =
    let python_args =
      match opt_proc_attrs with
      | Some {ProcAttributes.python_args}
        when Int.equal (List.length python_args) (List.length args) ->
          python_args
      | Some {ProcAttributes.python_args} ->
          L.d_printfln "[ocaml model] %d argument required but %d were given"
            (List.length python_args) (List.length args) ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
      | None ->
          L.d_printfln "[ocaml model] Failed to load attributes" ;
          List.mapi args ~f:(fun i _ -> Printf.sprintf "arg_%d" i)
    in
    let* locals = Dict.make python_args args in
    ret [locals]
  in
  apply_python_closure closure gen_closure_args


let call closure arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* value = call_dsl ~closure ~arg_names ~args in
  assign_ret value


let await_awaitable arg : unit DSL.model_monad =
  fst arg |> AddressAttributes.await_awaitable |> DSL.Syntax.exec_command


(* Only Python frontend builtins ($builtins.py_) have a C-style syntax, so we
   must catch other specific calls here *)
let modelled_python_call module_name fun_name args : DSL.aval option DSL.model_monad =
  let open DSL.Syntax in
  match (module_name, fun_name, args) with
  | "asyncio", Some "run", [arg] ->
      let* () = await_awaitable arg in
      let* res = fresh () in
      ret (Some res)
  | "asyncio", Some "sleep", _ ->
      let* res = fresh () in
      let* () = allocation Attribute.Awaitable res in
      ret (Some res)
  | _, _, _ ->
      ret None


let call_method name obj arg_names args : model =
  (* TODO: take into account named args *)
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true obj in
  let* res =
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} when Typ.Name.is_python_module type_name
      -> (
        (* since module types are final, static type will save us most of the time *)
        let module_name = Typ.Name.get_python_module_name type_name |> Option.value_exn in
        let* opt_str_name = as_constant_string name in
        let* opt_special_call = modelled_python_call module_name opt_str_name args in
        match opt_special_call with
        | None ->
            L.d_printfln "calling method %a on module object %s" (Pp.option F.pp_print_string)
              opt_str_name module_name ;
            let* closure = Dict.get obj name in
            call_dsl ~closure ~arg_names ~args
        | Some res ->
            L.d_printfln "catching special call %a on module object %s"
              (Pp.option F.pp_print_string) opt_str_name module_name ;
            ret res )
    | _ ->
        let* closure = Dict.get obj name in
        (* TODO: for OO method, gives self argument *)
        call_dsl ~closure ~arg_names ~args
  in
  assign_ret res


let gen_start_coroutine : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let get_awaitable arg : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* () = await_awaitable arg in
  assign_ret arg


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
  let module_name =
    Option.value_or_thunk opt_str ~default:(fun () ->
        L.die InternalError "frontend should always give a string here" )
  in
  let class_name = PythonClassName.make module_name in
  let function_name = "__module_body__" in
  let proc_name = Procname.make_python ~class_name:(Some class_name) ~function_name in
  let* module_ = PyModule.make module_name in
  if IRAttributes.load proc_name |> Option.is_none then assign_ret module_
  else
    let* _ = python_call proc_name [("globals", module_)] in
    assign_ret module_


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
        constructor ~deref:false int_tname []
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
  let* none = constructor ~deref:false none_tname [] in
  assign_ret none


let nullify_locals locals names : model =
  let open DSL.Syntax in
  start_model
  @@ fun () ->
  let* zero = null in
  list_iter names ~f:(fun name -> Dict.set locals name zero)


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


let yield_from _ _ : model =
  let open DSL.Syntax in
  start_model @@ fun () -> ret ()


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let arg = capt_arg_payload in
  [ -"$builtins" &:: "py_build_class" <>$ arg $+ arg $+++$--> build_class
  ; -"$builtins" &:: "py_call" <>$ arg $+ arg $+++$--> call
  ; -"$builtins" &:: "py_call_method" <>$ arg $+ arg $+ arg $+++$--> call_method
  ; -"$builtins" &:: "py_build_tuple" &::.*+++> build_tuple
  ; -"$builtins" &:: "py_gen_start_coroutine" <>--> gen_start_coroutine
  ; -"$builtins" &:: "py_get_awaitable" <>$ arg $--> get_awaitable
  ; -"$builtins" &:: "py_import_from" <>$ arg $+ arg $--> import_from
  ; -"$builtins" &:: "py_import_name" <>$ arg $+ arg $+ arg $--> import_name
  ; -"$builtins" &:: "py_load_fast" <>$ arg $+ arg $--> load_fast
  ; -"$builtins" &:: "py_load_global" <>$ arg $+ arg $--> load_global
  ; -"$builtins" &:: "py_load_name" <>$ arg $+ arg $+ arg $--> load_name
  ; -"$builtins" &:: "py_make_dictionary" &::.*+++> make_dictionary
  ; -"$builtins" &:: "py_make_function" <>$ arg $+ arg $+ arg $+ arg $+ arg $--> make_function
  ; -"$builtins" &:: "py_make_int" <>$ arg $--> make_int
  ; -"$builtins" &:: "py_make_none" <>--> make_none
  ; -"$builtins" &:: "py_nullify_locals" <>$ arg $+++$--> nullify_locals
  ; -"$builtins" &:: "py_subscript" <>$ arg $+ arg $--> subscript
  ; -"$builtins" &:: "py_store_fast" <>$ arg $+ arg $+ arg $--> store_fast
  ; -"$builtins" &:: "py_store_global" <>$ arg $+ arg $+ arg $--> store_global
  ; -"$builtins" &:: "py_store_name" <>$ arg $+ arg $+ arg $+ arg $--> store_name
  ; -"$builtins" &:: "py_yield_from" <>$ arg $+ arg $--> yield_from ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
