(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair
module State = Llair2TextualState
module TypeName = Llair2TextualTypeName

let get_suffix = ".get"

let set_suffix = ".set"

let modify_suffix = ".modify"

module OffsetIndex = struct
  (** Extract field name from getter method's unmangled name. E.g., "age.get" -> Some "age", "foo"
      -> None *)

  let extract_field_name_from_getter plain_name = String.chop_suffix ~suffix:get_suffix plain_name

  (** Recursively traverse expression to find the innermost Select with offset and class type. For
      nested types like %ptr_elt*[1]:%T5Hello6PersonC[0]:%TSi, we want offset 1 (PersonC) which is
      the innermost Select in the expression tree. *)
  let rec extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func
      (exp : Llair.Exp.t) =
    let formals_types = func.formals_types |> StdUtils.iarray_to_list in
    let formals = func.formals |> StdUtils.iarray_to_list in
    match exp with
    | Ap1 (Select offset, typ, inner_exp) -> (
      (* First recurse to find innermost Select *)
      match
        extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func inner_exp
      with
      | Some _ as some_result ->
          (* Use the innermost result *)
          some_result
      | None -> (
        (* This is the innermost Select, use it if it has a struct type *)
        match (typ, inner_exp, formals, formals_types) with
        | Llair.Typ.Struct {name; _}, _, _, _
          when not (String.equal Textual.BaseTypeName.swift_any_type_name.value name) ->
            Some
              ( TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map
                  name
              , offset )
        | _, Reg {id}, [arg], [arg_typ] when Int.equal id (Reg.id arg) ->
            Option.map
              ~f:(fun typ -> (typ, offset))
              (TypeName.struct_name_of_plain_name plain_map arg_typ)
        | _ ->
            None ) )
    | Ap1 (_, _, inner_exp) ->
        extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func inner_exp
    | Ap2 (_, _, exp1, exp2) -> (
      match extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp1 with
      | Some _ as some_result ->
          some_result
      | None ->
          extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp2 )
    | Ap3 (_, _, exp1, exp2, exp3) -> (
      match extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp1 with
      | Some _ as some_result ->
          some_result
      | None -> (
        match
          extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp2
        with
        | Some _ as some_result ->
            some_result
        | None ->
            extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp3 ) )
    | _ ->
        None


  (** Extract the first instance of offset and class type from instructions in a function *)
  let extract_offset_from_instructions lang ~mangled_map ~plain_map struct_map func cmnd =
    let instrs = StdUtils.iarray_to_list cmnd in
    List.find_map instrs ~f:(fun inst ->
        match inst with
        | Llair.Load {ptr; _} ->
            extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func ptr
        | Move {reg_exps: (Reg.t * Exp.t) NS.iarray} ->
            let exps = StdUtils.iarray_to_list reg_exps in
            List.find_map exps ~f:(fun (_, exp) ->
                extract_offset_and_type_from_exp lang ~mangled_map ~plain_map struct_map func exp )
        | _ ->
            None )


  (** Build the field offset map from getter/setter functions. This maps (class_name, offset) ->
      field_name by analyzing getter/setter methods. *)
  let build_field_offset_map lang ~mangled_map ~plain_map struct_map functions =
    let field_offset_map = State.FieldOffsetMap.create 64 in
    let process_function (func_name, func) =
      match FuncName.unmangled_name func_name with
      | None ->
          ()
      | Some plain_name -> (
        match extract_field_name_from_getter plain_name with
        | None ->
            ()
        | Some field_name -> (
            let entry = func.Llair.entry in
            match
              extract_offset_from_instructions lang ~mangled_map ~plain_map struct_map func
                entry.cmnd
            with
            | Some (class_name, offset) ->
                let key = State.FieldOffset.{class_name; offset} in
                State.FieldOffsetMap.replace field_offset_map key
                  (Textual.FieldName.of_string field_name)
            | None ->
                () ) )
    in
    List.iter functions ~f:process_function ;
    field_offset_map
end

let field_of_pos type_name pos =
  let name = Format.asprintf "field_%s" (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}


let field_of_pos_with_map field_offset_map type_name pos =
  let key = State.FieldOffset.{class_name= type_name; offset= pos} in
  match State.FieldOffsetMap.find_opt field_offset_map key with
  | Some field_name ->
      Textual.{enclosing_class= type_name; name= field_name}
  | None ->
      field_of_pos type_name pos


let tuple_field_prefix = "__infer_tuple_field_"

let tuple_field_of_pos type_name pos =
  let name = Format.sprintf "%s%s" tuple_field_prefix (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}
