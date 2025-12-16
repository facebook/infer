(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let to_textual_type_name lang ?plain_name name =
  if Textual.Lang.is_swift lang then Textual.TypeName.mk_swift_type_name ?plain_name name
  else Textual.TypeName.of_string name


let mangled_name_of_type_name (type_name : Textual.TypeName.t) =
  if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_type_name then
    match type_name.args with
    | {name; args= []} :: _ ->
        Some (Textual.BaseTypeName.to_string name)
    | _ ->
        None
  else None


let plain_name_of_type_name (type_name : Textual.TypeName.t) =
  if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_type_name then
    match type_name.args with
    | _ :: [{name; args= []}] ->
        Some (Textual.BaseTypeName.to_string name)
    | _ ->
        None
  else None


let rec update_type_name_with_plain_name ~plain_name (type_name : Textual.TypeName.t) =
  if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_type_name then
    match type_name.args with
    | [{name; args= []}] ->
        Textual.TypeName.mk_swift_type_name ~plain_name (Textual.BaseTypeName.to_string name)
    | _ ->
        type_name
  else if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_tuple_class_name then
    let args = List.map ~f:(update_type_name_with_plain_name ~plain_name) type_name.args in
    Textual.TypeName.mk_swift_tuple_type_name args
  else type_name


let rec update_type_name_with_mangled_name ~mangled_name (type_name : Textual.TypeName.t) =
  if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_type_name then
    match type_name.args with
    | _ :: [{name; args= []}] ->
        Textual.TypeName.mk_swift_type_name
          ~plain_name:(Textual.BaseTypeName.to_string name)
          mangled_name
    | _ ->
        type_name
  else if Textual.BaseTypeName.equal type_name.name Textual.BaseTypeName.swift_tuple_class_name then
    let args = List.map ~f:(update_type_name_with_mangled_name ~mangled_name) type_name.args in
    Textual.TypeName.mk_swift_tuple_type_name args
  else type_name


let struct_name_of_mangled_name lang struct_map name =
  let class_opt = ref None in
  let _ =
    Textual.TypeName.Map.exists
      (fun struct_name _ ->
        match mangled_name_of_type_name struct_name with
        | Some mangled_name when String.equal mangled_name name ->
            class_opt := Some struct_name ;
            true
        | _ ->
            false )
      struct_map
  in
  match !class_opt with None -> to_textual_type_name lang name | Some class_ -> class_


let struct_name_of_plain_name struct_map name =
  let class_opt = ref None in
  let _ =
    Textual.TypeName.Map.exists
      (fun struct_name _ ->
        match plain_name_of_type_name struct_name with
        | Some plain_name when String.equal plain_name name ->
            class_opt := Some struct_name ;
            true
        | _ ->
            false )
      struct_map
  in
  !class_opt
