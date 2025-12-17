(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module State = Llair2TextualState

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


let struct_name_of_mangled_name lang ~mangled_map struct_map name =
  let struct_name_of_mangled_name_inner lang struct_map name =
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
  in
  match mangled_map with
  | None ->
      struct_name_of_mangled_name_inner lang struct_map name
  | Some mangled_map -> (
    match IString.Map.find_opt name mangled_map with
    | Some struct_name ->
        struct_name
    | None ->
        to_textual_type_name lang name )


let compute_mangled_map struct_map =
  let mangled_map = IString.Map.empty in
  let add_mangled_name struct_name _ acc =
    match mangled_name_of_type_name struct_name with
    | Some mangled_name ->
        IString.Map.add mangled_name struct_name acc
    | None ->
        acc
  in
  Textual.TypeName.Map.fold add_mangled_name struct_map mangled_map


let struct_name_of_plain_name plain_map name = IString.Map.find_opt name plain_map

let compute_plain_map struct_map =
  let add_plain_name struct_name _ acc =
    match plain_name_of_type_name struct_name with
    | None ->
        acc
    | Some plain_name ->
        IString.Map.update plain_name
          (function
            | None ->
                Some struct_name
            | Some _ as some ->
                (* this means there is a plain name mapping to more than one type *)
                some )
          acc
  in
  Textual.TypeName.Map.fold add_plain_name struct_map IString.Map.empty
