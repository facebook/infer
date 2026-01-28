(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair
module L = Logging
module Field = Llair2TextualField
module TypeName = Llair2TextualTypeName
module ProcState = Llair2TextualState

let type_name_of_type typ = Textual.TypeName.of_string (Format.asprintf "%a" Textual.Typ.pp typ)

let field_of_pos type_name pos =
  let name = Format.asprintf "field_%s" (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}


let tuple_field_prefix = "__infer_tuple_field_"

let tuple_field_of_pos type_name pos =
  let name = Format.sprintf "%s%s" tuple_field_prefix (Int.to_string pos) in
  Textual.{enclosing_class= type_name; name= FieldName.of_string name}


let rec to_textual_typ lang ~mangled_map ~struct_map (typ : Llair.Typ.t) =
  match typ with
  | Function {return; args} ->
      let params_type =
        StdUtils.iarray_to_list args |> List.map ~f:(to_textual_typ lang ~mangled_map ~struct_map)
      in
      let return_type =
        Option.value_map return ~default:Textual.Typ.Void
          ~f:(to_textual_typ lang ~mangled_map ~struct_map)
      in
      Textual.Typ.Fun (Some {params_type; return_type})
  | Integer _ ->
      Textual.Typ.Int
  | Float _ ->
      Textual.Typ.Float
  | Pointer {elt} ->
      Textual.Typ.Ptr (to_textual_typ lang ~mangled_map ~struct_map elt)
  | Array {elt} ->
      (* often the type we want is ptr rather than array, and it's not always clear from llvm,
         when they really mean an array. This is a better heuristic. *)
      Textual.Typ.Ptr (to_textual_typ lang ~mangled_map ~struct_map elt)
  | Tuple {elts} ->
      let tuple_name = to_textual_tuple_name lang ~mangled_map ~struct_map elts in
      Textual.Typ.(Ptr (Struct tuple_name))
  | Struct {name} ->
      let struct_name = TypeName.struct_name_of_mangled_name lang ~mangled_map struct_map name in
      if Textual.Lang.is_c lang then Textual.Typ.Struct struct_name
      else Textual.Typ.(Ptr (Struct struct_name))
  | Opaque {name} ->
      (* From llair's docs: Uniquely named aggregate type whose definition is hidden. *)
      let struct_name = TypeName.struct_name_of_mangled_name lang ~mangled_map struct_map name in
      Textual.Typ.Struct struct_name


and to_textual_tuple_name lang ~mangled_map ~struct_map elements =
  let elts = StdUtils.iarray_to_list elements in
  let _, typs = List.sort ~compare:(fun (n1, _) (n2, _) -> Int.compare n1 n2) elts |> List.unzip in
  let textual_types =
    List.map
      ~f:(fun typ ->
        let textual_typ = to_textual_typ lang ~mangled_map ~struct_map typ in
        type_name_of_type textual_typ )
      typs
  in
  Textual.TypeName.mk_swift_tuple_type_name textual_types


let to_textual_field_decls lang ~struct_map ~tuple struct_name fields =
  let to_textual_field_decl pos (_, typ) =
    let qualified_name =
      if tuple then Field.tuple_field_of_pos struct_name pos else Field.field_of_pos struct_name pos
    in
    let textual_typ = to_textual_typ lang ~mangled_map:None ~struct_map typ in
    let attributes, textual_typ =
      match Textual.TypeName.swift_mangled_name_of_type_name struct_name with
      | Some "Any" when Int.equal pos 0 ->
          (* For AnyObject, the field_0 should be a generic pointer, but we get int* from llvm *)
          ([], Textual.Typ.Ptr Textual.Typ.any_type_swift)
      | _ -> (
        match textual_typ with
        | Textual.Typ.(Ptr (Struct name)) -> (
          match Textual.TypeName.swift_mangled_name_of_type_name name with
          | Some mangled_name when String.equal mangled_name "swift::weak" ->
              let textual_typ = Textual.Typ.(Ptr Textual.Typ.any_type_swift) in
              ([Textual.Attr.mk_weak], textual_typ)
          | _ ->
              ([], textual_typ) )
        | _ ->
            ([], textual_typ) )
    in
    Textual.FieldDecl.{qualified_name; typ= textual_typ; attributes}
  in
  let fields = StdUtils.iarray_to_list fields in
  List.mapi ~f:to_textual_field_decl fields


let translate_struct lang ~struct_map ~tuple struct_name elements =
  let fields = to_textual_field_decls lang ~tuple ~struct_map struct_name elements in
  let struct_ = {Textual.Struct.name= struct_name; supers= []; fields; attributes= []} in
  struct_


let is_ptr_struct typ =
  match typ with
  | Textual.Typ.Ptr (Textual.Typ.Struct name) -> (
    match Textual.TypeName.swift_mangled_name_of_type_name name with
    | Some mangled_name ->
        String.is_suffix ~suffix:"V" mangled_name
    | None ->
        false )
  | _ ->
      false


let lookup_field_type ~struct_map struct_name field_name =
  let struct_ = Textual.TypeName.Map.find_opt struct_name struct_map in
  match struct_ with
  | None ->
      None
  | Some struct_ ->
      let field =
        List.find
          ~f:(fun field ->
            Textual.equal_qualified_fieldname field.Textual.FieldDecl.qualified_name field_name )
          struct_.Textual.Struct.fields
      in
      Option.map ~f:(fun field -> field.Textual.FieldDecl.typ) field


let translate_types_env lang (types_defns : Llair.Typ.t list) =
  let translate_types_defn struct_map (typ : Llair.Typ.t) =
    match typ with
    | Struct {name; elts} ->
        let struct_name = TypeName.to_textual_type_name lang name in
        let struct_ = translate_struct ~struct_map lang ~tuple:false struct_name elts in
        Textual.TypeName.Map.add struct_name struct_ struct_map
    | Tuple {elts} ->
        let tuple_name = to_textual_tuple_name ~mangled_map:None ~struct_map lang elts in
        let struct_ = translate_struct ~struct_map lang ~tuple:true tuple_name elts in
        Textual.TypeName.Map.add tuple_name struct_ struct_map
    | Opaque _ ->
        struct_map
    | _ ->
        L.die InternalError "Unexpected type %a found in llair's type environment@." Llair.Typ.pp
          typ
  in
  List.fold ~f:translate_types_defn types_defns ~init:Textual.TypeName.Map.empty


let rec join (typ1 : Textual.Typ.t) (typ2 : Textual.Typ.t) : Textual.Typ.t =
  match (typ1, typ2) with
  | Int, Int ->
      Int
  | Float, Float ->
      Float
  | Ptr typ1, Ptr typ2 ->
      Ptr (join typ1 typ2)
  | Array typ1, Array typ2 ->
      Array (join typ1 typ2)
  | Textual.Typ.Struct name1, Textual.Typ.Struct name2 ->
      if Textual.TypeName.equal name1 name2 then Textual.Typ.Struct name1 else assert false
  | ( Textual.Typ.Fun (Some {params_type= params_type1; return_type= return_type1})
    , Textual.Typ.Fun (Some {params_type= params_type2; return_type= return_type2}) ) ->
      Textual.Typ.Fun
        (Some
           { params_type= List.map2_exn ~f:join params_type1 params_type2
           ; return_type= join return_type1 return_type2 } )
  | Null, Null ->
      Null
  | Void, Void ->
      Void
  | _ ->
      assert false


let join_typ typ1_opt typ2_opt =
  match (typ1_opt, typ2_opt) with
  | Some typ1, Some typ2 ->
      Some (join typ1 typ2)
  | Some typ1, None ->
      Some typ1
  | None, Some typ2 ->
      Some typ2
  | None, None ->
      None


let rec is_compatible typ1 typ2 =
  match (typ1, typ2) with
  | Textual.Typ.Struct _, Textual.Typ.Struct _ ->
      true
  | Textual.Typ.Ptr typ1, Textual.Typ.Ptr typ2 ->
      is_compatible typ1 typ2
  | ( Textual.Typ.Fun (Some {params_type= params_type1; return_type= return_type1})
    , Textual.Typ.Fun (Some {params_type= params_type2; return_type= return_type2}) ) ->
      List.for_all2_exn ~f:is_compatible params_type1 params_type2
      && is_compatible return_type1 return_type2
  | Textual.Typ.Int, Textual.Typ.Int ->
      true
  | Textual.Typ.Float, Textual.Typ.Float ->
      true
  | Textual.Typ.Null, Textual.Typ.Null ->
      true
  | Textual.Typ.Void, Textual.Typ.Void ->
      true
  | _ ->
      false


let rec signature_type_to_textual_typ signature_structs lang signature_type =
  if String.is_suffix signature_type ~suffix:"*" then
    let name = String.chop_suffix_if_exists signature_type ~suffix:"*" in
    match signature_type_to_textual_typ signature_structs lang name with
    | Some typ ->
        Some (Textual.Typ.Ptr typ)
    | None ->
        None
  else if String.equal signature_type "Int" then Some Textual.Typ.Int
  else if String.equal signature_type "<unknown>" || String.is_empty signature_type then None
  else if String.equal signature_type "$sytD" then Some Textual.Typ.Void
  else
    let struct_name =
      if Textual.Lang.is_swift lang then (* optional type *)
        if String.is_suffix signature_type ~suffix:"SgD" then
          let type_name = String.chop_suffix_if_exists signature_type ~suffix:"SgD" in
          let type_name = String.substr_replace_first type_name ~pattern:"$s" ~with_:"T" in
          TypeName.to_textual_type_name lang type_name
        else (
          Hash_set.add signature_structs signature_type ;
          TypeName.to_textual_type_name lang ~plain_name:signature_type "" )
      else TypeName.to_textual_type_name lang signature_type
    in
    if Textual.Lang.is_c lang then Some (Textual.Typ.Struct struct_name)
    else Some Textual.Typ.(Ptr (Textual.Typ.Struct struct_name))


let pp_signature_structs fmt signature_structs =
  let pp_item fmt key = Format.fprintf fmt "%s@." key in
  Hash_set.iter signature_structs ~f:(pp_item fmt)


let update_struct_name signature_structs struct_name =
  match Textual.TypeName.swift_mangled_name_of_type_name struct_name with
  | Some typ_name
    when String.is_suffix ~suffix:"C" typ_name || String.is_suffix ~suffix:"V" typ_name -> (
      (* we only want to find the plain name of classes or structs *)
      let f signature_struct = String.is_substring ~substring:signature_struct typ_name in
      match Hash_set.find ~f signature_structs with
      | Some signature_struct ->
          let struct_name =
            TypeName.update_type_name_with_plain_name ~plain_name:signature_struct struct_name
          in
          struct_name
      | None ->
          struct_name )
  | _ ->
      struct_name


let rec update_type ~update_struct_name typ =
  match typ with
  | Textual.Typ.Struct struct_name ->
      let struct_name = update_struct_name struct_name in
      Textual.Typ.Struct struct_name
  | Textual.Typ.Ptr typ ->
      Textual.Typ.Ptr (update_type ~update_struct_name typ)
  | Textual.Typ.Fun (Some {params_type; return_type}) ->
      Textual.Typ.Fun
        (Some
           { params_type= List.map ~f:(update_type ~update_struct_name) params_type
           ; return_type= (update_type ~update_struct_name) return_type } )
  | _ ->
      typ


let update_type_field_decl ~update_struct_name fields =
  let update_field_decl field =
    let typ = update_type ~update_struct_name field.Textual.FieldDecl.typ in
    let enclosing_class =
      update_struct_name field.Textual.FieldDecl.qualified_name.enclosing_class
    in
    { field with
      Textual.FieldDecl.typ
    ; qualified_name= Textual.{enclosing_class; name= field.FieldDecl.qualified_name.name} }
  in
  List.map ~f:update_field_decl fields


let update_struct_map signature_structs struct_map =
  let update_struct_name x = update_struct_name signature_structs x in
  let update_struct_map struct_name (Textual.Struct.{fields: _} as struct_) struct_map =
    let new_struct_name = update_struct_name struct_name in
    let struct_ =
      { struct_ with
        Textual.Struct.fields= update_type_field_decl ~update_struct_name fields
      ; name= new_struct_name }
    in
    Textual.TypeName.Map.add new_struct_name struct_ struct_map
  in
  Textual.TypeName.Map.fold update_struct_map struct_map Textual.TypeName.Map.empty


(** remove optional type from the signature *)
let struct_name_of_mangled_name lang ~mangled_map struct_map name =
  TypeName.struct_name_of_mangled_name lang ~mangled_map:(Some mangled_map) struct_map name


let update_signature_type lang ~mangled_map ~struct_map ~plain_map type_name =
  (let open IOption.Let_syntax in
   match Textual.TypeName.swift_plain_name_of_type_name type_name with
   | Some plain_name ->
       let* struct_name = TypeName.struct_name_of_plain_name plain_map plain_name in
       let+ mangled_name = Textual.TypeName.swift_mangled_name_of_type_name struct_name in
       TypeName.update_type_name_with_mangled_name ~mangled_name type_name
   | None ->
       let* mangled_name = Textual.TypeName.swift_mangled_name_of_type_name type_name in
       let struct_name = struct_name_of_mangled_name lang ~mangled_map struct_map mangled_name in
       let+ plain_name = Textual.TypeName.swift_plain_name_of_type_name struct_name in
       TypeName.update_type_name_with_plain_name ~plain_name struct_name )
  |> Option.value ~default:type_name


let update_signature_types lang ~mangled_map ~struct_map ~plain_map formal_types return_type =
  let update_signature_type typ =
    let typ =
      update_type typ.Textual.Typ.typ
        ~update_struct_name:(update_signature_type lang ~mangled_map ~struct_map ~plain_map)
    in
    Textual.Typ.mk_without_attributes typ
  in
  let update formal_types = List.map ~f:update_signature_type formal_types in
  let formal_types = Option.map ~f:update formal_types in
  let return_type = update_signature_type return_type in
  (formal_types, return_type)


let to_annotated_textual_typ lang ~mangled_map ~struct_map llair_typ =
  let typ = to_textual_typ lang ~mangled_map ~struct_map llair_typ in
  {typ; Textual.Typ.attributes= []}


let to_textual_typ_without_mangled_map lang ~struct_map llair_typ =
  to_textual_typ lang ~mangled_map:None ~struct_map llair_typ


let to_textual_typ lang ~mangled_map ~struct_map llair_typ =
  to_textual_typ lang ~mangled_map:(Some mangled_map) ~struct_map llair_typ


let to_annotated_textual_typ_without_mangled_map lang ~struct_map llair_typ =
  to_annotated_textual_typ lang ~mangled_map:None ~struct_map llair_typ


let to_annotated_textual_typ lang ~mangled_map ~struct_map llair_typ =
  to_annotated_textual_typ lang ~mangled_map:(Some mangled_map) ~struct_map llair_typ


(** Update struct field names using the field offset map. This replaces generic field_N names with
    actual field names from getter/setter methods. *)
let update_struct_map_with_field_names field_offset_map struct_map =
  let update_field_name struct_name pos field =
    let key = ProcState.FieldOffset.{class_name= struct_name; offset= pos} in
    match ProcState.FieldOffsetMap.find_opt field_offset_map key with
    | Some field_name ->
        { field with
          Textual.FieldDecl.qualified_name= {enclosing_class= struct_name; name= field_name} }
    | None ->
        field
  in
  let update_struct struct_name (struct_ : Textual.Struct.t) =
    let fields = List.mapi ~f:(update_field_name struct_name) struct_.fields in
    {struct_ with fields}
  in
  Textual.TypeName.Map.mapi update_struct struct_map
