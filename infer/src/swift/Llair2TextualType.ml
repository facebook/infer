(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair

type structMap = Textual.Struct.t Textual.TypeName.Map.t

let structMap : structMap ref = ref Textual.TypeName.Map.empty

let addStructToMap name struct_ =
  if Option.is_none (Textual.TypeName.Map.find_opt name !structMap) then
    structMap := Textual.TypeName.Map.add name struct_ !structMap


let field_of_pos pos = Format.asprintf "field_%s" (Int.to_string pos)

let rec to_textual_field_decls struct_name fields =
  let to_textual_field_decl (pos, typ) =
    let name = field_of_pos pos in
    let qualified_name = Textual.{enclosing_class= struct_name; name= FieldName.of_string name} in
    let textual_typ = to_textual_typ typ in
    Textual.FieldDecl.{qualified_name; typ= textual_typ; attributes= []}
  in
  let fields = StdUtils.iarray_to_list fields in
  List.map ~f:to_textual_field_decl fields


and to_textual_typ (typ : Llair.Typ.t) =
  match typ with
  | Function {return; args} ->
      let params_type = StdUtils.iarray_to_list args |> List.map ~f:to_textual_typ in
      let return_type = Option.value_map ~f:to_textual_typ return ~default:Textual.Typ.Void in
      Textual.Typ.Fun (Some {params_type; return_type})
  | Integer _ ->
      Textual.Typ.Int
  | Float _ ->
      Textual.Typ.Float
  | Pointer {elt} ->
      Textual.Typ.Ptr (to_textual_typ elt)
  | Array {elt} ->
      Textual.Typ.Array (to_textual_typ elt)
  | Tuple _ ->
      Textual.Typ.Void
      (* TODO: give this an annonymous name and add this struct to our type definitions *)
  | Struct {name; elts} ->
      let struct_name = Textual.TypeName.of_string name in
      let fields = to_textual_field_decls struct_name elts in
      let struct_ = {Textual.Struct.name= struct_name; supers= []; fields; attributes= []} in
      addStructToMap struct_name struct_ ;
      Textual.Typ.Struct struct_name
  | Opaque {name} ->
      (* From llair's docs: Uniquely named aggregate type whose definition is hidden. *)
      Textual.Typ.Struct (Textual.TypeName.of_string name)


let to_annotated_textual_typ llair_typ =
  let typ = to_textual_typ llair_typ in
  {typ; Textual.Typ.attributes= []}
