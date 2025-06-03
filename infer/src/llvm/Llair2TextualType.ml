(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! Llair
module L = Logging
module ProcState = Llair2TextualProcState

let add_struct_to_map name struct_ structMap =
  if Option.is_none (Textual.TypeName.Map.find_opt name structMap) then
    Textual.TypeName.Map.add name struct_ structMap
  else structMap


let field_of_pos pos = Format.asprintf "field_%s" (Int.to_string pos)

let tuple_field_of_pos pos =
  let name = Format.asprintf "__infer_tuple_field_%s" (Int.to_string pos) in
  Textual.{enclosing_class= Textual.TypeName.swift_tuple_class_name; name= FieldName.of_string name}


let rec translate_struct ?struct_map name elements =
  let struct_name = Textual.TypeName.of_string name in
  let fields = to_textual_field_decls ?struct_map struct_name elements in
  let struct_ = {Textual.Struct.name= struct_name; supers= []; fields; attributes= []} in
  (struct_name, struct_)


and to_textual_field_decls ?struct_map struct_name fields =
  let to_textual_field_decl pos (_, typ) =
    let name = field_of_pos pos in
    let qualified_name = Textual.{enclosing_class= struct_name; name= FieldName.of_string name} in
    let textual_typ = to_textual_typ ?struct_map typ in
    Textual.FieldDecl.{qualified_name; typ= textual_typ; attributes= []}
  in
  let fields = StdUtils.iarray_to_list fields in
  List.mapi ~f:to_textual_field_decl fields


and to_textual_typ ?struct_map (typ : Llair.Typ.t) =
  match typ with
  | Function {return; args} ->
      let params_type = StdUtils.iarray_to_list args |> List.map ~f:(to_textual_typ ?struct_map) in
      let return_type =
        Option.value_map ~f:(to_textual_typ ?struct_map) return ~default:Textual.Typ.Void
      in
      Textual.Typ.Fun (Some {params_type; return_type})
  | Integer _ ->
      Textual.Typ.Int
  | Float _ ->
      Textual.Typ.Float
  | Pointer {elt} ->
      Textual.Typ.Ptr (to_textual_typ ?struct_map elt)
  | Array {elt} ->
      Textual.Typ.Array (to_textual_typ ?struct_map elt)
  | Tuple _ ->
      Textual.Typ.(Ptr (Struct Textual.TypeName.swift_tuple_class_name))
  | Struct {name; elts} ->
      let struct_name =
        if Option.is_none struct_map then fst (translate_struct ?struct_map name elts)
        else Textual.TypeName.of_string name
      in
      Textual.Typ.Struct struct_name
  | Opaque {name} ->
      (* From llair's docs: Uniquely named aggregate type whose definition is hidden. *)
      let struct_name = Textual.TypeName.of_string name in
      Textual.Typ.Struct struct_name


let to_annotated_textual_typ ~struct_map llair_typ =
  let typ = to_textual_typ ~struct_map llair_typ in
  {typ; Textual.Typ.attributes= []}


let translate_types_env (types_defns : Llair.Typ.t list) =
  let translate_types_defn structMap (typ : Llair.Typ.t) =
    match typ with
    | Struct {name: string; elts} ->
        let struct_name, struct_ = translate_struct name elts in
        add_struct_to_map struct_name struct_ structMap
    | Opaque _ ->
        structMap
    | _ ->
        L.die InternalError "Unexpected type %a found in llair's type environment@." Llair.Typ.pp
          typ
  in
  List.fold ~f:translate_types_defn types_defns ~init:Textual.TypeName.Map.empty


let type_inference ~proc_state instrs =
  let type_inference instr =
    match (instr : Textual.Instr.t) with
    | Load {id; exp} -> (
      match ProcState.get_local_or_formal_type ~proc_state (Var id) with
      | Some typ_annot ->
          ProcState.update_local_or_formal_type ~typ_modif:PtrModif ~proc_state exp typ_annot.typ
      | _ ->
          () )
    | Store {exp1; exp2} -> (
      match ProcState.get_local_or_formal_type ~proc_state exp1 with
      | Some typ_annot ->
          let typ_modif : ProcState.typ_modif =
            match exp1 with Var _ -> PtrModif | _ -> NoModif
          in
          ProcState.update_local_or_formal_type ~typ_modif ~proc_state exp2 typ_annot.typ
      | _ ->
          () )
    | _ ->
        ()
  in
  List.iter ~f:type_inference (List.rev instrs)


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
