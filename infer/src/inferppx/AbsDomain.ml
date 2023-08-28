(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let rec join_of_longident = function
  | Ldot (l, "t") ->
      Ldot (join_of_longident l, "join")
  | Ldot (l, s) ->
      Ldot (join_of_longident l, s)
  | Lident _ as l ->
      l
  | Lapply _ ->
      assert false


let join_of_core_type = function
  | Ptyp_constr (l, _) ->
      join_of_longident l.txt
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_arrow _
  | Ptyp_tuple _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_variant _
  | Ptyp_poly _
  | Ptyp_package _
  | Ptyp_extension _ ->
      assert false


let create_initializer ~loc (ld : label_declaration) =
  let field_lid = Loc.make ~loc (Longident.Lident ld.pld_name.txt) in
  let lhs_access = Ast_helper.Exp.field ~loc [%expr lhs] field_lid in
  let rhs_access = Ast_helper.Exp.field ~loc [%expr rhs] field_lid in
  let func_lid = Loc.make ~loc (join_of_core_type ld.pld_type.ptyp_desc) in
  let func_name =
    Ast_helper.Exp.ident
    (* ~attrs:[Ast_helper.Attr.mk {loc; txt= "ocaml.doc"} (PStr [%str [%e b]])] *)
      ~loc func_lid
  in
  let join_call = [%expr [%e func_name] [%e lhs_access] [%e rhs_access]] in
  (field_lid, join_call)


let join_impl ~loc (lds : label_declaration list) =
  let fun_label = Loc.make ~loc "join" in
  let initializer_list = List.map lds ~f:(create_initializer ~loc) in
  let record_expr = Ast_helper.Exp.record ~loc initializer_list None in
  let body = [%expr fun lhs rhs -> [%e record_expr]] in
  let fn = Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc fun_label) body in
  Ast_helper.Str.value ~loc Nonrecursive [fn]


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {ptype_kind= Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _} ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc "Cannot derive join for non record types"
          in
          [Ast_builder.Default.pstr_extension ~loc ext []]
      | {ptype_kind= Ptype_record fields; _} ->
          [join_impl ~loc fields] )
  |> List.concat


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "absdom" ~str_type_decl
