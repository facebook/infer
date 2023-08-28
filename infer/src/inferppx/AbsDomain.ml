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


(* [var.field] *)
let access ~loc var field_lid =
  let var_lid = Loc.make ~loc (Longident.Lident var) in
  Ast_helper.Exp.field ~loc (Ast_helper.Exp.ident ~loc var_lid) field_lid


(* [Field.join lhs.field rhs.field] *)
let create_initializer ~loc (ld : label_declaration) =
  let field_lid = Loc.make ~loc (Longident.Lident ld.pld_name.txt) in
  let lhs_access = access ~loc "lhs" field_lid in
  let rhs_access = access ~loc "rhs" field_lid in
  let func_lid = Loc.make ~loc (join_of_core_type ld.pld_type.ptyp_desc) in
  let func_name =
    Ast_helper.Exp.ident
    (* ~attrs:[Ast_helper.Attr.mk {loc; txt= "ocaml.doc"} (PStr [%str [%e b]])] *)
      ~loc func_lid
  in
  [%expr [%e func_name] [%e lhs_access] [%e rhs_access]]


(* [let field = join lhs.field rhs.field in acc] *)
let let_field_equal_join_expr ~loc acc ld =
  let join_call = create_initializer ~loc ld in
  let field_pat = Ast_helper.Pat.var ~loc ld.pld_name in
  let vb = Ast_helper.Vb.mk ~loc field_pat join_call in
  let let_exp = Ast_helper.Exp.let_ ~loc Nonrecursive [vb] acc in
  let_exp


(* [phys_equal field var.field] *)
let phys_equal_field ~loc var ld =
  let field_lid = Loc.make ~loc (Longident.Lident ld.pld_name.txt) in
  let field_exp = field_lid |> Ast_helper.Exp.ident ~loc in
  let access = access ~loc var field_lid in
  [%expr phys_equal [%e field_exp] [%e access]]


(* conjunction of [phys_equal] over all fields *)
let phys_equal_fields ~loc var lds =
  let phys_equal_exps = List.map lds ~f:(phys_equal_field ~loc var) in
  let f acc exp =
    match acc with None -> Some exp | Some exp_acc -> Some [%expr [%e exp] && [%e exp_acc]]
  in
  List.fold phys_equal_exps ~init:None ~f |> Option.value_exn


(* [if (phys_equal a lhs.a && phys_equal b lhs.b && ...) then lhs else else_exp] *)
let if_phys_equal_then_var ~loc var lds else_exp =
  let guard = phys_equal_fields ~loc var lds in
  let var_lid = Loc.make ~loc (Longident.Lident var) |> Ast_helper.Exp.ident ~loc in
  let then_exp = [%expr [%e var_lid]] in
  Ast_helper.Exp.ifthenelse ~loc guard then_exp (Some else_exp)


(* record expression [{ a=a; b=b; c=c; ...}] *)
let create_record ~loc lds =
  let field_lids = List.map lds ~f:(fun ld -> Loc.make ~loc (Longident.Lident ld.pld_name.txt)) in
  let field_exps = List.map field_lids ~f:(fun fld -> Ast_helper.Exp.ident ~loc fld) in
  let initializers = List.zip_exn field_lids field_exps in
  Ast_helper.Exp.record ~loc initializers None


let join_impl ~loc (lds : label_declaration list) =
  let fun_label = Loc.make ~loc "join" in
  let record_exp = create_record ~loc lds in
  let guarded =
    if_phys_equal_then_var ~loc "rhs" lds record_exp |> if_phys_equal_then_var ~loc "lhs" lds
  in
  let final_expr = List.fold lds ~init:guarded ~f:(let_field_equal_join_expr ~loc) in
  let body = [%expr fun lhs rhs -> [%e final_expr]] in
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
