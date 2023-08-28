(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let rec fun_of_longident fun_name lid =
  match lid with
  | Ldot (l, "t") ->
      Ldot (fun_of_longident fun_name l, fun_name)
  | Ldot (l, s) ->
      Ldot (fun_of_longident fun_name l, s)
  | Lident _ as l ->
      l
  | Lapply _ ->
      assert false


let fun_of_core_type ~loc fun_name ct =
  match ct with
  | Ptyp_constr (l, _) ->
      fun_of_longident fun_name l.txt |> Loc.make ~loc
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


let join_of_core_type = fun_of_core_type "join"

(* [var.field] *)
let access ~loc var field_lid =
  let var_lid = Loc.make ~loc (Longident.Lident var) in
  Ast_helper.Exp.field ~loc (Ast_helper.Exp.ident ~loc var_lid) field_lid


(* [Field.join lhs.field rhs.field] *)
let create_initializer ~loc (ld : label_declaration) =
  let field_lid = Loc.make ~loc (Longident.Lident ld.pld_name.txt) in
  let lhs_access = access ~loc "lhs" field_lid in
  let rhs_access = access ~loc "rhs" field_lid in
  let func_lid = join_of_core_type ~loc ld.pld_type.ptyp_desc in
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


let conjunction ~loc exprs =
  let f acc exp =
    match acc with None -> Some exp | Some exp_acc -> Some [%expr [%e exp] && [%e exp_acc]]
  in
  List.fold exprs ~init:None ~f |> Option.value_exn


(* conjunction of [phys_equal] over all fields *)
let phys_equal_fields ~loc var lds =
  let phys_equal_exps = List.map lds ~f:(phys_equal_field ~loc var) in
  conjunction ~loc phys_equal_exps


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


let leq_of_core_type = fun_of_core_type "leq"

let leq_expr ~loc ld =
  let field_lid = Loc.make ~loc (Longident.Lident ld.pld_name.txt) in
  let lhs_access = access ~loc "lhs" field_lid in
  let rhs_access = access ~loc "rhs" field_lid in
  let leq_call = Ast_helper.Exp.ident ~loc (leq_of_core_type ~loc ld.pld_type.ptyp_desc) in
  [%expr [%e leq_call] ~lhs:[%e lhs_access] ~rhs:[%e rhs_access]]


let leq_impl ~loc lds =
  let leq_exprs = List.rev_map lds ~f:(leq_expr ~loc) in
  let conj_expr = conjunction ~loc leq_exprs in
  let final_expr = [%expr phys_equal lhs rhs || [%e conj_expr]] in
  let body = [%expr fun ~lhs ~rhs -> [%e final_expr]] in
  let fun_label = Loc.make ~loc "leq" in
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
          [join_impl ~loc fields; leq_impl ~loc fields] )
  |> List.concat


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "absdom" ~str_type_decl
