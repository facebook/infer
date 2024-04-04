(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let make_longident ~loc s = Loc.make ~loc (Longident.Lident s)

let make_function ~loc name body =
  let fun_label = Loc.make ~loc name in
  let fn = Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc fun_label) body in
  Ast_helper.Str.value ~loc Nonrecursive [fn]


let make_ident_exp ~loc s = Ast_helper.Exp.ident ~loc (make_longident ~loc s)

let conjunction ~loc = function
  | [] ->
      [%expr true]
  | first :: rest ->
      List.fold rest ~init:first ~f:(fun exp_acc exp -> [%expr [%e exp] && [%e exp_acc]])


(* record expression [{ a=exp_a; b=exp_b; c=exp_c; ...}] *)
let create_record ~loc ?with_value fields field_exps =
  let field_lids = List.map fields ~f:(fun ld -> make_longident ~loc ld.pld_name.txt) in
  let initializers = List.zip_exn field_lids field_exps in
  Ast_helper.Exp.record ~loc initializers with_value


(* [var.field] *)
let access ~loc var ld =
  let var_exp = make_ident_exp ~loc var in
  let field_lid = make_longident ~loc ld.pld_name.txt in
  Ast_helper.Exp.field ~loc var_exp field_lid


(* [phys_equal field var.field] *)
let phys_equal_field ~loc var ld =
  let field_exp = make_ident_exp ~loc ld.pld_name.txt in
  let access = access ~loc var ld in
  [%expr phys_equal [%e field_exp] [%e access]]


(* conjunction of [phys_equal] over all fields *)
let phys_equal_fields ~loc var lds = List.map lds ~f:(phys_equal_field ~loc var) |> conjunction ~loc

(* [if (phys_equal a lhs.a && phys_equal b lhs.b && ...) then lhs else else_exp] *)
let if_phys_equal_then_var ~loc var lds else_exp =
  let guard = phys_equal_fields ~loc var lds in
  let var_lid = make_longident ~loc var |> Ast_helper.Exp.ident ~loc in
  let then_exp = [%expr [%e var_lid]] in
  Ast_helper.Exp.ifthenelse ~loc guard then_exp (Some else_exp)


(* [let field = (initializer field) (rhs_initializer ld) in acc] *)
let let_field_equal_rhs_expr ~loc rhs_initializer acc ld =
  let rhs = rhs_initializer ~loc ld in
  let field_pat = Ast_helper.Pat.var ~loc ld.pld_name in
  let vb = Ast_helper.Vb.mk ~loc field_pat rhs in
  Ast_helper.Exp.let_ ~loc Nonrecursive [vb] acc


let rec pp_longident fmt = function
  | Ldot (l, s) ->
      Format.fprintf fmt "Ldot(%a, %s)" pp_longident l s
  | Lident s ->
      Format.fprintf fmt "Lident(%s)" s
  | Lapply (l, r) ->
      Format.fprintf fmt "Lapply(%a, %a)" pp_longident l pp_longident r


(* generate a passthrough of the form [let f = f] for use within modules *)
let generate_passthrough_function ~loc fun_of_type fun_name manifest_type =
  let normalize_origin = fun_of_type ~loc manifest_type in
  let body = Ast_helper.Exp.ident ~loc normalize_origin in
  make_function ~loc fun_name body
