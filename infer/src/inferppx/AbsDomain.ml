(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let func_str_from_typename ~root typename = match typename with "t" -> root | s -> root ^ "_" ^ s

let fun_of_longident root lid =
  match lid with
  | Lident typename ->
      Lident (func_str_from_typename ~root typename)
  | Ldot (l, typename) ->
      Ldot (l, func_str_from_typename ~root typename)
  | _ ->
      Format.eprintf "Could not parse ident: %a@\n" Common.pp_longident lid ;
      assert false


let fun_of_core_type ~loc fun_name ct =
  match ct.ptyp_desc with
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


let func_name_from_type ~root lt = func_str_from_typename ~root lt.txt

let join_of_core_type ~loc core_type = fun_of_core_type "join" ~loc core_type

(* [Field.join lhs.field rhs.field] *)
let create_join_initializer ~loc (ld : label_declaration) =
  let lhs_access = Common.access ~loc "lhs" ld in
  let rhs_access = Common.access ~loc "rhs" ld in
  let func_lid = join_of_core_type ~loc ld.pld_type in
  let func_name = Ast_helper.Exp.ident ~loc func_lid in
  [%expr [%e func_name] [%e lhs_access] [%e rhs_access]]


(* record expression [{ a=a; b=b; c=c; ...}] *)
let create_record ~loc fields =
  let field_exps = List.map fields ~f:(fun fld -> Common.make_ident_exp ~loc fld.pld_name.txt) in
  Common.create_record ~loc fields field_exps


let join_impl ~loc typ_name (lds : label_declaration list) =
  let func_name = func_name_from_type ~root:"join" typ_name in
  let record_exp = create_record ~loc lds in
  let guarded =
    Common.if_phys_equal_then_var ~loc "rhs" lds record_exp
    |> Common.if_phys_equal_then_var ~loc "lhs" lds
  in
  let final_expr =
    List.fold lds ~init:guarded ~f:(Common.let_field_equal_rhs_expr ~loc create_join_initializer)
  in
  let body = [%expr fun lhs rhs -> if phys_equal lhs rhs then lhs else [%e final_expr]] in
  Common.make_function ~loc func_name body


let leq_of_core_type = fun_of_core_type "leq"

let leq_expr ~loc ld =
  let lhs_access = Common.access ~loc "lhs" ld in
  let rhs_access = Common.access ~loc "rhs" ld in
  let leq_call = Ast_helper.Exp.ident ~loc (leq_of_core_type ~loc ld.pld_type) in
  [%expr [%e leq_call] ~lhs:[%e lhs_access] ~rhs:[%e rhs_access]]


let leq_impl ~loc typ_name lds =
  let func_name = func_name_from_type ~root:"leq" typ_name in
  let leq_exprs = List.rev_map lds ~f:(leq_expr ~loc) in
  let conj_expr = Common.conjunction ~loc leq_exprs in
  let final_expr = [%expr phys_equal lhs rhs || [%e conj_expr]] in
  let body = [%expr fun ~lhs ~rhs -> [%e final_expr]] in
  Common.make_function ~loc func_name body


let widen_of_core_type = fun_of_core_type "widen"

(* [Field.widen ~prev:prev.field ~next:next.field ~num_iters] *)
let create_widen_initializer ~loc (ld : label_declaration) =
  let lhs_access = Common.access ~loc "prev" ld in
  let rhs_access = Common.access ~loc "next" ld in
  let func_lid = fun_of_core_type "widen" ~loc ld.pld_type in
  let func_name = Ast_helper.Exp.ident ~loc func_lid in
  [%expr [%e func_name] ~prev:[%e lhs_access] ~next:[%e rhs_access] ~num_iters]


let widen_impl ~loc typ_name (lds : label_declaration list) =
  let func_name = func_name_from_type ~root:"widen" typ_name in
  let record_exp = create_record ~loc lds in
  let guarded =
    Common.if_phys_equal_then_var ~loc "prev" lds record_exp
    |> Common.if_phys_equal_then_var ~loc "next" lds
  in
  let final_expr =
    List.fold lds ~init:guarded ~f:(Common.let_field_equal_rhs_expr ~loc create_widen_initializer)
  in
  let body =
    [%expr fun ~prev ~next ~num_iters -> if phys_equal prev next then prev else [%e final_expr]]
  in
  Common.make_function ~loc func_name body


let generate_passthroughs_impl ~loc ptype_name manifest_type =
  let join_name = func_name_from_type ~root:"join" ptype_name in
  let leq_name = func_name_from_type ~root:"leq" ptype_name in
  let widen_name = func_name_from_type ~root:"widen" ptype_name in
  [ Common.generate_passthrough_function ~loc join_of_core_type join_name manifest_type
  ; Common.generate_passthrough_function ~loc leq_of_core_type leq_name manifest_type
  ; Common.generate_passthrough_function ~loc widen_of_core_type widen_name manifest_type ]


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {ptype_kind= Ptype_record fields; ptype_name; _} ->
          [ join_impl ~loc ptype_name fields
          ; leq_impl ~loc ptype_name fields
          ; widen_impl ~loc ptype_name fields ]
      | {ptype_kind= Ptype_abstract; ptype_manifest= Some manifest_type; ptype_name} ->
          (* passthrough case like [let nonrec t = t] *)
          generate_passthroughs_impl ~loc ptype_name manifest_type
      | {ptype_kind= Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _} ->
          let ext =
            Location.error_extensionf ~loc:ptype_loc "Cannot derive functions for non record types"
          in
          [Ast_builder.Default.pstr_extension ~loc ext []] )
  |> List.concat


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "abstract_domain" ~str_type_decl
