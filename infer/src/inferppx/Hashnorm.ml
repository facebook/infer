(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
open Ppxlib

let normalize_of_longident ?(suffix = "") lid =
  match lid with
  | Lident "t" ->
      (* `t` is not enclosed in a module *)
      Lident ("normalize" ^ suffix)
  | Ldot (l, "t") ->
      (* `M.t` *)
      Ldot (Ldot (l, "Normalizer"), "normalize" ^ suffix)
  | _ ->
      Format.eprintf "Could not parse ident: %a@\n" Common.pp_longident lid ;
      assert false


(* ident `A.B.C.normalize`/`A.B.C.normalize_opt` from the type `A.B.C.t`/`A.B.C.t option` *)
let normalize_of_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_constr (l, []) ->
      (* monomorphic type *)
      normalize_of_longident l.txt |> Loc.make ~loc
  | Ptyp_constr ({txt= Lident "option"}, [{ptyp_desc= Ptyp_constr (l, [])}]) ->
      (* option type *)
      normalize_of_longident ~suffix:"_opt" l.txt |> Loc.make ~loc
  | Ptyp_constr _
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


(* [Field.normalize t.field] *)
let create_normalize_initializer ~loc (ld : label_declaration) =
  let field_lid = Common.make_longident ~loc ld.pld_name.txt in
  let lhs_access = Common.access ~loc "t" field_lid in
  let func_lid = normalize_of_core_type ~loc ld.pld_type in
  let func_name = Ast_helper.Exp.ident ~loc func_lid in
  [%expr [%e func_name] [%e lhs_access]]


let should_normalize_field (ld : label_declaration) =
  let should_normalize_basic_type _ = false in
  match ld.pld_type.ptyp_desc with
  | Ptyp_constr ({txt= Lident "option"}, [_]) ->
      true
  | Ptyp_constr ({txt= Lident type_ident}, _) ->
      should_normalize_basic_type type_ident
  | Ptyp_constr ({txt= Ldot (_, "t")}, _) ->
      true
  | _ ->
      false


let normalize_impl ~loc (lds : label_declaration list) =
  let lds = List.filter lds ~f:should_normalize_field in
  let record_exp = Common.create_record ~loc ~with_value:[%expr t] lds in
  let guarded = Common.if_phys_equal_then_var ~loc "t" lds record_exp in
  let final_expr =
    List.fold lds ~init:guarded
      ~f:(Common.let_field_equal_rhs_expr ~loc create_normalize_initializer)
  in
  let body = [%expr fun t -> [%e final_expr]] in
  Common.make_function ~loc "normalize" body


let normalize_passthrough_impl ~loc manifest_type =
  Common.generate_passthrough_function ~loc normalize_of_core_type "normalize" manifest_type


let normalize_type_declaration ~loc (td : type_declaration) =
  match td with
  | {ptype_kind= Ptype_record fields; _} ->
      [normalize_impl ~loc fields]
  | {ptype_kind= Ptype_abstract; ptype_manifest= Some manifest_type} ->
      (* passthrough case like `let nonrec t = t` *)
      [normalize_passthrough_impl ~loc manifest_type]
  | {ptype_kind= Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _} ->
      let ext =
        Location.error_extensionf ~loc:ptype_loc "Cannot derive functions for non record types"
      in
      [Ast_builder.Default.pstr_extension ~loc ext []]


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(normalize_type_declaration ~loc) |> List.concat


let _ =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "normalize" ~str_type_decl
