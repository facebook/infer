(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ppxlib

let name_of_type_name ~root type_name = match type_name with "t" -> root | s -> root ^ "_" ^ s

let destruct_stats_field_type ~loc pld_type =
  match pld_type.ptyp_desc with
  | Ptyp_constr ({txt= Ldot (module_names, type_name)}, _) ->
      Ok (module_names, type_name)
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_arrow _
  | Ptyp_tuple _
  | Ptyp_constr _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_alias _
  | Ptyp_variant _
  | Ptyp_poly _
  | Ptyp_package _
  | Ptyp_extension _ ->
      Error
        (Location.error_extensionf ~loc
           "ERROR: Unsupported type, please define a module for your type. Only types of the form \
            [M.blah], [M.N.blah], etc., or variations with type arguments like [some_type M.t] are \
            supported" )


let fun_exp_of_field ~root ~loc pld_type =
  match destruct_stats_field_type ~loc pld_type with
  | Ok (module_names, type_name) ->
      Ast_helper.Exp.mk ~loc
        (Pexp_ident {txt= Ldot (module_names, name_of_type_name ~root type_name); loc})
  | Error ext ->
      Ast_builder.Default.pexp_extension ~loc ext


let mk_initial_stats_record_field_exps fields =
  List.map fields ~f:(fun {pld_name= _; pld_type; pld_loc= loc} ->
      fun_exp_of_field ~root:"init" ~loc pld_type )


(** generate the pattern [{f1=f1; ...; fn=fn}] to bind [f1], ..., [fn] *)
let mk_all_fields_pattern ~loc fields =
  let pattern_fields =
    List.map fields ~f:(fun {pld_name; pld_loc= loc} ->
        let label = {txt= Lident pld_name.txt; loc} in
        let pattern = Ast_helper.Pat.mk ~loc (Ppat_var pld_name) in
        (label, pattern) )
  in
  Ast_helper.Pat.record ~loc pattern_fields Closed


(** generate the function call [Fields.Direct.set_all_mutable_fields into ~f1 ~f2 ... ~fn] *)
let mk_set_all_mutable_fields_call ~loc fields =
  let labelled_arguments =
    List.map fields ~f:(fun {pld_name; pld_loc= loc} ->
        let arg_expr = Common.make_ident_exp ~loc pld_name.txt in
        (Labelled pld_name.txt, arg_expr) )
  in
  Ast_helper.Exp.apply ~loc [%expr Fields.Direct.set_all_mutable_fields]
    ((Nolabel, [%expr into]) :: labelled_arguments)


(** generate [let merge stats1 stats2 = {f1= M1.merge_stats stats1.f1 stats2.f1; ...}] *)
let mk_merge_fun_decl ~loc fields =
  let res =
    List.fold_result fields ~init:[] ~f:(fun res {pld_name; pld_type; pld_loc= loc} ->
        let open IResult.Let_syntax in
        let label = {txt= Lident pld_name.txt; loc} in
        let+ module_names, type_name = destruct_stats_field_type ~loc pld_type in
        let merge_fun_exp =
          Ast_helper.Exp.mk ~loc
            (Pexp_ident {txt= Ldot (module_names, name_of_type_name ~root:"merge" type_name); loc})
        in
        let stats1_field_exp = Ast_helper.Exp.field ~loc [%expr stats1] label in
        let stats2_field_exp = Ast_helper.Exp.field ~loc [%expr stats2] label in
        let merge_apply_exp =
          [%expr [%e merge_fun_exp] [%e stats1_field_exp] [%e stats2_field_exp]]
        in
        (label, merge_apply_exp) :: res )
  in
  match res with
  | Error ext ->
      Ast_builder.Default.pstr_extension ~loc ext []
  | Ok field_assignments ->
      let record_assignment = Ast_helper.Exp.record field_assignments None in
      [%stri let merge stats1 stats2 = [%e record_assignment]]


(** generate the function call [Fields.to_list ~f1:(of_field FieldTypeModule1.to_log_entries) ...] *)
let mk_fields_to_list_log_call ~loc fields =
  let labelled_arguments =
    List.map fields ~f:(fun {pld_name; pld_type; pld_loc= loc} ->
        let pp_function = fun_exp_of_field ~loc ~root:"to_log_entries" pld_type in
        (Labelled pld_name.txt, [%expr of_field [%e pp_function]]) )
  in
  Ast_helper.Exp.apply ~loc [%expr Fields.to_list] labelled_arguments


let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match type_declarations with
  | [] ->
      []
  | [{ptype_kind= Ptype_record fields; _}] ->
      let field_exps = mk_initial_stats_record_field_exps fields in
      let global_stats_init_record = Common.create_record ~loc fields field_exps in
      let all_fields_pattern = mk_all_fields_pattern ~loc fields in
      let set_all_mutable_fields_expression = mk_set_all_mutable_fields_call ~loc fields in
      let merge_fun_decl = mk_merge_fun_decl ~loc fields in
      let fields_to_list_log_call = mk_fields_to_list_log_call ~loc fields in
      [%str
        let global_stats = [%e global_stats_init_record]

        let initial = [%e global_stats_init_record]

        let copy from ~into =
          let [%p all_fields_pattern] = from in
          [%e set_all_mutable_fields_expression]


        [%%i merge_fun_decl]

        let to_log_entries stats =
          let of_field value_to_log_entries field =
            value_to_log_entries ~field_name:(Field.name field) (Field.get field stats)
          in
          [%e fields_to_list_log_call] |> List.concat]
  | _ ->
      let ext =
        Location.error_extensionf ~loc
          "Cannot derive stats functions for non record types or for multiple records in a file"
      in
      [Ast_builder.Default.pstr_extension ~loc ext []]


let () =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "infer_stats" ~str_type_decl |> ignore
