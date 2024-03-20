(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Ppxlib

let name_of_type_name ~root type_name = match type_name with "t" -> root | s -> root ^ "_" ^ s

let mk_initial_stats_record_field_exps fields =
  List.map fields ~f:(fun {pld_name= _; pld_type; pld_loc= loc} ->
      match pld_type.ptyp_desc with
      | Ptyp_constr ({txt= Ldot (module_names, type_name)}, _) ->
          Ast_helper.Exp.mk ~loc
            (Pexp_ident {txt= Ldot (module_names, name_of_type_name ~root:"init" type_name); loc})
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
          let ext =
            Location.error_extensionf ~loc
              "ERROR: Unsupported type, please define a module for your type. Only types of the \
               form [M.blah], [M.N.blah], etc., or variations with type arguments like [some_type \
               M.t] are supported"
          in
          Ast_builder.Default.pexp_extension ~loc ext )


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
      [%str
        let global_stats = [%e global_stats_init_record]

        let initial = [%e global_stats_init_record]

        let copy from ~into =
          let [%p all_fields_pattern] = from in
          [%e set_all_mutable_fields_expression]]
  | _ ->
      let ext =
        Location.error_extensionf ~loc
          "Cannot derive stats functions for non record types or for multiple records in a file"
      in
      [Ast_builder.Default.pstr_extension ~loc ext []]


let () =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  Deriving.add "infer_stats" ~str_type_decl |> ignore
