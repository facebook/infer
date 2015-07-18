(*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*)

(** Module for utility functions for the whole frontend. Includes functions for printing,  *)
(** for transformations of ast nodes and general utility functions such as  functions on lists *)

open Utils
open Clang_ast_t

module L = Logging
module F = Format

module Printing =
struct
  let log_out fmt =
    let pp = if !CFrontend_config.debug_mode then Format.fprintf else Format.ifprintf in
    pp Format.std_formatter fmt

  let log_err fmt =
    let pp = if !CFrontend_config.debug_mode then Format.fprintf else Format.ifprintf in
    pp Format.err_formatter fmt

  let log_stats fmt =
    let pp =
      if !CFrontend_config.stats_mode || !CFrontend_config.debug_mode
      then Format.fprintf else Format.ifprintf in
    pp Format.std_formatter fmt

  let print_tenv tenv =
    Sil.tenv_iter (fun typname typ ->
            match typname with
            | Sil.TN_csu (Sil.Class, _) | Sil.TN_csu (Sil.Protocol, _) ->
                (match typ with (Sil.Tstruct (fields, static_fields, _, cls, super_classes, methods, iann)) ->
                      (print_endline (
                            (Sil.typename_to_string typname)^"\n"^
                            "---> superclass and protocols "^(list_to_string (fun (csu, x) ->
                                      let nsu = Sil.TN_csu (csu, x) in
                                      "\t"^(Sil.typename_to_string nsu)^"\n") super_classes)^
                            "---> methods "^(list_to_string (fun x ->"\t"^(Procname.to_string x)^"\n") methods)^"  "^
                            "\t---> static fields "^(list_to_string (fun (fieldname, typ, _) ->
                                      "\t "^(Ident.fieldname_to_string fieldname)^" "^
                                      (Sil.typ_to_string typ)^"\n") static_fields)^
                            "\t---> fields "^(list_to_string (fun (fieldname, typ, _) ->
                                      "\t "^(Ident.fieldname_to_string fieldname)^" "^
                                      (Sil.typ_to_string typ)^"\n") fields
                            )
                          )
                      )
                  | _ -> ())
            | _ -> ()
      ) tenv

  let print_tenv_struct_unions tenv =
    Sil.tenv_iter (fun typname typ ->
            match typname with
            | Sil.TN_csu (Sil.Struct, _) | Sil.TN_csu (Sil.Union, _) ->
                (match typ with
                  | (Sil.Tstruct (fields, static_fields, _, cls, super_classes, methods, iann)) ->
                      (print_endline (
                            (Sil.typename_to_string typname)^"\n"^
                            "\t---> fields "^(list_to_string (fun (fieldname, typ, _) ->
                                      match typ with
                                      | Sil.Tvar tname -> "tvar"^(Sil.typename_to_string tname)
                                      | Sil.Tstruct (_, _, _, _, _, _, _) | _ ->
                                          "\t struct "^(Ident.fieldname_to_string fieldname)^" "^
                                          (Sil.typ_to_string typ)^"\n") fields
                            )
                          )
                      )
                  | _ -> ())
            | Sil.TN_typedef typname ->
                print_endline ((Mangled.to_string typname)^"-->"^(Sil.typ_to_string typ))
            | _ -> ()
      ) tenv

  let print_procedures cfg =
    let procs = Cfg.get_all_procs cfg in
    print_endline
      (list_to_string (fun pdesc ->
                let pname = Cfg.Procdesc.get_proc_name pdesc in
                "name> "^
                (Procname.to_string pname) ^
                " defined? " ^ (string_of_bool (Cfg.Procdesc.is_defined pdesc)) ^ "\n")
          procs)

  let print_failure_info pointer =
    L.err "AST Element> %s IN FILE> %s @.@." pointer !CFrontend_config.json

  let print_nodes nodes =
    list_iter (fun node -> print_endline (Cfg.Node.get_description Utils.pe_text node)) nodes

  let instrs_to_string instrs =
    let pp fmt () = Format.fprintf fmt "%a" (Sil.pp_instr_list Utils.pe_text) instrs in
    pp_to_string pp ()

end

module Ast_utils =
struct

  let string_of_decl decl =
    let name = Clang_ast_proj.get_decl_kind_string decl in
    let info = Clang_ast_proj.get_decl_tuple decl in
    "<\"" ^ name ^ "\"> '" ^ info.Clang_ast_t.di_pointer ^ "'"

  let string_of_unary_operator_kind = function
    | `PostInc -> "PostInc"
    | `PostDec -> "PostDec"
    | `PreInc -> "PreInc"
    | `PreDec -> "PreDec"
    | `AddrOf -> "AddrOf"
    | `Deref -> "Deref"
    | `Plus -> "Plus"
    | `Minus -> "Minus"
    | `Not -> "Not"
    | `LNot -> "LNot"
    | `Real -> "Real"
    | `Imag -> "Imag"
    | `Extension -> "Extension"

  let string_of_stmt stmt =
    let name = Clang_ast_proj.get_stmt_kind_string stmt in
    let info, _ = Clang_ast_proj.get_stmt_tuple stmt in
    "<\"" ^ name ^ "\"> '" ^ info.Clang_ast_t.si_pointer ^ "'"

  let get_stmts_from_stmt stmt =
    match stmt with
    | OpaqueValueExpr(_, lstmt, _, opaque_value_expr_info) ->
        (match opaque_value_expr_info.Clang_ast_t.ovei_source_expr with
          | Some stmt -> lstmt@[stmt]
          | _ -> lstmt)
    (* given that this has not been translated, looking up for variables *)
    (* inside leads to inconsistencies *)
    | ObjCAtCatchStmt (stmt_info, stmt_list, obj_c_message_expr_kind) ->
        []
    | _ -> snd (Clang_ast_proj.get_stmt_tuple stmt)

  let namespace_to_string namespace =
    match namespace with
    | None -> ""
    | Some ns when ns ="" -> ""
    | Some ns -> ns^"::"

  let property_name property_impl_decl_info =
    let no_property_name = "WARNING_NO_PROPERTY_NAME" in
    match property_impl_decl_info.Clang_ast_t.opidi_property_decl with
    | Some decl_ref ->
        (match decl_ref.Clang_ast_t.dr_name with
          | Some n -> n.Clang_ast_t.ni_name
          | _ -> no_property_name)
    | None -> no_property_name

  let property_attribute_compare att1 att2 =
    match att1, att2 with
      `Readonly, `Readonly -> 0
    | `Readonly, _ -> -1
    | _, `Readonly -> 1
    | `Assign, `Assign -> 0
    | `Assign, _ -> -1
    | _, `Assign -> 1
    | `Readwrite, `Readwrite -> 0
    | `Readwrite, _ -> -1
    | _, `Readwrite -> 1
    | `Retain, `Retain -> 0
    | `Retain, _ -> -1
    | _, `Retain -> 1
    | `Copy, `Copy -> 0
    | `Copy, _ -> -1
    | _, `Copy -> 1
    | `Nonatomic, `Nonatomic -> 0
    | `Nonatomic, _ -> -1
    | _, `Nonatomic -> 1
    | `Atomic, `Atomic -> 0
    | `Atomic, _ -> 1
    | _, `Atomic -> 1
    | `Weak, `Weak -> 0
    | `Weak, _ -> -1
    | _, `Weak -> 1
    | `Strong, `Strong -> 0
    | `Strong, _ -> -1
    | _, `Strong -> 1
    | `Unsafe_unretained, `Unsafe_unretained -> 0
    | `Unsafe_unretained, _ -> -1
    | _, `Unsafe_unretained -> 1
    | `Getter _, `Getter _ -> 0
    | `Getter _, _ -> -1
    | _, `Getter _ -> 1
    | `Setter _, `Setter _ -> 0

  let property_attribute_eq att1 att2 =
    property_attribute_compare att1 att2 = 0

  let get_memory_management_attributes () =
    [`Assign; `Retain; `Copy; `Weak; `Strong; `Unsafe_unretained]

  let is_retain attribute_opt =
    match attribute_opt with
    | Some attribute ->
        attribute = `Retain || attribute = `Strong
    | _ -> false

  let is_copy attribute_opt =
    match attribute_opt with
    | Some attribute ->
        attribute = `Copy
    | _ -> false


 let name_opt_of_name_info_opt name_info_opt =
      match name_info_opt with
      | Some name_info -> Some name_info.Clang_ast_t.ni_name
      | None -> None

  let rec getter_attribute_opt attributes =
    match attributes with
    | [] -> None
    | attr:: rest ->
        match attr with
        | `Getter getter -> name_opt_of_name_info_opt getter.Clang_ast_t.dr_name
        | _ -> (getter_attribute_opt rest)

  let rec setter_attribute_opt attributes =
    match attributes with
    | [] -> None
    | attr:: rest ->
        match attr with
        | `Setter setter -> name_opt_of_name_info_opt setter.Clang_ast_t.dr_name
        | _ -> (setter_attribute_opt rest)

  (*TODO: take the attributes into account too. To be done after we get the attribute's arguments. *)
  let is_type_nonnull qt attributes =
    Utils.string_is_prefix CFrontend_config.nonnull_attribute qt.Clang_ast_t.qt_raw

  let pointer_counter = ref 0

  let get_fresh_pointer () =
    pointer_counter := !pointer_counter + 1;
    CFrontend_config.pointer_prefix^(string_of_int (!pointer_counter))

  let get_invalid_pointer () =
    CFrontend_config.pointer_prefix^("INVALID")

  let type_from_unary_expr_or_type_trait_expr_info info =
    match info.uttei_qual_type with
    | Some qt -> Some qt
    | None -> None

end

(* Global counter for anonymous block*)
let block_counter = ref 0

(* Returns a fresh index for a new anonymous block *)
let get_fresh_block_index () =
  block_counter := !block_counter +1;
  !block_counter

module General_utils =
struct

  let rec swap_elements_list l =
    match l with
    | el1:: el2:: rest ->
        el2:: el1:: (swap_elements_list rest)
    | [] -> []
    | _ -> assert false

  let rec string_from_list l =
    match l with
    | [] -> ""
    | [item] -> item
    | item:: l' -> item^" "^(string_from_list l')

  let get_fun_body fdecl_info = fdecl_info.Clang_ast_t.fdi_body

  let rec append_no_duplicates eq list1 list2 =
    match list2 with
    | el:: rest2 ->
        if (list_mem eq el list1) then
          (append_no_duplicates eq list1 rest2)
        else (append_no_duplicates eq list1 rest2)@[el]
    | [] -> list1

  let append_no_duplicates_csu list1 list2 =
    append_no_duplicates Sil.csu_name_equal list1 list2

  let append_no_duplicates_methods list1 list2 =
    append_no_duplicates Procname.equal list1 list2

  let append_no_duplicated_vars list1 list2 =
    let eq (m1, t1) (m2, t2) = (Mangled.equal m1 m2) && (Sil.typ_equal t1 t2) in
    append_no_duplicates eq list1 list2

  let append_no_duplicated_pvars list1 list2 =
    let eq (e1, t1) (e2, t2) = (Sil.exp_equal e1 e2) && (Sil.typ_equal t1 t2) in
    append_no_duplicates eq list1 list2

  let append_no_duplicates_fields list1 list2 =
    let field_eq (n1, t1, a1) (n2, t2, a2) =
      match Ident.fieldname_equal n1 n2, Sil.typ_equal t1 t2, Sil.item_annotation_compare a1 a2 with
      | true, true, _ -> true
      | _, _, _ -> false in
    append_no_duplicates field_eq list1 list2

  let sort_fields fields =
    let compare (name1, _, _) (name2, _, _) =
      Ident.fieldname_compare name1 name2 in
    list_sort compare fields

  let rec collect_list_tuples l (a, a1, b, c, d) =
    match l with
    | [] -> (a, a1, b, c, d)
    | (a', a1', b', c', d'):: l' -> collect_list_tuples l' (a@a', a1@a1', b@b', c@c', d@d')

  let is_static_var var_decl_info =
    match var_decl_info.Clang_ast_t.vdi_storage_class with
    | Some sc -> sc = CFrontend_config.static
    | _ -> false

  let block_procname_with_index defining_proc i =
    Config.anonymous_block_prefix^(Procname.to_string defining_proc)^Config.anonymous_block_num_sep^(string_of_int i)

  (* Makes a fresh name for a block defined inside the defining procedure.*)
  (* It updates the global block_counter *)
  let mk_fresh_block_procname defining_proc =
    let name = block_procname_with_index defining_proc (get_fresh_block_index ()) in
    Procname.mangled_objc_block name

  (* Returns the next fresh name for a block defined inside the defining procedure *)
  (* It does not update the global block_counter *)
  let get_next_block_pvar defining_proc =
    let name = block_procname_with_index defining_proc (!block_counter +1) in
    Sil.mk_pvar (Mangled.from_string (CFrontend_config.temp_var^"_"^name)) defining_proc

  (* Reset  block counter *)
  let reset_block_counter () =
    block_counter := 0

  let mk_function_decl_info_from_block block_decl_info =
    {
      Clang_ast_t.fdi_storage_class = None;
      Clang_ast_t.fdi_is_inline = true; (* This value should not matter as we don't use it*)
      Clang_ast_t.fdi_is_virtual = false; (* This value should not matter as we don't use it*)
      Clang_ast_t.fdi_is_module_private = true; (* This value should not matter as we don't use it*)
      Clang_ast_t.fdi_is_pure = false; (* This value should not matter as we don't use it*)
      Clang_ast_t.fdi_is_delete_as_written = false; (* This value should not matter as we don't use it*)
      Clang_ast_t.fdi_decls_in_prototype_scope =[];
      Clang_ast_t.fdi_parameters = block_decl_info.Clang_ast_t.bdi_parameters;
      Clang_ast_t.fdi_cxx_ctor_initializers = [];
      Clang_ast_t.fdi_body = block_decl_info.Clang_ast_t.bdi_body;
    }

  let rec zip xs ys =
    match xs, ys with
    | [], _
    | _, [] -> []
    | x :: xs, y :: ys -> (x, y) :: zip xs ys

  let list_range i j =
    let rec aux n acc =
      if n < i then acc else aux (n -1) (n :: acc)
    in aux j [] ;;

  let replicate n el = list_map (fun i -> el) (list_range 0 (n -1))

end




