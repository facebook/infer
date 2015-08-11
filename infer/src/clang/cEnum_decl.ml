(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Translate an enumeration declaration by adding it to the tenv and *)
(** translating the code and adding it to a fake procdesc *)

open CFrontend_utils

let create_empty_procdesc () =
  let procname = Procname.from_string_c_fun "__INFER_$GLOBAL_VAR_env" in
  let open Cfg.Procdesc in
  let proc_attributes =
    {
      Sil.access = Sil.Default;
      Sil.exceptions = [];
      Sil.is_abstract = false;
      Sil.is_bridge_method = false;
      Sil.is_objc_instance_method = false;
      Sil.is_synthetic_method = false;
      Sil.language = Sil.C_CPP;
      Sil.func_attributes = [];
      Sil.method_annotation = Sil.method_annotation_empty;
      Sil.is_generated = false;
    } in
  create {
    cfg = Cfg.Node.create_cfg ();
    name = procname;
    is_defined = false;
    ret_type = Sil.Tvoid;
    formals = [];
    locals = [];
    captured = [];
    loc = Sil.loc_none;
    proc_attributes = proc_attributes;
  }

(* We will use global_procdesc for storing global variables. *)
(* Globals will be stored as locals in global_procdesc and they are added*)
(* when traversing the AST. *)
let global_procdesc = ref (create_empty_procdesc ())

let rec get_enum_constants context decl_list v =
  match decl_list with
  | [] -> []
  | Clang_ast_t.EnumConstantDecl (decl_info, name_info, qual_type, enum_constant_decl_info) :: decl_list' ->
      let name = name_info.Clang_ast_t.ni_name in
      (match enum_constant_decl_info.Clang_ast_t.ecdi_init_expr with
       | None -> Printing.log_out "%s" ("  ...Defining Enum Constant ("^name^", "^(string_of_int v));
           (Mangled.from_string name, Sil.Cint (Sil.Int.of_int v))
           :: get_enum_constants context decl_list' (v + 1)
       | Some stmt ->
           let e = CGen_trans.CTransImpl.expression_trans context stmt
               "WARNING: Expression in Enumeration constant not found\n" in
           let const = (match Prop.exp_normalize_prop Prop.prop_emp e with
               | Sil.Const c -> c
               | _ -> (* This is a hack to avoid failing in some strange definition of Enum *)
                   Sil.Cint Sil.Int.zero) in
           Printing.log_out "  ...Defining Enum Constant ('%s', " name;
           Printing.log_out "'%s')\n" (Sil.exp_to_string (Sil.Const const));
           (Mangled.from_string name, const) :: get_enum_constants context decl_list' v)
  | _ -> assert false

let enum_decl name tenv cfg cg namespace decl_list opt_type =
  Printing.log_out "ADDING: EnumDecl '%s'\n" name;
  let context' =
    CContext.create_context tenv cg cfg !global_procdesc namespace CContext.ContextNoCls
      false [] false in
  let enum_constants = get_enum_constants context' decl_list 0 in
  let name = (match opt_type with (* If the type is defined it's of the kind "enum name" and we take that.*)
      | `Type s -> s
      | `NoType -> assert false) in
  (* Here we could give "enum "^name but I want to check that this the type is always defined *)
  let typename = Sil.TN_enum (Mangled.from_string name) in
  let typ = Sil.Tenum enum_constants in
  Printing.log_out "  TN_typename('%s')\n" (Sil.typename_to_string typename);
  Sil.tenv_add tenv typename typ
