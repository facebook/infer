(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Process variable declarations by saving them as local or global variables.  *)
(** Computes the local variables of a function or method to be added to the procdesc *)

open Utils
open CFrontend_utils
open Clang_ast_t

module L = Logging

(* For a variable declaration it return/construct the type *)
let get_var_type tenv name t =
  let typ = CTypes_decl.qual_type_to_sil_type tenv t in
  Printing.log_out "     Getting/Defining type for variable '%s'" name;
  Printing.log_out " as  sil type '%s'\n" (Sil.typ_to_string typ);
  typ

(* NOTE: Currently we use this function to avoid certain C++ global variable definition defined *)
(* in traits and config files.*)
(* We recogneze them because these declaration have di_parent_pointer and di_previous_decl defined*)
let global_to_be_added di =
  (di.Clang_ast_t.di_parent_pointer = None) && (di.Clang_ast_t.di_previous_decl =`None)

let global_var_decl tenv namespace decl_info name t =
  Printing.log_out "PASSING: VarDecl for '%s' to global procdesc" name;
  Printing.log_out "  pointer= '%s'\n" decl_info.Clang_ast_t.di_pointer;
  if global_to_be_added decl_info then (
    let typ = get_var_type tenv name t in
    Printing.log_out "     >>> Adding entry to global procdesc: ('%s', " name;
    Printing.log_out "'%s')\n" (Sil.typ_to_string typ);
    CGlobal_vars.add name typ)
  else Printing.log_out "SKIPPING VarDecl for '%s'\n" name

let rec lookup_ahead_for_vardecl context pointer var_name kind decl_list =
  match decl_list with
  | [] -> Printing.log_out "     Failing when looking ahead for variable '%s'\n" var_name;
      assert false (* nothing has been found ahead, maybe something bad in the AST *)
  | VarDecl(decl_info, var_info, t, _) :: rest when var_name = var_info.Clang_ast_t.ni_name ->
      let var_name' = var_info.Clang_ast_t.ni_name in
      if global_to_be_added decl_info then (
        let tenv = CContext.get_tenv context in
        Printing.log_out "ADDING (later-defined): VarDecl '%s' to global procdesc\n" var_name';
        let typ = get_var_type tenv var_name' t in
        Printing.log_out "     >>> Adding (later-defined) entry to global procdesc: ('%s', " var_name';
        Printing.log_out "'%s')\n" (Sil.typ_to_string typ);
        CGlobal_vars.add var_name' typ;
        let mangled_var_name = Mangled.from_string var_name' in
        let global_var = CGlobal_vars.find mangled_var_name in
        CGlobal_vars.var_get_name global_var)
      else (Printing.log_out "SKIPPING VarDecl for '%s'\n" var_name;
            lookup_ahead_for_vardecl context pointer var_name kind rest)
  | _ :: rest ->
      lookup_ahead_for_vardecl context pointer var_name kind rest

let lookup_var_static_globals context name =
  let remove_separator s =
    match Str.split (Str.regexp_string Config.anonymous_block_num_sep) s with
    | s'':: _ -> s''
    | _ -> assert false in
  let remove_block_prefix s =
    match Str.split (Str.regexp_string Config.anonymous_block_prefix) s with
    | [_; s''] -> s''
    | [s''] -> s''
    | _ -> assert false in
  let remove_block_name pname =
    let s = Procname.to_string pname in
    let s'= remove_block_prefix s in
    let s'' = if s'= s then s'
      else remove_separator s' in
    s'' in
  let pname = Cfg.Procdesc.get_proc_name context.CContext.procdesc in
  let str_pname = remove_block_name pname in
  let static_name = Sil.mk_static_local_name str_pname name in
  Printing.log_out "   ...Looking for variable '%s' in static globals...\n" static_name;
  let var_name = Mangled.from_string static_name in
  let global_var = CGlobal_vars.find var_name in
  let var = CGlobal_vars.var_get_name global_var in
  Printing.log_out "   ...Variable '%s' found in static globals!!\n" (Sil.pvar_to_string var);
  var

let lookup_var stmt_info context pointer var_name kind =
  let pvar = CContext.LocalVars.lookup_var context stmt_info.Clang_ast_t.si_pointer var_name kind in
  match pvar with
  | Some var -> var
  | None ->
      try
        lookup_var_static_globals context var_name
      with Not_found ->
        (Printing.log_out "Looking on later-defined decls for '%s' with pointer '%s' \n" var_name stmt_info.Clang_ast_t.si_pointer;
         let decl_list = !CFrontend_config.global_translation_unit_decls in
         lookup_ahead_for_vardecl context pointer var_name kind decl_list )

(* Traverses the body of the method top down and collects the                                 *)
(* variable definitions in a map in the context. To be able to find the right variable name   *)
(* in the reference instructions, all the variable names are also saved in a map from pointers *)
(* to variable names to be used in the translation of the method's body.                      *)
let rec get_variables_stmt context (stmt : Clang_ast_t.stmt) : unit =
  match stmt with
  | DeclStmt(_, lstmt, decl_list) ->
      get_variables_decls context decl_list;
      get_fun_locals context lstmt;
  | DeclRefExpr(stmt_info, stmt_list, expr_info, decl_ref_expr_info) ->
      (* Notice that DeclRefExpr is the reference to a declared var/function/enum... *)
      (* so no declaration here *)
      Printing.log_out "Collecting variables, passing from DeclRefExpr '%s'\n"
        stmt_info.Clang_ast_t.si_pointer;
      let var_name = CTrans_utils.get_name_decl_ref_exp_info decl_ref_expr_info stmt_info in
      let kind = CTrans_utils.get_decl_kind decl_ref_expr_info in
      (match kind with
       | `EnumConstant | `ObjCIvar | `CXXMethod | `ObjCProperty -> ()
       | _ ->
           let pvar = lookup_var stmt_info context stmt_info.Clang_ast_t.si_pointer var_name kind in
           CContext.LocalVars.add_pointer_var stmt_info.Clang_ast_t.si_pointer pvar context)
  | CompoundStmt(stmt_info, lstmt) ->
      Printing.log_out "Collecting variables, passing from CompoundStmt '%s'\n"
        stmt_info.Clang_ast_t.si_pointer;
      CContext.LocalVars.enter_and_leave_scope context get_fun_locals lstmt
  | ForStmt(stmt_info, lstmt) ->
      Printing.log_out "Collecting variables, passing from ForStmt '%s'\n"
        stmt_info.Clang_ast_t.si_pointer;
      CContext.LocalVars.enter_and_leave_scope context get_fun_locals lstmt
  | _ ->
      let lstmt = Ast_utils.get_stmts_from_stmt stmt in
      get_fun_locals context lstmt

and get_fun_locals context (stmts : Clang_ast_t.stmt list) : unit =
  match stmts with
  | [] -> ()
  | stmt:: rest ->
      (get_variables_stmt context stmt);
      (get_fun_locals context rest)

(* Collects the local of a function. *)
and get_variables_decls context (decl_list : Clang_ast_t.decl list) : unit =
  let do_one_decl decl =
    match decl with
    | VarDecl (decl_info, name_info, qual_type, var_decl_info) ->
        Printing.log_out "Collecting variables, passing from VarDecl '%s'\n" decl_info.Clang_ast_t.di_pointer;
        let name = name_info.Clang_ast_t.ni_name in
        let typ = get_var_type context.CContext.tenv name qual_type in
        (match var_decl_info.Clang_ast_t.vdi_storage_class with
         | Some "static" ->
             let pname = Cfg.Procdesc.get_proc_name context.CContext.procdesc in
             let static_name = (Procname.to_string pname)^"_"^name in
             CGlobal_vars.add static_name typ;
             let var = Sil.mk_pvar_global (Mangled.from_string static_name) in
             CContext.LocalVars.add_pointer_var decl_info.Clang_ast_t.di_pointer var context
         | _ ->
             CContext.LocalVars.add_local_var context name typ decl_info.Clang_ast_t.di_pointer
               (CFrontend_utils.General_utils.is_static_var var_decl_info))
    | CXXRecordDecl(di, n_info, ot, dl, dci, rdi)
    | RecordDecl(di, n_info, ot, dl, dci, rdi) ->
        let typ = CTypes_decl.get_declaration_type context.CContext.tenv context.CContext.namespace
            di n_info.Clang_ast_t.ni_name ot dl dci rdi in
        CTypes_decl.add_struct_to_tenv context.CContext.tenv typ
    | TypedefDecl (decl_info, name_info, opt_type, typedef_decl_info) ->
        CTypes_decl.do_typedef_declaration context.CContext.tenv context.CContext.namespace
          decl_info name_info.Clang_ast_t.ni_name opt_type typedef_decl_info
    | StaticAssertDecl decl_info -> (* We do not treat Assertions. *)
        Printing.log_out
          "WARNING: When collecting variables, passing from StaticAssertDecl '%s'. Skipped.\n"
          decl_info.Clang_ast_t.di_pointer
    | _ -> Printing.log_out
             "!!! When collecting locals of a function found '%s'. Cannot continue\n\n"
             (Clang_ast_j.string_of_decl decl);
        assert false in
  list_iter do_one_decl decl_list
