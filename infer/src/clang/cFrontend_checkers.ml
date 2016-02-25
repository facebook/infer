(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open CFrontend_utils

(* To create a new checker you should: *)
(* 1. Define a checker function, say my_checker, in this module. *)
(* my_checker should define: *)
(*   -a) a condition that determine if the checker fires *)
(*   -b) a warning_desc that describes the warning (see warning_desc definition) *)
(* 2. Add your checker to the CFrontend_checkers interface *)
(* 3. Decide in which element of the AST my_checker should be evaluated. *)
(*    - If it is a statement then you need to invoke my_checker from *)
(*    run_frontend_checkers_on_stmt in CFrontend_error module.*)
(*    - If it is a declaration invoke it from run_frontend_checkers_on_decl *)

type warning_desc = {
  name : string; (* name for the checker, this will be a kind of bug *)
  description : string; (* Description in the error message *)
  suggestion : string; (* an optional suggestion or correction *)
  loc : Location.t; (* location in the code *)
}

(* Helper functions *)

let property_name_contains_word pname word =
  let rexp = Str.regexp_string_case_fold word in
  try
    Str.search_forward rexp pname.Clang_ast_t.ni_name 0 >= 0
  with Not_found -> false

let location_from_sinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.si_source_range true

let location_from_dinfo info =
  CLocation.get_sil_location_from_range info.Clang_ast_t.di_source_range true

let proc_name_from_context context =
  Cfg.Procdesc.get_proc_name (CContext.get_procdesc context)

(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
let strong_delegate_warning decl_info pname obj_c_property_decl_info =
  let condition = property_name_contains_word pname "delegate"
                  && ObjcProperty_decl.is_strong_property obj_c_property_decl_info in
  if condition then
    Some { name = "STRONG_DELEGATE_WARNING";
           description = "Property or ivar "^pname.Clang_ast_t.ni_name^" declared strong";
           suggestion = "In general delegates should be declared weak or assign";
           loc = location_from_dinfo decl_info; }
  else None

(* Direct Atomic Property access:
   a property declared atomic should not be accessed directly via its ivar *)
let direct_atomic_property_access_warning context stmt_info ivar_name =
  let tenv = CContext.get_tenv context in
  let mname = proc_name_from_context context in
  let ivar, cname = match ivar_name with
    | Some n ->
        General_utils.mk_class_field_name n,
        Ast_utils.get_class_name_from_member n
    | _ -> Ident.create_fieldname (Mangled.from_string "") 0, "" in
  let tname = Typename.TN_csu (Csu.Class Csu.Objc, Mangled.from_string cname) in
  let condition = match Sil.tenv_lookup tenv tname with
    | Some Sil.Tstruct { Sil.instance_fields; static_fields } ->
        (* We give the warning when:
             (1) the property has the atomic attribute and
               (2) the access of the ivar is not in a getter or setter method.
                 (3) the access of the ivar is not in the init method
                   Last two conditions avoids false positives *)
        (CField_decl.is_ivar_atomic ivar (instance_fields @ static_fields))
        && not (CContext.is_curr_proc_objc_getter context ivar)
        && not (CContext.is_curr_proc_objc_setter context ivar)
        && not (Procname.is_constructor mname)
        && not (Procname.is_objc_dealloc mname)
    | _ -> false in
  if condition then
    Some {
      name = "DIRECT_ATOMIC_PROPERTY_ACCESS";
      description = "Direct access to ivar " ^ (Ident.fieldname_to_string ivar) ^
                    " of an atomic property";
      suggestion = "Accessing an ivar of an atomic property makes the property nonatomic";
      loc = location_from_sinfo stmt_info; }
  else None


(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
let captured_cxx_ref_in_objc_block_warning stmt_info captured_vars =
  let is_cxx_ref (_, typ) =
    match typ with
    | Sil.Tptr(_, Sil.Pk_reference) -> true
    | _ -> false in
  let capt_refs = IList.filter is_cxx_ref captured_vars in
  let pvar_descs =
    IList.fold_left (fun s (v, _)  -> s ^ " '" ^ (Sil.pvar_to_string v) ^ "' ") "" capt_refs in
  (* Fire if the list of captured references is not empty *)
  let condition = IList.length capt_refs > 0 in
  if condition then
    Some {
      name = "CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK";
      description = "C++ Reference variable(s) " ^ pvar_descs ^
                    " captured by Objective-C block";
      suggestion = "C++ References are unmanaged and may be invalid " ^
                   "by the time the block executes.";
      loc = location_from_sinfo stmt_info; }
  else None
