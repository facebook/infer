(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open CFrontend_utils
open General_utils


(* === Warnings on properties === *)

(* Strong Delegate Warning: a property with name delegate should not be declared strong *)
let checker_strong_delegate_warning class_decl_info pname ptype =
  Printing.log_out "Checking for STRONG_DELEGATE property warning\n";
  let delegate_regexp = Str.regexp_string_case_fold "delegate" in
  let pname_contains_delegate = try
      Str.search_forward delegate_regexp pname.Clang_ast_t.ni_name 0 >= 0
    with Not_found -> false in
  let condition = pname_contains_delegate
                  && ObjcProperty_decl.is_strong_property ptype in
  let warning_desc =
    { name = "STRONG_DELEGATE_WARNING";
      description = "Property or ivar "^pname.Clang_ast_t.ni_name^" declared strong";
      suggestion = "In general delegates should be declared weak or assign";
      loc = CLocation.get_sil_location_from_range class_decl_info.Clang_ast_t.di_source_range true;
    } in
  (condition, warning_desc)

(* Direct Atomic Property access:
   a property declared atomic should not be accessed directly via its ivar *)
let direct_atomic_property_access context stmt_info ivar_name =
  let tenv = CContext.get_tenv context in
  let curr_class = CContext.get_curr_class context in
  let mname = Cfg.Procdesc.get_proc_name (CContext.get_procdesc context) in
  let ivar, cname = match ivar_name with
    | Some n ->
        General_utils.mk_class_field_name n,
        Ast_utils.get_class_name_from_member n
    | _ -> Ident.create_fieldname (Mangled.from_string "") 0, "" in
  let tname = Sil.TN_csu (Sil.Class, Mangled.from_string cname) in
  let loc = CLocation.get_sil_location_from_range stmt_info.Clang_ast_t.si_source_range true in
  match Sil.tenv_lookup tenv tname with
  | Some Sil.Tstruct (flds1, flds2, _, _, _, _, _) ->
      (* We give the warning when:
         (1) the property has the atomic attribute and
         (2) the access of the ivar is not in a getter or setter method. This condition
             avoids false positives *)
      let condition = (CField_decl.is_ivar_atomic ivar (flds1 @ flds2))
                      && not (ObjcProperty_decl.is_getter_setter curr_class mname ivar_name) in
      let warning_desc = {
        name = "DIRECT_ATOMIC_PROPERTY_ACCESS";
        description = "Direct access to ivar " ^ (Ident.fieldname_to_string ivar) ^
                      " of an atomic property";
        suggestion = "Accessing an ivar of an atomic property makes the property nonatomic";
        loc = loc;
      } in
      (condition, Some warning_desc)
  | _ -> (false, None) (* No warning *)
