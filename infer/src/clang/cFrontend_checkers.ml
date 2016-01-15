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
let checker_strong_delegate_warning decl_info pname obj_c_property_decl_info =
  Printing.log_out "Checking for STRONG_DELEGATE property warning\n";
  let delegate_regexp = Str.regexp_string_case_fold "delegate" in
  let pname_contains_delegate = try
      Str.search_forward delegate_regexp pname.Clang_ast_t.ni_name 0 >= 0
    with Not_found -> false in
  let condition = pname_contains_delegate
                  && ObjcProperty_decl.is_strong_property obj_c_property_decl_info in
  let warning_desc =
    { name = "STRONG_DELEGATE_WARNING";
      description = "Property or ivar "^pname.Clang_ast_t.ni_name^" declared strong";
      suggestion = "In general delegates should be declared weak or assign";
      loc = CLocation.get_sil_location_from_range decl_info.Clang_ast_t.di_source_range true;
    } in
  (condition, warning_desc)

(* Direct Atomic Property access:
   a property declared atomic should not be accessed directly via its ivar *)
let direct_atomic_property_access context stmt_info ivar_name =
  let tenv = CContext.get_tenv context in
  let mname = Cfg.Procdesc.get_proc_name (CContext.get_procdesc context) in
  let ivar, cname = match ivar_name with
    | Some n ->
        General_utils.mk_class_field_name n,
        Ast_utils.get_class_name_from_member n
    | _ -> Ident.create_fieldname (Mangled.from_string "") 0, "" in
  let tname = Typename.TN_csu (Csu.Class, Mangled.from_string cname) in
  let loc = CLocation.get_sil_location_from_range stmt_info.Clang_ast_t.si_source_range true in
  match Sil.tenv_lookup tenv tname with
  | Some Sil.Tstruct (flds1, flds2, _, _, _, _, _) ->
      (* We give the warning when:
         (1) the property has the atomic attribute and
         (2) the access of the ivar is not in a getter or setter method.
         (3) the access of the ivar is not in the init method
         Last two conditions avoids false positives *)
      let condition = (CField_decl.is_ivar_atomic ivar (flds1 @ flds2))
                      && not (CContext.is_curr_proc_objc_getter context ivar)
                      && not (CContext.is_curr_proc_objc_setter context ivar)
                      && not (Procname.is_constructor mname)
                      && not (Procname.is_objc_dealloc mname) in
      let warning_desc = {
        name = "DIRECT_ATOMIC_PROPERTY_ACCESS";
        description = "Direct access to ivar " ^ (Ident.fieldname_to_string ivar) ^
                      " of an atomic property";
        suggestion = "Accessing an ivar of an atomic property makes the property nonatomic";
        loc = loc;
      } in
      (condition, Some warning_desc)
  | _ -> (false, None) (* No warning *)

(* CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK: C++ references
   should not be captured in blocks.  *)
let captured_cxx_ref_in_objc_block stmt_info captured_vars =
  let is_cxx_ref (_, typ) =
    match typ with
    | Sil.Tptr(_, Sil.Pk_reference) -> true
    | _ -> false in
  let capt_refs = IList.filter is_cxx_ref captured_vars in
  match capt_refs with
  | [] -> (false, None) (* No warning *)
  | _ ->
      let pvar_descs =
        IList.fold_left (fun s (v, _)  -> s ^ " '" ^ (Sil.pvar_to_string v) ^ "' ") "" capt_refs in
      let loc = CLocation.get_sil_location_from_range stmt_info.Clang_ast_t.si_source_range true in
      let warning_desc = {
        name = "CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK";
        description = "C++ Reference variable(s) " ^ pvar_descs ^
                      " captured by Objective-C block";
        suggestion = "C++ References are unmanaged and may be invalid " ^
                     "by the time the block executes.";
        loc = loc;
      } in
      (true, Some warning_desc)
