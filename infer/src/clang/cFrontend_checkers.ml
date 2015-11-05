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
let checker_strong_delegate_warning pname ptype =
  Printing.log_out "Checking for STRONG_DELEGATE property warning\n";
  let delegate_regexp = Str.regexp_string_case_fold "delegate" in
  let pname_contains_delegate = try
      Str.search_forward delegate_regexp pname.Clang_ast_t.ni_name 0 >= 0
    with Not_found -> false in
  let condition = pname_contains_delegate
                  && ObjcProperty_decl.is_strong_property ptype in
  let warning_desc= {name = "STRONG_DELEGATE_WARNING";
                     description = "Property or ivar "^pname.Clang_ast_t.ni_name^" declared strong";
                     suggestion = "In general delegates should be declared weak or assign";
                     loc = string_of_int (ObjcProperty_decl.property_line ptype);
                    } in
  (condition, warning_desc)
