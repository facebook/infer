(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

module P = Printf

let report_error fragment_typ fld fld_typ pname pdesc =
  let retained_view = "CHECKERS_FRAGMENT_RETAINS_VIEW" in
  let description = Localise.desc_fragment_retains_view fragment_typ fld fld_typ pname in
  let exn =  Exceptions.Checkers (retained_view, description) in
  let loc = Procdesc.get_loc pdesc in
  Reporting.log_error pname ~loc exn

let callback_fragment_retains_view_java
    pname_java { Callbacks.proc_desc; tenv } =
  (* TODO: complain if onDestroyView is not defined, yet the Fragment has View fields *)
  (* TODO: handle fields nullified in callees in the same file *)
  let is_on_destroy_view = String.equal (Procname.java_get_method pname_java) "onDestroyView" in
  let fld_typ_is_view = function
    | Typ.Tptr (Tstruct tname, _) -> AndroidFramework.is_view tenv tname
    | _ -> false in
  (* is [fldname] a View type declared by [class_typename]? *)
  let is_declared_view_typ class_typename (fldname, fld_typ, _) =
    let fld_classname = Typename.Java.from_string (Ident.java_fieldname_get_class fldname) in
    Typename.equal fld_classname class_typename && fld_typ_is_view fld_typ in
  if is_on_destroy_view then
    begin
      let class_typename =
        Typename.Java.from_string (Procname.java_get_class_name pname_java) in
      match Tenv.lookup tenv class_typename with
      | Some { fields } when AndroidFramework.is_fragment tenv class_typename ->
          let declared_view_fields =
            List.filter ~f:(is_declared_view_typ class_typename) fields in
          let fields_nullified = PatternMatch.get_fields_nullified proc_desc in
          (* report if a field is declared by C, but not nulled out in C.onDestroyView *)
          List.iter
            ~f:(fun (fname, fld_typ, _) ->
               if not (Ident.FieldSet.mem fname fields_nullified) then
                 report_error
                   (Tstruct class_typename) fname fld_typ (Procname.Java pname_java) proc_desc)
            declared_view_fields
      | _ -> ()
    end

let callback_fragment_retains_view ({ Callbacks.proc_name } as args) =
  match proc_name with
  | Procname.Java pname_java ->
      callback_fragment_retains_view_java pname_java args
  | _ ->
      ()
