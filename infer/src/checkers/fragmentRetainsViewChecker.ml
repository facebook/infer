(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

module P = Printf
open Utils

let report_error fld fld_typ pname pdesc =
  let retained_view = "CHECKERS_FRAGMENT_RETAINS_VIEW" in
  let fld_decl_class, fld_name =
    Ident.java_fieldname_get_class fld, Ident.java_fieldname_get_field fld in
  let description =
    P.sprintf
      "Fragment %s does not nullify View field %s (type %s) in onDestroyView. If the Fragment is placed on the back stack, a reference to the View may be retained."
      fld_decl_class
      fld_name
      (Sil.typ_to_string (Sil.typ_strip_ptr fld_typ)) in
  let loc = Cfg.Procdesc.get_loc pdesc in
  let exn = Exceptions.Checkers (retained_view, Localise.verbatim_desc description) in
  Reporting.log_error pname ~loc:(Some loc) exn

let callback_fragment_retains_view { Callbacks.proc_desc; proc_name; tenv } =
  (* TODO: complain if onDestroyView is not defined, yet the Fragment has View fields *)
  (* TODO: handle fields nullified in callees in the same file *)
  let is_on_destroy_view = Procname.java_get_method proc_name = "onDestroyView" in
  (* this is needlessly complicated because field types are Tvars instead of Tstructs *)
  let fld_typ_is_view = function
    | Sil.Tptr (Sil.Tvar tname, _) ->
        begin
          match Sil.tenv_lookup tenv tname with
          | Some typ -> AndroidFramework.is_view typ tenv
          | None -> false
        end
    | _ -> false in
  (* is [fldname] a View type declared by [class_typename]? *)
  let is_declared_view_typ class_typename (fldname, fld_typ, _) =
    let fld_classname = Typename.Java.from_string (Ident.java_fieldname_get_class fldname) in
    Typename.equal fld_classname class_typename && fld_typ_is_view fld_typ in
  if is_on_destroy_view then
    begin
      let class_typename = Typename.Java.from_string (Procname.java_get_class proc_name) in
      match Sil.tenv_lookup tenv class_typename with
      | Some (Sil.Tstruct { Sil.struct_name = Some _; instance_fields }
              as typ) when AndroidFramework.is_fragment typ tenv ->
          let declared_view_fields =
            IList.filter (is_declared_view_typ class_typename) instance_fields in
          let fields_nullified = PatternMatch.get_fields_nullified proc_desc in
          (* report if a field is declared by C, but not nulled out in C.onDestroyView *)
          IList.iter
            (fun (fname, typ, _) ->
               if not (Ident.FieldSet.mem fname fields_nullified) then
                 report_error fname typ proc_name proc_desc)
            declared_view_fields
      | _ -> ()
    end
