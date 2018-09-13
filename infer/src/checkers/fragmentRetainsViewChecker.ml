(*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

let rec format_typ typ =
  match typ.Typ.desc with
  | Typ.Tptr (t, _) when Language.curr_language_is Java ->
      format_typ t
  | Typ.Tstruct name ->
      Typ.Name.name name
  | _ ->
      Typ.to_string typ


let format_field f =
  if Language.curr_language_is Java then Typ.Fieldname.Java.get_field f
  else Typ.Fieldname.to_string f


let format_method pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      Typ.Procname.Java.get_method pname_java
  | _ ->
      Typ.Procname.to_string pname


let report_error fragment_typ fld fld_typ summary pdesc =
  let pname = Procdesc.get_proc_name pdesc in
  let description =
    Printf.sprintf
      "Fragment %s does not nullify View field %s (type %s) in %s. If this Fragment is placed on \
       the back stack, a reference to this (probably dead) View will be retained. In general, it \
       is a good idea to initialize View's in onCreateView, then nullify them in onDestroyView."
      (format_typ fragment_typ) (format_field fld) (format_typ fld_typ) (format_method pname)
  in
  let loc = Procdesc.get_loc pdesc in
  Reporting.log_error summary ~loc IssueType.checkers_fragment_retain_view description


let callback_fragment_retains_view_java pname_java {Callbacks.proc_desc; summary; tenv} =
  (* TODO: complain if onDestroyView is not defined, yet the Fragment has View fields *)
  (* TODO: handle fields nullified in callees in the same file *)
  let is_on_destroy_view =
    String.equal (Typ.Procname.Java.get_method pname_java) "onDestroyView"
  in
  let fld_typ_is_view typ =
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct tname}, _) ->
        AndroidFramework.is_view tenv tname
    | _ ->
        false
  in
  (* is [fldname] a View type declared by [class_typename]? *)
  let is_declared_view_typ class_typename (fldname, fld_typ, _) =
    let fld_classname = Typ.Name.Java.from_string (Typ.Fieldname.Java.get_class fldname) in
    Typ.Name.equal fld_classname class_typename && fld_typ_is_view fld_typ
  in
  if is_on_destroy_view then
    let class_typename = Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name pname_java) in
    match Tenv.lookup tenv class_typename with
    | Some {fields} when AndroidFramework.is_fragment tenv class_typename ->
        let declared_view_fields = List.filter ~f:(is_declared_view_typ class_typename) fields in
        let fields_nullified = PatternMatch.get_fields_nullified proc_desc in
        (* report if a field is declared by C, but not nulled out in C.onDestroyView *)
        List.iter
          ~f:(fun (fname, fld_typ, ia) ->
            if
              not
                ( Annotations.ia_ends_with ia Annotations.auto_cleanup
                || Typ.Fieldname.Set.mem fname fields_nullified )
            then report_error (Typ.mk (Tstruct class_typename)) fname fld_typ summary proc_desc )
          declared_view_fields
    | _ ->
        ()


let callback_fragment_retains_view ({Callbacks.summary} as args) : Summary.t =
  let proc_name = Summary.get_proc_name summary in
  ( match proc_name with
  | Typ.Procname.Java pname_java ->
      callback_fragment_retains_view_java pname_java args
  | _ ->
      () ) ;
  summary
