(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Make sure callbacks are always unregistered. drive the point home by reporting possible NPE's *)

let on_create_view = "onCreateView"

let on_destroy_view = "onDestroyView"

let rec format_typ typ =
  match typ.Typ.desc with
  | Typ.Tptr (t, _) when Language.curr_language_is Java ->
      format_typ t
  | Typ.Tstruct name ->
      Typ.Name.name name
  | _ ->
      Typ.to_string typ


let format_method pname =
  match pname with
  | Typ.Procname.Java pname_java ->
      Typ.Procname.Java.get_method pname_java
  | _ ->
      Typ.Procname.to_string pname


let report_warning class_name fld fld_typ summary =
  let pname = Summary.get_proc_name summary in
  let loc = Summary.get_loc summary in
  let pp_m = MarkupFormatter.pp_monospaced in
  let description =
    Format.asprintf
      "Fragment %a does not nullify View field %a (type %a) in %a. If this Fragment is placed on \
       the back stack, a reference to this (probably dead) View will be retained. In general, it \
       is a good idea to initialize View's in %a, then nullify them in %a."
      pp_m (Typ.Name.name class_name) pp_m
      (Typ.Fieldname.to_flat_string fld)
      pp_m (format_typ fld_typ) pp_m (format_method pname) pp_m on_create_view pp_m on_destroy_view
  in
  Reporting.log_warning summary ~loc IssueType.checkers_fragment_retain_view description


let callback_fragment_retains_view_java java_pname {Callbacks.summary; exe_env} =
  (* TODO: complain if onDestroyView is not defined, yet the Fragment has View fields *)
  (* TODO: handle fields nullified in callees in the same file *)
  let tenv = Exe_env.get_tenv exe_env (Summary.get_proc_name summary) in
  let is_on_destroy_view = String.equal (Typ.Procname.Java.get_method java_pname) on_destroy_view in
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
    let class_name = Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_pname) in
    match Tenv.lookup tenv class_name with
    | Some {fields} when AndroidFramework.is_fragment tenv class_name ->
        let declared_view_fields = List.filter ~f:(is_declared_view_typ class_name) fields in
        let fields_nullified = PatternMatch.get_fields_nullified (Summary.get_proc_desc summary) in
        (* report if a field is declared by C, but not nulled out in C.onDestroyView *)
        List.iter
          ~f:(fun (fname, fld_typ, ia) ->
            if
              not
                ( Annotations.ia_ends_with ia Annotations.auto_cleanup
                || Typ.Fieldname.Set.mem fname fields_nullified )
            then report_warning class_name fname fld_typ summary )
          declared_view_fields
    | _ ->
        ()


let callback_fragment_retains_view ({Callbacks.summary} as args) : Summary.t =
  let proc_name = Summary.get_proc_name summary in
  ( match proc_name with
  | Typ.Procname.Java java_pname ->
      callback_fragment_retains_view_java java_pname args
  | _ ->
      () ) ;
  summary
