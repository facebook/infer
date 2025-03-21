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
  | Procname.Java pname_java ->
      Procname.Java.get_method pname_java
  | _ ->
      Procname.to_string pname


let report_warning proc_desc err_log class_name fld fld_typ =
  let pp_m = MarkupFormatter.pp_monospaced in
  let field_name = Fieldname.get_field_name fld in
  let description =
    Format.asprintf
      "Fragment %a does not nullify View field %a (type %a) in %a. If this Fragment is placed on \
       the back stack, a reference to this (probably dead) View will be retained."
      pp_m (Typ.Name.name class_name) pp_m field_name pp_m (format_typ fld_typ) pp_m
      (format_method (Procdesc.get_proc_name proc_desc))
  in
  let suggestion =
    Format.asprintf
      "In general, it is a good idea to initialize View's in %a, then nullify them in %a. Note \
       that you also need to make sure they are not being used afterwards."
      pp_m on_create_view pp_m on_destroy_view
  in
  let loc = Procdesc.get_loc proc_desc in
  let autofix =
    let replacement =
      Format.asprintf "\n %s = null%s" field_name
        (if String.is_suffix ~suffix:".java" (SourceFile.to_string loc.file) then ";" else "")
    in
    [ { Jsonbug_t.original= None
      ; replacement=
          None
          (* Add one to the line because we expect a call to super to be the first line in the function. *)
      ; additional= Some [{Jsonbug_t.line= loc.line + 1; column= 1; original= ""; replacement}] } ]
  in
  Reporting.log_issue ~suggestion proc_desc err_log ~loc FragmentRetainsView ~autofix
    IssueType.checkers_fragment_retain_view description


let callback_fragment_retains_view_java {IntraproceduralAnalysis.proc_desc; tenv; err_log}
    java_pname =
  (* TODO: complain if onDestroyView is not defined, yet the Fragment has View fields *)
  (* TODO: handle fields nullified in callees in the same file *)
  let is_on_destroy_view = String.equal (Procname.Java.get_method java_pname) on_destroy_view in
  let fld_typ_is_view typ =
    match typ.Typ.desc with
    | Typ.Tptr ({desc= Tstruct tname}, _) ->
        AndroidFramework.is_view tenv tname
    | _ ->
        false
  in
  (* is [fldname] a View type declared by [class_typename]? *)
  let is_declared_view_typ class_typename {Struct.name= fldname; typ= fld_typ} =
    let fld_classname = Fieldname.get_class_name fldname in
    Typ.Name.equal fld_classname class_typename && fld_typ_is_view fld_typ
  in
  if is_on_destroy_view then
    let class_name = Procname.Java.get_class_type_name java_pname in
    match Tenv.lookup tenv class_name with
    | Some {fields} when AndroidFramework.is_fragment tenv class_name ->
        let declared_view_fields = List.filter ~f:(is_declared_view_typ class_name) fields in
        let fields_nullified = PatternMatch.get_fields_nullified proc_desc in
        (* report if a field is declared by C, but not nulled out in C.onDestroyView *)
        List.iter
          ~f:(fun {Struct.name= fname; typ= fld_typ; annot= ia} ->
            if
              not
                ( Annotations.ia_ends_with ia Annotations.auto_cleanup
                || Fieldname.Set.mem fname fields_nullified )
            then report_warning proc_desc err_log class_name fname fld_typ )
          declared_view_fields
    | _ ->
        ()


let callback_fragment_retains_view ({IntraproceduralAnalysis.proc_desc} as analysis_data) : unit =
  let proc_name = Procdesc.get_proc_name proc_desc in
  match proc_name with
  | Procname.Java java_pname ->
      callback_fragment_retains_view_java analysis_data java_pname
  | _ ->
      ()
