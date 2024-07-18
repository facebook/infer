(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type log_t =
     ?loc_instantiated:Location.t
  -> ?ltr:Errlog.loc_trace
  -> ?extras:Jsonbug_t.extra
  -> ?suggestion:string
  -> Checker.t
  -> IssueType.t
  -> string
  -> unit

module Suppression = struct
  let does_annotation_suppress_issue =
    let prefix_regexp = Str.regexp "^[A-Za-z]+_" in
    fun (kind : IssueType.t) (annot : Annot.t) ->
      let normalize str =
        String.lowercase str |> String.filter ~f:(function ' ' | '_' | '-' -> false | _ -> true)
      in
      let is_parameter_suppressed () =
        let normalized_equal annot_param =
          let pred s =
            normalize s
            |> fun s ->
            String.equal s (normalize kind.IssueType.hum)
            || String.equal s (normalize kind.IssueType.unique_id)
          in
          Annot.(has_matching_str_value annot_param.value ~pred)
        in
        String.is_suffix annot.class_name ~suffix:Annotations.suppress_lint
        && List.exists ~f:normalized_equal annot.parameters
      in
      let drop_prefix str = Str.replace_first prefix_regexp "" str in
      let is_annotation_suppressed () =
        String.is_suffix
          ~suffix:(normalize (drop_prefix kind.IssueType.unique_id))
          (normalize annot.class_name)
      in
      is_parameter_suppressed () || is_annotation_suppressed ()


  let is_method_suppressed proc_attributes kind =
    Annotations.method_has_annotation_with proc_attributes.ProcAttributes.ret_annots
      (List.map proc_attributes.ProcAttributes.formals ~f:trd3)
      (does_annotation_suppress_issue kind)


  let is_field_suppressed field_name tenv proc_attributes kind =
    let lookup = Tenv.lookup tenv in
    match (field_name, PatternMatch.get_this_type_nonstatic_methods_only proc_attributes) with
    | Some field_name, Some t -> (
      match Struct.get_field_type_and_annotation ~lookup field_name t with
      | Some (_, ia) ->
          Annotations.ia_has_annotation_with ia (does_annotation_suppress_issue kind)
      | None ->
          false )
    | _ ->
        false


  let is_class_suppressed tenv proc_attributes kind =
    match PatternMatch.get_this_type_nonstatic_methods_only proc_attributes with
    | Some t -> (
      match PatternMatch.type_get_annotation tenv t with
      | Some ia ->
          Annotations.ia_has_annotation_with ia (does_annotation_suppress_issue kind)
      | None ->
          false )
    | None ->
        false


  let is_suppressed ?(field_name = None) tenv proc_attributes kind =
    (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
       suppressed with the following annotations:
       - @android.annotation.SuppressLint("checker-error-name")
       - @some.PrefixErrorName
       where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
    let is_method_suppressed () = is_method_suppressed proc_attributes kind in
    let is_field_suppressed () = is_field_suppressed field_name tenv proc_attributes kind in
    let is_class_suppressed () = is_class_suppressed tenv proc_attributes kind in
    is_method_suppressed () || is_field_suppressed () || is_class_suppressed ()
end

let log_issue_from_errlog ?severity_override err_log ~loc ~node ~session ~ltr ~access ~extras
    checker (issue_to_report : IssueToReport.t) =
  let issue_type = issue_to_report.issue_type in
  if (not Config.filtering) (* no-filtering takes priority *) || issue_type.IssueType.enabled then
    Errlog.log_issue ?severity_override err_log ~loc ~node ~session ~ltr ~access ~extras checker
      issue_to_report


let log_issue_from_summary ?severity_override proc_desc err_log ~node ~session ~loc ~ltr ?extras
    checker exn =
  let procname = Procdesc.get_proc_name proc_desc in
  let issue_type = exn.IssueToReport.issue_type in
  let is_java_generated_method =
    match procname with
    | Procname.Java java_pname ->
        Procname.Java.is_generated java_pname
    | Procname.CSharp csharp_pname ->
        Procname.CSharp.is_generated csharp_pname
    | _ ->
        false
  in
  let is_java_external_package =
    match procname with
    | Procname.Java java_pname ->
        Procname.Java.is_external java_pname
    | _ ->
        false
  in
  let check_suppress proc_attributes issue_type =
    if Config.suppress_lint_ignore_types then
      (* This is for backwards compatibility only!
         We should really honor the issues types specified as params to @SuppressLint *)
      Annotations.ia_is_suppress_lint proc_attributes.ProcAttributes.ret_annots
    else Suppression.is_method_suppressed proc_attributes issue_type
  in
  let should_suppress_lint =
    Language.curr_language_is Java && check_suppress (Procdesc.get_attributes proc_desc) issue_type
  in
  if should_suppress_lint || is_java_generated_method || is_java_external_package then
    Logging.debug Analysis Medium "Reporting is suppressed!@\n" (* Skip the reporting *)
  else
    log_issue_from_errlog ?severity_override err_log ~loc ~node ~session ~ltr ~access:None ~extras
      checker exn


let mk_issue_to_report ?suggestion issue_type error_message =
  { IssueToReport.issue_type
  ; description= Localise.verbatim_desc error_message ?suggestion
  ; ocaml_pos= None }


let log_issue_from_summary_simplified ?severity_override proc_desc err_log ~loc ?(ltr = []) ?extras
    ?suggestion checker issue_type error_message =
  let issue_to_report = mk_issue_to_report issue_type error_message ?suggestion in
  let node : Errlog.node =
    match AnalysisState.get_node () with None -> UnknownNode | Some node -> BackendNode {node}
  in
  log_issue_from_summary ?severity_override proc_desc err_log ~node ~session:0 ~loc ~ltr ?extras
    checker issue_to_report


let log_issue proc_desc err_log ~loc ?loc_instantiated ?ltr ?extras ?suggestion checker issue_type
    error_message =
  let ltr =
    Option.map ltr ~f:(fun default ->
        Option.value_map ~default loc_instantiated ~f:(fun loc_instantiated ->
            let depth = 0 in
            let tags = [] in
            Errlog.make_trace_element depth loc_instantiated "first instantiated at" tags :: default ) )
  in
  log_issue_from_summary_simplified proc_desc err_log ~loc ?ltr ?extras checker issue_type
    error_message ?suggestion


let log_issue_external procname ~issue_log ?severity_override ~loc ~ltr ?access ?extras ?suggestion
    checker issue_type error_message =
  let issue_to_report = mk_issue_to_report issue_type error_message ?suggestion in
  let issue_log, errlog = IssueLog.get_or_add issue_log ~proc:procname in
  let node = Errlog.UnknownNode in
  log_issue_from_errlog ?severity_override errlog ~loc ~node ~session:0 ~ltr ~access ~extras checker
    issue_to_report ;
  issue_log


let is_suppressed = Suppression.is_suppressed
