(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type log_t =
  ?ltr:Errlog.loc_trace -> ?extras:Jsonbug_t.extra -> Checker.t -> IssueType.t -> string -> unit

let log_issue_from_errlog ?severity_override err_log ~loc ~node ~session ~ltr ~access ~extras
    checker (issue_to_report : IssueToReport.t) =
  let issue_type = issue_to_report.issue_type in
  if (not Config.filtering) (* no-filtering takes priority *) || issue_type.IssueType.enabled then
    let doc_url = issue_type.doc_url in
    let linters_def_file = issue_type.linters_def_file in
    Errlog.log_issue ?severity_override err_log ~loc ~node ~session ~ltr ~linters_def_file ~doc_url
      ~access ~extras checker issue_to_report


let log_frontend_issue errlog ~loc ~node_key ~ltr exn =
  let node = Errlog.FrontendNode {node_key} in
  log_issue_from_errlog errlog ~loc ~node ~session:0 ~ltr ~access:None ~extras:None Linters exn


let log_issue_from_summary ?severity_override proc_desc err_log ~node ~session ~loc ~ltr ?extras
    checker exn =
  let procname = Procdesc.get_proc_name proc_desc in
  let is_java_generated_method =
    match procname with
    | Procname.Java java_pname ->
        Procname.Java.is_generated java_pname
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
  let should_suppress_lint =
    Language.curr_language_is Java
    && Annotations.ia_is_suppress_lint
         (Procdesc.get_attributes proc_desc).ProcAttributes.method_annotation.return
  in
  if should_suppress_lint || is_java_generated_method || is_java_external_package then
    Logging.debug Analysis Medium "Reporting is suppressed!@\n" (* Skip the reporting *)
  else
    log_issue_from_errlog ?severity_override err_log ~loc ~node ~session ~ltr ~access:None ~extras
      checker exn


let mk_issue_to_report issue_type error_message =
  {IssueToReport.issue_type; description= Localise.verbatim_desc error_message; ocaml_pos= None}


let log_issue_from_summary_simplified ?severity_override attrs err_log ~loc ?(ltr = []) ?extras
    checker issue_type error_message =
  let issue_to_report = mk_issue_to_report issue_type error_message in
  log_issue_from_summary ?severity_override attrs err_log ~node:Errlog.UnknownNode ~session:0 ~loc
    ~ltr ?extras checker issue_to_report


let log_issue attrs err_log ~loc ?ltr ?extras checker issue_type error_message =
  log_issue_from_summary_simplified attrs err_log ~loc ?ltr ?extras checker issue_type error_message


let log_issue_external procname ~issue_log ?severity_override ~loc ~ltr ?access ?extras checker
    issue_type error_message =
  let issue_to_report = mk_issue_to_report issue_type error_message in
  let issue_log, errlog = IssueLog.get_or_add issue_log ~proc:procname in
  let node = Errlog.UnknownNode in
  log_issue_from_errlog ?severity_override errlog ~loc ~node ~session:0 ~ltr ~access ~extras checker
    issue_to_report ;
  issue_log


let is_suppressed ?(field_name = None) tenv proc_attributes kind =
  let lookup = Tenv.lookup tenv in
  (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
     suppressed with the following annotations:
     - @android.annotation.SuppressLint("checker-error-name")
     - @some.PrefixErrorName
     where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
  let annotation_matches (a : Annot.t) =
    let normalize str = Str.global_replace (Str.regexp "[_-]") "" (String.lowercase str) in
    let drop_prefix str = Str.replace_first (Str.regexp "^[A-Za-z]+_") "" str in
    let normalized_equal s1 a2 =
      Annot.(
        has_matching_str_value a2.value ~pred:(fun s -> String.equal (normalize s1) (normalize s)))
    in
    let is_parameter_suppressed () =
      String.is_suffix a.class_name ~suffix:Annotations.suppress_lint
      && List.exists ~f:(normalized_equal kind.IssueType.unique_id) a.parameters
    in
    let is_annotation_suppressed () =
      String.is_suffix
        ~suffix:(normalize (drop_prefix kind.IssueType.unique_id))
        (normalize a.class_name)
    in
    is_parameter_suppressed () || is_annotation_suppressed ()
  in
  let is_method_suppressed () =
    Annotations.ma_has_annotation_with proc_attributes.ProcAttributes.method_annotation
      annotation_matches
  in
  let is_field_suppressed () =
    match (field_name, PatternMatch.get_this_type_nonstatic_methods_only proc_attributes) with
    | Some field_name, Some t -> (
      match Struct.get_field_type_and_annotation ~lookup field_name t with
      | Some (_, ia) ->
          Annotations.ia_has_annotation_with ia annotation_matches
      | None ->
          false )
    | _ ->
        false
  in
  let is_class_suppressed () =
    match PatternMatch.get_this_type_nonstatic_methods_only proc_attributes with
    | Some t -> (
      match PatternMatch.type_get_annotation tenv t with
      | Some ia ->
          Annotations.ia_has_annotation_with ia annotation_matches
      | None ->
          false )
    | None ->
        false
  in
  is_method_suppressed () || is_field_suppressed () || is_class_suppressed ()
