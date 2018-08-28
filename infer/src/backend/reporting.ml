(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type log_t =
     ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string
  -> ?doc_url:string
  -> ?access:string
  -> ?extras:Jsonbug_t.extra
  -> exn
  -> unit

let log_issue_from_errlog procname ~clang_method_kind severity err_log ~loc ~node ~ltr
    ~linters_def_file ~doc_url ~access ~extras exn =
  let issue_type = (Exceptions.recognize_exception exn).name in
  if (not Config.filtering) (* no-filtering takes priority *) || issue_type.IssueType.enabled then
    let session = (State.get_session () :> int) in
    Errlog.log_issue procname ~clang_method_kind severity err_log ~loc ~node ~session ~ltr
      ~linters_def_file ~doc_url ~access ~extras exn


let log_frontend_issue procname severity errlog ~loc ~node_key ~ltr ~linters_def_file ~doc_url exn
    =
  let node = Errlog.FrontendNode {node_key} in
  log_issue_from_errlog procname ~clang_method_kind:None severity errlog ~loc ~node ~ltr
    ~linters_def_file ~doc_url ~access:None ~extras:None exn


let log_issue_from_summary severity summary ~node ~loc ?ltr ?linters_def_file ?doc_url ?access
    ?extras exn =
  let attrs = Summary.get_attributes summary in
  let procname = attrs.proc_name in
  let is_java_generated_method =
    match procname with
    | Typ.Procname.Java java_pname ->
        Typ.Procname.Java.is_generated java_pname
    | _ ->
        false
  in
  let should_suppress_lint =
    Language.curr_language_is Java
    && Annotations.ia_is_suppress_lint
         (fst (Summary.get_attributes summary).ProcAttributes.method_annotation)
  in
  if should_suppress_lint || is_java_generated_method then () (* Skip the reporting *)
  else
    let err_log = Summary.get_err_log summary in
    let clang_method_kind = Some attrs.clang_method_kind in
    let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
    let node = Errlog.BackendNode {node} in
    log_issue_from_errlog procname ~clang_method_kind severity err_log ~loc ~node ~ltr
      ~linters_def_file ~doc_url ~access ~extras exn


let log_issue_deprecated severity proc_name ?node ?loc ?ltr ?linters_def_file ?doc_url ?access
    ?extras:_ exn =
  match Summary.get proc_name with
  | Some summary ->
      let node = match node with None -> State.get_node () | Some node -> node in
      let loc = match loc with None -> State.get_loc () | Some loc -> loc in
      log_issue_from_summary severity summary ~node ~loc ?ltr ?linters_def_file ?doc_url ?access
        exn
  | None ->
      L.(die InternalError)
        "Trying to report error on procedure %a, but cannot because no summary exists for this \
         procedure. Did you mean to log the error on the caller of %a instead?"
        Typ.Procname.pp proc_name Typ.Procname.pp proc_name


let log_issue_from_summary_simplified severity summary ~loc ?ltr ?linters_def_file ?doc_url ?access
    ?extras exn =
  let node = State.get_node () in
  log_issue_from_summary severity summary ~node ~loc ?ltr ?linters_def_file ?doc_url ?access
    ?extras exn


let log_error = log_issue_from_summary_simplified Exceptions.Error

let log_warning = log_issue_from_summary_simplified Exceptions.Warning

let log_issue_external procname severity ~loc ~ltr ?access issue_type error_message =
  let exn = Exceptions.Checkers (issue_type, Localise.verbatim_desc error_message) in
  let errlog = IssueLog.get_errlog procname in
  let node = Errlog.UnknownNode in
  log_issue_from_errlog procname ~clang_method_kind:None severity errlog ~loc ~node ~ltr
    ~linters_def_file:None ~doc_url:None ~access ~extras:None exn


let is_suppressed ?(field_name = None) tenv proc_desc kind =
  let lookup = Tenv.lookup tenv in
  let proc_attributes = Procdesc.get_attributes proc_desc in
  (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
         suppressed with the following annotations:
         - @android.annotation.SuppressLint("checker-error-name")
         - @some.PrefixErrorName
         where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
  let annotation_matches (a : Annot.t) =
    let normalize str = Str.global_replace (Str.regexp "[_-]") "" (String.lowercase str) in
    let drop_prefix str = Str.replace_first (Str.regexp "^[A-Za-z]+_") "" str in
    let normalized_equal s1 s2 = String.equal (normalize s1) (normalize s2) in
    let is_parameter_suppressed () =
      String.is_suffix a.class_name ~suffix:Annotations.suppress_lint
      && List.mem ~equal:normalized_equal a.parameters kind.IssueType.unique_id
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
    match (field_name, PatternMatch.get_this_type proc_attributes) with
    | Some field_name, Some t -> (
      match Typ.Struct.get_field_type_and_annotation ~lookup field_name t with
      | Some (_, ia) ->
          Annotations.ia_has_annotation_with ia annotation_matches
      | None ->
          false )
    | _ ->
        false
  in
  let is_class_suppressed () =
    match PatternMatch.get_this_type proc_attributes with
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
