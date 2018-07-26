(*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type log_t =
  ?loc:Location.t -> ?node_id:int * Caml.Digest.t -> ?session:int -> ?ltr:Errlog.loc_trace
  -> ?linters_def_file:string -> ?doc_url:string -> ?access:string -> ?extras:Jsonbug_t.extra
  -> exn -> unit

type log_issue_from_errlog = Errlog.t -> log_t

let log_issue_from_errlog procname ?clang_method_kind err_kind err_log ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access ?extras exn =
  let clang_method_kind =
    Option.map clang_method_kind ~f:ProcAttributes.string_of_clang_method_kind
  in
  let issue_type = (Exceptions.recognize_exception exn).name in
  if not Config.filtering (* no-filtering takes priority *) || issue_type.IssueType.enabled then
    let loc = match loc with None -> State.get_loc () | Some loc -> loc in
    let node_id =
      match node_id with
      | None ->
          (State.get_node_id_key () :> int * Caml.Digest.t)
      | Some node_id ->
          node_id
    in
    let session =
      match session with None -> (State.get_session () :> int) | Some session -> session
    in
    let ltr = match ltr with None -> State.get_loc_trace () | Some ltr -> ltr in
    Errlog.log_issue procname ?clang_method_kind err_kind err_log loc node_id session ltr
      ?linters_def_file ?doc_url ?access ?extras exn


let log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file ?doc_url
    ?access ?extras exn =
  let attrs = Summary.get_attributes summary in
  let procname = attrs.proc_name in
  let clang_method_kind = attrs.clang_method_kind in
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
    log_issue_from_errlog procname ~clang_method_kind err_kind err_log ?loc ?node_id ?session ?ltr
      ?linters_def_file ?doc_url ?access ?extras exn


let log_issue_deprecated ?(store_summary= false) err_kind proc_name ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access ?extras:_ exn =
  match Summary.get proc_name with
  | Some summary ->
      log_issue_from_summary err_kind summary ?loc ?node_id ?session ?ltr ?linters_def_file
        ?doc_url ?access exn ;
      if store_summary then
        (* TODO (#16348004): This is currently needed as ThreadSafety works as a cluster checker *)
        Summary.store summary
  | None ->
      L.(die InternalError)
        "Trying to report error on procedure %a, but cannot because no summary exists for this \
         procedure. Did you mean to log the error on the caller of %a instead?"
        Typ.Procname.pp proc_name Typ.Procname.pp proc_name


let log_error = log_issue_from_summary Exceptions.Kerror

let log_warning = log_issue_from_summary Exceptions.Kwarning

let log_error_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kerror


let log_warning_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kwarning


let log_info_deprecated ?(store_summary= false) =
  log_issue_deprecated ~store_summary Exceptions.Kinfo


let log_issue_external procname ?clang_method_kind err_kind ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access ?extras exn =
  let errlog = IssueLog.get_errlog procname in
  log_issue_from_errlog procname ?clang_method_kind err_kind errlog ?loc ?node_id ?session ?ltr
    ?linters_def_file ?doc_url ?access ?extras exn


let is_suppressed ?(field_name= None) tenv proc_desc kind =
  let lookup = Tenv.lookup tenv in
  let proc_attributes = Procdesc.get_attributes proc_desc in
  (* Errors can be suppressed with annotations. An error of kind CHECKER_ERROR_NAME can be
         suppressed with the following annotations:
         - @android.annotation.SuppressLint("checker-error-name")
         - @some.PrefixErrorName
         where the kind matching is case - insensitive and ignores '-' and '_' characters. *)
  let annotation_matches (a: Annot.t) =
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
