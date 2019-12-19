(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_object_nullability_self_explanatory ~object_expression object_origin =
  (* Fundamentally, object can be of two kinds:
     1. Indirect: local variable that was instantiated before.
     In this case, normally origin is NOT trivial
     (because for complex flows it might be tricky to figure out where the offending
     initialization came from, and it is a good idea to at least point to the line).
     2. Direct: some sort of expression. In this case, this expression itself is often
     self-explanatory and it is OK to be offended.Infer
     NOTE: code below is heuristic, it indends to cover most practical cases and does not try
     to be 100% precise.
  *)
  match object_origin with
  | TypeOrigin.NullConst _ ->
      (* Expect either a local variable or null literal (latter case is trivial) *)
      String.equal object_expression "null"
  | TypeOrigin.MethodParameter {mangled} ->
      (* Either local variable or literally parameter. In latter case, its nullability is
         self-explanatory because the user can quickly see the current method signature.
      *)
      let param_name = Mangled.to_string mangled in
      String.equal object_expression param_name
  | TypeOrigin.Field {field_name} ->
      (* Either local variable or expression like `<smth>.field_name`. Latter case is trivial:
         the user can quickly go to field_name definition and see if its annotation. *)
      let field_name_str = Fieldname.get_field_name field_name in
      String.is_suffix object_expression ~suffix:field_name_str
  | TypeOrigin.MethodCall {pname; annotated_signature= {model_source}} ->
      let is_modelled = Option.is_some model_source in
      if is_modelled then (* This is non-trivial and should always be explained to the user *)
        false
      else
        (* Either local variable or expression like <smth>.method_name(...).
           Latter case is self-explanatory: it is easy to the user to jump to definition
           and check out the method annotation.
        *)
        let method_name = Procname.to_simplified_string pname in
        String.is_suffix object_expression ~suffix:method_name
  (* These cases are not yet supported because they normally mean non-nullable case, for which
     we don't render important messages yet.
  *)
  | TypeOrigin.NonnullConst _
  | TypeOrigin.This
  | TypeOrigin.New
  | TypeOrigin.ArrayLengthResult
  | TypeOrigin.ArrayAccess
  | TypeOrigin.InferredNonnull _
  | TypeOrigin.OptimisticFallback
  | TypeOrigin.Undef ->
      false


type message_info =
  { offending_object: string
  ; object_loc: Location.t
  ; coming_from_explanation: string
  ; what_is_used: string
  ; recommendation: string
  ; issue_type: IssueType.t }

let get_method_class_name procname =
  match procname with
  | Procname.Java java_pname ->
      Some (Procname.Java.get_simple_class_name java_pname)
  | _ ->
      None


let get_field_class_name field_name =
  let class_with_field = Fieldname.to_simplified_string field_name in
  String.rsplit2 class_with_field ~on:'.'
  |> Option.value_map ~f:(fun (classname, _) -> classname) ~default:"the field class"


let get_info object_origin =
  match object_origin with
  | TypeOrigin.MethodCall {pname; call_loc} ->
      let offending_object =
        Format.asprintf "%a" MarkupFormatter.pp_monospaced
          (Procname.to_simplified_string ~withclass:true pname)
      in
      let object_loc = call_loc in
      let suggested_third_party_sig_file =
        ThirdPartyAnnotationInfo.lookup_related_sig_file_by_package
          (ThirdPartyAnnotationGlobalRepo.get_repo ())
          pname
      in
      let what_is_used = "Result of this call" in
      (* Two main cases: it is either FB owned code or third party.
         We determine the difference based on presense of suggested_third_party_sig_file.
      *)
      let coming_from_explanation, recommendation, issue_type =
        let what_to_strictify =
          Option.value (get_method_class_name pname) ~default:offending_object
        in
        match suggested_third_party_sig_file with
        | None ->
            ( "non-strict classes"
            , Format.sprintf "strictify %s" what_to_strictify
            , IssueType.eradicate_forbidden_non_strict_in_strict )
        | Some sig_file_name ->
            ( "not vetted third party methods"
            , Format.sprintf "add the correct signature to %s"
                (ThirdPartyAnnotationGlobalRepo.get_user_friendly_third_party_sig_file_name
                   ~filename:sig_file_name)
            , IssueType.eradicate_unvetted_third_party_in_strict )
      in
      { offending_object
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type }
  | TypeOrigin.Field {field_name; access_loc} ->
      let offending_object =
        Format.asprintf "%a" MarkupFormatter.pp_monospaced
          (Fieldname.to_simplified_string field_name)
      in
      let object_loc = access_loc in
      (* TODO: currently we do not support third-party annotations for fields. Because of this,
         render error like it is a non-stict class. *)
      let what_is_used = "This field" in
      let coming_from_explanation = "non-strict classes" in
      let recommendation = Format.sprintf "strictify %s" (get_field_class_name field_name) in
      let issue_type = IssueType.eradicate_forbidden_non_strict_in_strict in
      { offending_object
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type }
  | _ ->
      Logging.die Logging.InternalError
        "Invariant violation: unexpected origin of declared non-nullable value"


let get_strict_mode_violation_issue ~bad_usage_location object_origin =
  let { offending_object
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type } =
    get_info object_origin
  in
  let description =
    Format.sprintf
      "%s: `@NullsafeStrict` mode prohibits using values coming from %s without a check. %s is \
       used at line %d. Either add a local check for null or assertion, or %s."
      offending_object coming_from_explanation what_is_used bad_usage_location.Location.line
      recommendation
  in
  (description, issue_type, object_loc)
