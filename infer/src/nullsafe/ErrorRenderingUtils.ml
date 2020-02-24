(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let is_object_nullability_self_explanatory ~object_expression (object_origin : TypeOrigin.t) =
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
  | NullConst _ ->
      (* Expect either a local variable or null literal (latter case is trivial) *)
      String.equal object_expression "null"
  | MethodParameter (Normal {mangled}) ->
      (* Either local variable or literally parameter. In latter case, its nullability is
         self-explanatory because the user can quickly see the current method signature.
      *)
      let param_name = Mangled.to_string mangled in
      String.equal object_expression param_name
  | MethodParameter ObjectEqualsOverride ->
      false
  | Field {field_name} ->
      (* Either local variable or expression like `<smth>.field_name`. Latter case is trivial:
         the user can quickly go to field_name definition and see if its annotation. *)
      let field_name_str = Fieldname.get_field_name field_name in
      String.is_suffix object_expression ~suffix:field_name_str
  | MethodCall {pname; annotated_signature= {model_source}} ->
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
  | NonnullConst _
  | This
  | New
  | CallToGetKnownToContainsKey
  | ArrayLengthResult
  | ArrayAccess
  | InferredNonnull _
  | OptimisticFallback
  | Undef ->
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


let mk_coming_from nullsafe_mode nullability =
  match (nullsafe_mode, nullability) with
  | NullsafeMode.Strict, Nullability.ThirdPartyNonnull
  | NullsafeMode.Strict, Nullability.UncheckedNonnull ->
      Some "non-strict classes"
  | NullsafeMode.Strict, Nullability.LocallyCheckedNonnull ->
      Some "nullsafe-local classes"
  | NullsafeMode.Local _, Nullability.ThirdPartyNonnull ->
      Some "third-party classes"
  | NullsafeMode.Local _, Nullability.UncheckedNonnull ->
      Some "non-nullsafe classes"
  | _ ->
      None


let mk_recommendation nullsafe_mode nullability what =
  match (nullsafe_mode, nullability) with
  | NullsafeMode.Strict, Nullability.ThirdPartyNonnull
  | NullsafeMode.Strict, Nullability.UncheckedNonnull
  | NullsafeMode.Strict, Nullability.LocallyCheckedNonnull ->
      Some (F.sprintf "make %s nullsafe strict" what)
  | NullsafeMode.Local _, Nullability.UncheckedNonnull ->
      Some (F.sprintf "make %s nullsafe" what)
  | _ ->
      None


let mk_recommendation_for_third_party_field nullsafe_mode nullability field =
  match (nullsafe_mode, nullability) with
  | NullsafeMode.Strict, Nullability.ThirdPartyNonnull ->
      Some (F.sprintf "access %s via a nullsafe strict getter" field)
  | NullsafeMode.Local _, Nullability.ThirdPartyNonnull ->
      Some (F.sprintf "access %s via a nullsafe getter" field)
  | _ ->
      None


let get_info object_origin nullsafe_mode bad_nullability =
  let open IOption.Let_syntax in
  match object_origin with
  | TypeOrigin.MethodCall {pname; call_loc} ->
      let offending_object =
        F.asprintf "%a" MarkupFormatter.pp_monospaced
          (Procname.to_simplified_string ~withclass:true pname)
      in
      let object_loc = call_loc in
      let suggested_third_party_sig_file =
        ThirdPartyAnnotationInfo.lookup_related_sig_file_for_proc
          (ThirdPartyAnnotationGlobalRepo.get_repo ())
          pname
      in
      let what_is_used = "Result of this call" in
      (* Two main cases: it is either FB owned code or third party.
         We determine the difference based on presense of suggested_third_party_sig_file.
      *)
      let+ coming_from_explanation, recommendation, issue_type =
        match suggested_third_party_sig_file with
        | Some sig_file_name ->
            (* Dereferences or assignment violations with regular Nullables
               should not be special cased *)
            if Nullability.equal Nullable bad_nullability then None
            else
              return
                ( "not vetted third party methods"
                , F.sprintf "add the correct signature to %s"
                    (ThirdPartyAnnotationGlobalRepo.get_user_friendly_third_party_sig_file_name
                       ~filename:sig_file_name)
                , IssueType.eradicate_unvetted_third_party_in_nullsafe )
        | None ->
            let* from = mk_coming_from nullsafe_mode bad_nullability in
            let+ recommendation =
              let what_to_strictify =
                Option.value (get_method_class_name pname) ~default:offending_object
              in
              mk_recommendation nullsafe_mode bad_nullability what_to_strictify
            in
            let issue_type = IssueType.eradicate_unchecked_usage_in_nullsafe in
            (from, recommendation, issue_type)
      in
      { offending_object
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type }
  | TypeOrigin.Field {field_name; access_loc} ->
      let qualified_name =
        F.asprintf "%a" MarkupFormatter.pp_monospaced (Fieldname.to_simplified_string field_name)
      in
      let unqualified_name =
        F.asprintf "%a" MarkupFormatter.pp_monospaced (Fieldname.get_field_name field_name)
      in
      let object_loc = access_loc in
      (* TODO: currently we do not support third-party annotations for fields. Because of this,
         render error like it is a non-stict class. *)
      let what_is_used = "This field" in
      let* coming_from_explanation = mk_coming_from nullsafe_mode bad_nullability in
      let+ recommendation =
        Option.first_some
          (mk_recommendation_for_third_party_field nullsafe_mode bad_nullability unqualified_name)
          (mk_recommendation nullsafe_mode bad_nullability (get_field_class_name field_name))
      in
      let issue_type = IssueType.eradicate_unchecked_usage_in_nullsafe in
      { offending_object= qualified_name
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type }
  | _ ->
      None


let mk_special_nullsafe_issue ~nullsafe_mode ~bad_nullability ~bad_usage_location object_origin =
  let open IOption.Let_syntax in
  let+ { offending_object
       ; object_loc
       ; coming_from_explanation
       ; what_is_used
       ; recommendation
       ; issue_type } =
    get_info object_origin nullsafe_mode bad_nullability
  in
  let description =
    F.asprintf
      "%s: `@Nullsafe%a` mode prohibits using values coming from %s without a check. %s is used at \
       line %d. Either add a local check for null or assertion, or %s."
      offending_object NullsafeMode.pp nullsafe_mode coming_from_explanation what_is_used
      bad_usage_location.Location.line recommendation
  in
  (description, issue_type, object_loc)


let find_alternative_nonnull_method_description nullable_origin =
  let open IOption.Let_syntax in
  match nullable_origin with
  | TypeOrigin.MethodCall {pname= Procname.Java java_pname as pname} ->
      let* ModelTables.{package_name; class_name; method_name} =
        Models.find_nonnullable_alternative pname
      in
      let+ original_package_name = Procname.Java.get_package java_pname in
      if String.equal original_package_name package_name then
        (* The same package that is from origin - omit name for simplicity *)
        class_name ^ "." ^ method_name ^ "()"
      else (* Fully qualified name *)
        package_name ^ "." ^ class_name ^ "." ^ method_name ^ "()"
  | _ ->
      None
