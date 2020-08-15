(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module UserFriendlyNullable = struct
  type t = ExplainablyNullable of explainably_nullable_kind | UntrustedNonnull of untrusted_kind

  and explainably_nullable_kind = Nullable | Null

  and untrusted_kind = ThirdPartyNonnull | UncheckedNonnull | LocallyCheckedNonnull

  let from_nullability = function
    | Nullability.Nullable ->
        Some (ExplainablyNullable Nullable)
    | Nullability.Null ->
        Some (ExplainablyNullable Null)
    | Nullability.UncheckedNonnull ->
        Some (UntrustedNonnull UncheckedNonnull)
    | Nullability.LocallyCheckedNonnull ->
        Some (UntrustedNonnull LocallyCheckedNonnull)
    | Nullability.ThirdPartyNonnull ->
        Some (UntrustedNonnull ThirdPartyNonnull)
    | Nullability.LocallyTrustedNonnull ->
        (* The value is trusted in the current mode by definition, hence is not treated as nullable. *)
        None
    | Nullability.StrictNonnull ->
        None
end

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
  | CurrMethodParameter (Normal {mangled}) ->
      (* Either local variable or literally parameter. In latter case, its nullability is
         self-explanatory because the user can quickly see the current method signature.
      *)
      let param_name = Mangled.to_string mangled in
      String.equal object_expression param_name
  | CurrMethodParameter ObjectEqualsOverride ->
      (* This needs a dedicated explanation for the user *)
      false
  | Field {field_name} ->
      (* Either local variable or expression like `<smth>.field_name`. Latter case is trivial:
         the user can quickly go to field_name definition and see if its annotation. *)
      let field_name_str = Fieldname.get_field_name field_name in
      String.is_suffix object_expression ~suffix:field_name_str
  | MethodCall {pname; annotated_signature= {kind}} -> (
    match kind with
    | FirstParty | ThirdParty Unregistered ->
        (* Either local variable or expression like <smth>.method_name(...).
           Latter case is self-explanatory: it is easy to the user to jump to definition
           and check out the method annotation.
        *)
        let method_name = Procname.Java.to_simplified_string pname in
        String.is_suffix object_expression ~suffix:method_name
    | ThirdParty ModelledInternally | ThirdParty (InThirdPartyRepo _) ->
        (* This is non-trivial and should always be explained to the user *)
        false )
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
  | OptimisticFallback ->
      false


type message_info =
  { offending_object: string
  ; object_loc: Location.t
  ; coming_from_explanation: string
  ; what_is_used: string
  ; recommendation: string
  ; issue_type: IssueType.t }

let get_field_class_name field_name =
  let class_with_field = Fieldname.to_simplified_string field_name in
  String.rsplit2 class_with_field ~on:'.'
  |> Option.value_map ~f:(fun (classname, _) -> classname) ~default:"the field class"


let mk_coming_from_unchecked_or_locally_checked_case_only nullsafe_mode untrusted_kind =
  match (nullsafe_mode, untrusted_kind) with
  | NullsafeMode.Strict, UserFriendlyNullable.UncheckedNonnull ->
      "non-strict classes"
  | NullsafeMode.Strict, UserFriendlyNullable.LocallyCheckedNonnull ->
      "nullsafe-local classes"
  | NullsafeMode.Local _, _ ->
      "non-nullsafe classes"
  | NullsafeMode.Default, _ ->
      Logging.die InternalError
        "mk_coming_from_unchecked_or_locally_checked_case_only:: not applicable to default mode"
  | _, UserFriendlyNullable.ThirdPartyNonnull ->
      Logging.die InternalError
        "mk_coming_from_unchecked_or_locally_checked_case_only:: not applicable to \
         ThirdPartyNonnull case"


let mk_strictification_advice_unchecked_or_locally_checked_case_only nullsafe_mode untrusted_kind
    ~what_to_strictify =
  match untrusted_kind with
  | UserFriendlyNullable.UncheckedNonnull | UserFriendlyNullable.LocallyCheckedNonnull -> (
    match nullsafe_mode with
    | NullsafeMode.Strict ->
        F.sprintf "make `%s` nullsafe strict" what_to_strictify
    | NullsafeMode.Local _ ->
        F.sprintf "make `%s` @Nullsafe (or add it to trust list)" what_to_strictify
    | NullsafeMode.Default ->
        Logging.die InternalError
          "mk_recommendation_unchecked_or_locally_checked_case_only:: should not be called for \
           default mode" )
  | UserFriendlyNullable.ThirdPartyNonnull ->
      Logging.die InternalError
        "mk_recommendation_unchecked_or_locally_checked_case_only:: not applicable to \
         ThirdPartyNonnull case"


let mk_recommendation_for_third_party_field nullsafe_mode field =
  match nullsafe_mode with
  | NullsafeMode.Strict ->
      F.sprintf "access %s via a nullsafe strict getter" field
  | NullsafeMode.Local _ ->
      F.sprintf "access %s via a nullsafe getter" field
  | NullsafeMode.Default ->
      Logging.die InternalError
        "mk_recommendation_for_third_party_field:: Should not happen: we should tolerate third \
         party in default mode"


let get_info object_origin nullsafe_mode untrusted_kind =
  match object_origin with
  | TypeOrigin.MethodCall {pname; call_loc} ->
      let offending_object =
        F.asprintf "%a" MarkupFormatter.pp_monospaced
          (Procname.Java.to_simplified_string ~withclass:true pname)
      in
      let object_loc = call_loc in
      let what_is_used = "Result of this call" in
      let coming_from_explanation, recommendation, issue_type =
        match untrusted_kind with
        | UserFriendlyNullable.ThirdPartyNonnull ->
            let suggested_third_party_sig_file =
              ThirdPartyAnnotationInfo.lookup_related_sig_file_for_proc
                (ThirdPartyAnnotationGlobalRepo.get_repo ())
                pname
            in
            let where_to_add_signature =
              Option.value_map suggested_third_party_sig_file
                ~f:(fun sig_file_name ->
                  ThirdPartyAnnotationGlobalRepo.get_user_friendly_third_party_sig_file_name
                    ~filename:sig_file_name )
                  (* this can happen when third party is registered in a deprecated way (not in third party repository) *)
                ~default:"the third party signature storage"
            in
            ( "not vetted third party methods"
            , F.sprintf "add the correct signature to %s" where_to_add_signature
            , IssueType.eradicate_unvetted_third_party_in_nullsafe )
        | UserFriendlyNullable.UncheckedNonnull | UserFriendlyNullable.LocallyCheckedNonnull ->
            let from =
              mk_coming_from_unchecked_or_locally_checked_case_only nullsafe_mode untrusted_kind
            in
            let recommendation =
              let what_to_strictify = Procname.Java.get_simple_class_name pname in
              mk_strictification_advice_unchecked_or_locally_checked_case_only nullsafe_mode
                untrusted_kind ~what_to_strictify
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
      let coming_from_explanation, recommendation, issue_type =
        match untrusted_kind with
        | UserFriendlyNullable.ThirdPartyNonnull ->
            ( "third-party classes"
            , mk_recommendation_for_third_party_field nullsafe_mode unqualified_name
            , IssueType.eradicate_unvetted_third_party_in_nullsafe )
        | UserFriendlyNullable.UncheckedNonnull | UserFriendlyNullable.LocallyCheckedNonnull ->
            let from =
              mk_coming_from_unchecked_or_locally_checked_case_only nullsafe_mode untrusted_kind
            in
            let recommendation =
              mk_strictification_advice_unchecked_or_locally_checked_case_only nullsafe_mode
                untrusted_kind ~what_to_strictify:(get_field_class_name field_name)
            in
            (from, recommendation, IssueType.eradicate_unchecked_usage_in_nullsafe)
      in
      { offending_object= qualified_name
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type }
  | other ->
      Logging.die InternalError
        "get_info:: untrusted_kind is possible only for MethodCall and Field origins, got %s \
         instead"
        (TypeOrigin.to_string other)


let mk_nullsafe_issue_for_untrusted_values ~nullsafe_mode ~untrusted_kind ~bad_usage_location
    object_origin =
  let { offending_object
      ; object_loc
      ; coming_from_explanation
      ; what_is_used
      ; recommendation
      ; issue_type } =
    get_info object_origin nullsafe_mode untrusted_kind
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
  | TypeOrigin.MethodCall {pname} ->
      let* ModelTables.{package_name; class_name; method_name} =
        Models.find_nonnullable_alternative pname
      in
      let+ original_package_name = Procname.Java.get_package pname in
      if String.equal original_package_name package_name then
        (* The same package that is from origin - omit name for simplicity *)
        class_name ^ "." ^ method_name ^ "()"
      else (* Fully qualified name *)
        package_name ^ "." ^ class_name ^ "." ^ method_name ^ "()"
  | _ ->
      None
