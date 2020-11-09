(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {nullability: InferredNullability.t} [@@deriving compare]

module ProvisionalViolation = struct
  type t = {offending_annotations: ProvisionalAnnotation.t list}

  let offending_annotations {offending_annotations} = offending_annotations

  let from {nullability} =
    let offending_annotations = InferredNullability.get_provisional_annotations nullability in
    if List.is_empty offending_annotations then None else Some {offending_annotations}
end

module ReportableViolation = struct
  type t = {nullsafe_mode: NullsafeMode.t; violation: violation}

  type dereference_type =
    | MethodCall of Procname.Java.t
    | AccessToField of Fieldname.t
    | AccessByIndex of {index_desc: string}
    | ArrayLengthAccess
  [@@deriving compare]

  let from nullsafe_mode ({nullability} as violation) =
    if
      Nullability.is_considered_nonnull ~nullsafe_mode
        (InferredNullability.get_nullability nullability)
    then None
    else Some {nullsafe_mode; violation}


  let get_origin_opt ~nullable_object_descr origin =
    let should_show_origin =
      match nullable_object_descr with
      | Some object_expression ->
          not (ErrorRenderingUtils.is_object_nullability_self_explanatory ~object_expression origin)
      | None ->
          true
    in
    if should_show_origin then Some origin else None


  let mk_nullsafe_issue_for_explicitly_nullable_values ~explicit_kind ~dereference_type
      dereference_location ~nullsafe_mode ~nullable_object_descr ~nullable_object_origin =
    let module MF = MarkupFormatter in
    let what_is_dereferred_str =
      match dereference_type with
      | MethodCall _ | AccessToField _ -> (
        match nullable_object_descr with
        | None ->
            "Object"
        (* Just describe an object itself *)
        | Some descr ->
            MF.monospaced_to_string descr )
      | ArrayLengthAccess | AccessByIndex _ -> (
        (* In Java, those operations can be applied only to arrays *)
        match nullable_object_descr with
        | None ->
            "Array"
        | Some descr ->
            Format.sprintf "Array %s" (MF.monospaced_to_string descr) )
    in
    let action_descr =
      match dereference_type with
      | MethodCall method_name ->
          Format.sprintf "calling %s"
            (MF.monospaced_to_string (Procname.Java.to_simplified_string method_name))
      | AccessToField field_name ->
          Format.sprintf "accessing field %s"
            (MF.monospaced_to_string (Fieldname.to_simplified_string field_name))
      | AccessByIndex {index_desc} ->
          Format.sprintf "accessing at index %s" (MF.monospaced_to_string index_desc)
      | ArrayLengthAccess ->
          "accessing its length"
    in
    let origin_descr =
      get_origin_opt ~nullable_object_descr nullable_object_origin
      |> Option.bind ~f:(fun origin -> TypeOrigin.get_description origin)
      |> Option.value_map ~f:(fun origin -> ": " ^ origin) ~default:""
    in
    let alternative_method_description =
      ErrorRenderingUtils.find_alternative_nonnull_method_description nullable_object_origin
    in
    let alternative_recommendation =
      Option.value_map alternative_method_description
        ~f:(fun descr ->
          Format.asprintf " If this is intentional, use %a instead." MF.pp_monospaced descr )
        ~default:""
    in
    let description =
      match explicit_kind with
      | ErrorRenderingUtils.UserFriendlyNullable.Null ->
          Format.sprintf
            "NullPointerException will be thrown at this line! %s is `null` and is dereferenced \
             via %s%s."
            what_is_dereferred_str action_descr origin_descr
      | ErrorRenderingUtils.UserFriendlyNullable.Nullable ->
          Format.sprintf "%s is nullable and is not locally checked for null when %s%s.%s"
            what_is_dereferred_str action_descr origin_descr alternative_recommendation
    in
    let nullable_methods =
      match nullable_object_origin with TypeOrigin.MethodCall origin -> [origin] | _ -> []
    in
    NullsafeIssue.make ~description ~issue_type:IssueType.eradicate_nullable_dereference
      ~loc:dereference_location
      ~severity:(NullsafeMode.severity nullsafe_mode)
      ~field_name:None
    |> NullsafeIssue.with_nullable_methods nullable_methods


  let make_nullsafe_issue {nullsafe_mode; violation= {nullability}} ~dereference_location
      dereference_type ~nullable_object_descr =
    let user_friendly_nullable =
      ErrorRenderingUtils.UserFriendlyNullable.from_nullability
        (InferredNullability.get_nullability nullability)
      |> IOption.if_none_eval ~f:(fun () ->
             Logging.die InternalError
               "get_description:: Dereference violation should not be possible for non-nullable \
                values" )
    in
    let nullable_object_origin = InferredNullability.get_simple_origin nullability in
    match user_friendly_nullable with
    | ErrorRenderingUtils.UserFriendlyNullable.UntrustedNonnull untrusted_kind ->
        (* Attempt to dereference a value which is not explictly declared as nullable,
           but still can not be trusted in this particular mode.
        *)
        ErrorRenderingUtils.mk_nullsafe_issue_for_untrusted_values ~nullsafe_mode ~untrusted_kind
          ~bad_usage_location:dereference_location nullable_object_origin
    | ErrorRenderingUtils.UserFriendlyNullable.ExplainablyNullable explicit_kind ->
        (* Attempt to dereference value that can be explained to the user as nullable. *)
        mk_nullsafe_issue_for_explicitly_nullable_values ~explicit_kind ~dereference_type
          ~nullsafe_mode dereference_location ~nullable_object_descr ~nullable_object_origin
end

let check nullability =
  match InferredNullability.get_nullability nullability with
  (* StrictNonnull is the only "real" value that is not null according to type system rules.
     Other values can not be fully trusted.
  *)
  | Nullability.StrictNonnull ->
      Ok ()
  | _ ->
      Error {nullability}
