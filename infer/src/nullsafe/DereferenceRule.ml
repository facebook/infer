(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {nullsafe_mode: NullsafeMode.t; nullability: Nullability.t} [@@deriving compare]

type dereference_type =
  | MethodCall of Procname.t
  | AccessToField of Fieldname.t
  | AccessByIndex of {index_desc: string}
  | ArrayLengthAccess
[@@deriving compare]

let check ~nullsafe_mode nullability =
  match nullability with
  | Nullability.Nullable | Nullability.Null ->
      Error {nullsafe_mode; nullability}
  | Nullability.UncheckedNonnull -> (
    match nullsafe_mode with
    | NullsafeMode.Strict | NullsafeMode.Local (NullsafeMode.Trust.Only []) ->
        Error {nullsafe_mode; nullability}
    | NullsafeMode.Local (NullsafeMode.Trust.Only _typ_names) ->
        (* TODO(T61473665). For now treat as trust=none.  *)
        Error {nullsafe_mode; nullability}
    | NullsafeMode.Local NullsafeMode.Trust.All | NullsafeMode.Default ->
        Ok () )
  | Nullability.LocallyCheckedNonnull -> (
    match nullsafe_mode with
    | NullsafeMode.Default | NullsafeMode.Local _ ->
        Ok ()
    | NullsafeMode.Strict ->
        Error {nullsafe_mode; nullability} )
  | Nullability.StrictNonnull ->
      Ok ()


let get_origin_opt ~nullable_object_descr origin =
  let should_show_origin =
    match nullable_object_descr with
    | Some object_expression ->
        not (ErrorRenderingUtils.is_object_nullability_self_explanatory ~object_expression origin)
    | None ->
        true
  in
  if should_show_origin then Some origin else None


let violation_description {nullsafe_mode; nullability} ~dereference_location dereference_type
    ~nullable_object_descr ~nullable_object_origin =
  let module MF = MarkupFormatter in
  match nullability with
  | Nullability.UncheckedNonnull | Nullability.LocallyCheckedNonnull ->
      (* These types of violations are possible in non-default nullsafe mode. For them we
         provide tailored error messages. *)
      ErrorRenderingUtils.mk_special_nullsafe_issue ~nullsafe_mode ~bad_nullability:nullability
        ~bad_usage_location:dereference_location nullable_object_origin
  | _ ->
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
              (MF.monospaced_to_string (Procname.to_simplified_string method_name))
        | AccessToField field_name ->
            Format.sprintf "accessing field %s"
              (MF.monospaced_to_string (Fieldname.to_simplified_string field_name))
        | AccessByIndex {index_desc} ->
            Format.sprintf "accessing at index %s" (MF.monospaced_to_string index_desc)
        | ArrayLengthAccess ->
            "accessing its length"
      in
      let suffix =
        get_origin_opt ~nullable_object_descr nullable_object_origin
        |> Option.bind ~f:(fun origin -> TypeOrigin.get_description origin)
        |> Option.value_map ~f:(fun origin -> ": " ^ origin) ~default:""
      in
      let description =
        match nullability with
        | Nullability.Null ->
            Format.sprintf
              "NullPointerException will be thrown at this line! %s is `null` and is dereferenced \
               via %s%s."
              what_is_dereferred_str action_descr suffix
        | Nullability.Nullable ->
            Format.sprintf "%s is nullable and is not locally checked for null when %s%s."
              what_is_dereferred_str action_descr suffix
        | Nullability.UncheckedNonnull
        | Nullability.LocallyCheckedNonnull
        | Nullability.StrictNonnull ->
            Logging.die InternalError "Invariant violation: unexpected nullability"
      in
      (description, IssueType.eradicate_nullable_dereference, dereference_location)


let violation_severity {nullsafe_mode} = NullsafeMode.severity nullsafe_mode
