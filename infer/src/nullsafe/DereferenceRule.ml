(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {nullability: Nullability.t} [@@deriving compare]

module ReportableViolation = struct
  type t = {nullsafe_mode: NullsafeMode.t; violation: violation}

  type dereference_type =
    | MethodCall of Procname.t
    | AccessToField of Fieldname.t
    | AccessByIndex of {index_desc: string}
    | ArrayLengthAccess
  [@@deriving compare]

  let get_origin_opt ~nullable_object_descr origin =
    let should_show_origin =
      match nullable_object_descr with
      | Some object_expression ->
          not (ErrorRenderingUtils.is_object_nullability_self_explanatory ~object_expression origin)
      | None ->
          true
    in
    if should_show_origin then Some origin else None


  let get_description {nullsafe_mode; violation= {nullability}} ~dereference_location
      dereference_type ~nullable_object_descr ~nullable_object_origin =
    let module MF = MarkupFormatter in
    let special_message =
      if not (NullsafeMode.equal NullsafeMode.Default nullsafe_mode) then
        ErrorRenderingUtils.mk_special_nullsafe_issue ~nullsafe_mode ~bad_nullability:nullability
          ~bad_usage_location:dereference_location nullable_object_origin
      else None
    in
    match special_message with
    | Some desc ->
        desc
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
          match nullability with
          | Nullability.Null ->
              Format.sprintf
                "NullPointerException will be thrown at this line! %s is `null` and is \
                 dereferenced via %s%s."
                what_is_dereferred_str action_descr origin_descr
          | Nullability.Nullable ->
              Format.sprintf "%s is nullable and is not locally checked for null when %s%s.%s"
                what_is_dereferred_str action_descr origin_descr alternative_recommendation
          | other ->
              Logging.die InternalError
                "violation_description:: invariant violation: unexpected nullability %a"
                Nullability.pp other
        in
        (description, IssueType.eradicate_nullable_dereference, dereference_location)


  let get_severity {nullsafe_mode} = NullsafeMode.severity nullsafe_mode
end

let check nullability =
  match nullability with
  (* StrictNonnull is the only "real" value that is not null according to type system rules.
     Other values can not be fully trusted.
  *)
  | Nullability.StrictNonnull ->
      Ok ()
  | _ ->
      Error {nullability}


let to_reportable_violation nullsafe_mode ({nullability} as violation) =
  if Nullability.is_considered_nonnull ~nullsafe_mode nullability then None
  else Some ReportableViolation.{nullsafe_mode; violation}
