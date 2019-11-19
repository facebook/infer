(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = Nullability.t [@@deriving compare]

type dereference_type =
  | MethodCall of Typ.Procname.t
  | AccessToField of Typ.Fieldname.t
  | AccessByIndex of {index_desc: string}
  | ArrayLengthAccess
[@@deriving compare]

let check ~is_strict_mode nullability =
  match nullability with
  | Nullability.Nullable ->
      Error nullability
  | Nullability.DeclaredNonnull ->
      if is_strict_mode then Error nullability else Ok ()
  | Nullability.Nonnull ->
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


let violation_description _ dereference_type ~nullable_object_descr ~nullable_object_origin =
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
          (MF.monospaced_to_string (Typ.Procname.to_simplified_string method_name))
    | AccessToField field_name ->
        Format.sprintf "accessing field %s"
          (MF.monospaced_to_string (Typ.Fieldname.to_simplified_string field_name))
    | AccessByIndex {index_desc} ->
        Format.sprintf "accessing at index %s" (MF.monospaced_to_string index_desc)
    | ArrayLengthAccess ->
        "accessing its length"
  in
  let suffix =
    get_origin_opt ~nullable_object_descr nullable_object_origin
    |> Option.bind ~f:(fun origin -> TypeOrigin.get_description origin)
    |> Option.value_map ~f:(fun origin -> ": " ^ origin) ~default:"."
  in
  Format.sprintf "%s is nullable and is not locally checked for null when %s%s"
    what_is_dereferred_str action_descr suffix
