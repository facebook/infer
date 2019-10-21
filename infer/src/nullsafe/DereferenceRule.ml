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


let violation_description _ dereference_type ~nullable_object_descr ~origin_descr =
  let module MF = MarkupFormatter in
  let nullable_object_descr =
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
  Format.sprintf "%s is nullable and is not locally checked for null when %s. %s"
    nullable_object_descr action_descr origin_descr
