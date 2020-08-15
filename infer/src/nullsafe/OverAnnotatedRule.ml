(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

type violation = {declared_nullability: Nullability.t; can_be_narrowed_to: Nullability.t}
[@@deriving compare]

let check ~what ~by_rhs_upper_bound =
  match (what, by_rhs_upper_bound) with
  | Nullability.Nullable, Nullability.StrictNonnull
  | Nullability.Nullable, Nullability.UncheckedNonnull ->
      Error {declared_nullability= what; can_be_narrowed_to= by_rhs_upper_bound}
  | Nullability.Null, Nullability.StrictNonnull | Nullability.Null, Nullability.UncheckedNonnull ->
      (* Null, Nonnull pais is an edge case that we don't expect to arise in practice for two reasons:
         1. The only way to declare something as Null is to declare it is java.lang.Void, but nullsafe
         does not know about it just yet.
         2. Even if it knew, such could should not compile: the only way it could compile if it returns `null` directly.
      *)
      Error {declared_nullability= what; can_be_narrowed_to= by_rhs_upper_bound}
  | _ ->
      Ok ()


type violation_type =
  | FieldOverAnnoted of Fieldname.t
  | ReturnOverAnnotated of Procname.Java.t  (** Return value of a method can be made non-nullable *)
[@@deriving compare]

let violation_description _ violation_type =
  let module MF = MarkupFormatter in
  let nullable_annotation = "@Nullable" in
  match violation_type with
  | FieldOverAnnoted field_name ->
      Format.asprintf "Field %a is always initialized in the constructor but is declared %a"
        MF.pp_monospaced
        (Fieldname.to_simplified_string field_name)
        MF.pp_monospaced nullable_annotation
  | ReturnOverAnnotated proc_name ->
      Format.asprintf "Method %a is annotated with %a but never returns null." MF.pp_monospaced
        (Procname.Java.to_simplified_string proc_name)
        MF.pp_monospaced nullable_annotation
