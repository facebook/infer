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
  if
    Nullability.is_strict_subtype ~subtype:by_rhs_upper_bound ~supertype:what
    && (* Suppress violations for anything apart from Nullable since such
          issues are not very actionable and/or clear for the user.
          E.g. we technically can suggest changing [DeclaredNonnull] to [Nonnull],
          but in practice that requires strictification the code, which is a
          separate effort.
       *)
    Nullability.equal what Nullable
  then Error {declared_nullability= what; can_be_narrowed_to= by_rhs_upper_bound}
  else Ok ()


type violation_type =
  | FieldOverAnnoted of Typ.Fieldname.t
  | ReturnOverAnnotated of Typ.Procname.t  (** Return value of a method can be made non-nullable *)
[@@deriving compare]

let violation_description _ violation_type =
  let module MF = MarkupFormatter in
  let nullable_annotation = "@Nullable" in
  match violation_type with
  | FieldOverAnnoted field_name ->
      Format.asprintf "Field %a is always initialized in the constructor but is declared %a"
        MF.pp_monospaced
        (Typ.Fieldname.to_simplified_string field_name)
        MF.pp_monospaced nullable_annotation
  | ReturnOverAnnotated proc_name ->
      Format.asprintf "Method %a is annotated with %a but never returns null." MF.pp_monospaced
        (Typ.Procname.to_simplified_string proc_name)
        MF.pp_monospaced nullable_annotation
