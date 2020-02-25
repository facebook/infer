(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Representation of a (non local) type in Java program, with added information about its
    nullability, according to the source code. Nullability information might come either from
    explicit annotations, or from other sources, including conventions about defaults. Note that
    nullsafe omits Nullability information in types used for local variable declarations: this
    information is inferred according to flow-sensitive inferrence rule. *)

(** See {!Nullability.t} for explanation *)
type t =
  | Nullable of nullable_origin
  | ThirdPartyNonnull
  | UncheckedNonnull of unchecked_nonnull_origin
  | LocallyCheckedNonnull
  | StrictNonnull of strict_nonnull_origin
[@@deriving compare]

and nullable_origin =
  | AnnotatedNullable  (** The type is expicitly annotated with [@Nullable] in the code *)
  | AnnotatedPropagatesNullable
      (** If a function param is annotated as [@PropagatesNullable], this param is automatically
          nullable *)
  | HasPropagatesNullableInParam
      (** If a method has at least one param marked as [@PropagatesNullable], return value is
          automatically nullable *)
  | ModelledNullable  (** nullsafe knows it is nullable via its internal models *)
[@@deriving compare]

and unchecked_nonnull_origin =
  | AnnotatedNonnull
      (** The type is explicitly annotated as non nullable via one of nonnull annotations Nullsafe
          recognizes *)
  | ImplicitlyNonnull
      (** Infer was run in mode where all not annotated (non local) types are treated as non
          nullable *)

and strict_nonnull_origin =
  | ModelledNonnull  (** nullsafe knows it is non-nullable via its internal models *)
  | StrictMode  (** under strict mode we consider non-null declarations to be trusted *)
  | PrimitiveType  (** Primitive types are non-nullable by language design *)
  | EnumValue
      (** Java enum value are statically initialized with non-nulls according to language semantics *)
[@@deriving compare]

let get_nullability = function
  | Nullable _ ->
      Nullability.Nullable
  | ThirdPartyNonnull ->
      Nullability.ThirdPartyNonnull
  | UncheckedNonnull _ ->
      Nullability.UncheckedNonnull
  | LocallyCheckedNonnull ->
      Nullability.LocallyCheckedNonnull
  | StrictNonnull _ ->
      Nullability.StrictNonnull


let pp fmt t =
  let string_of_nullable_origin nullable_origin =
    match nullable_origin with
    | AnnotatedNullable ->
        "@"
    | AnnotatedPropagatesNullable ->
        "propagates"
    | HasPropagatesNullableInParam ->
        "<-propagates"
    | ModelledNullable ->
        "model"
  in
  let string_of_declared_nonnull_origin origin =
    match origin with AnnotatedNonnull -> "@" | ImplicitlyNonnull -> "implicit"
  in
  let string_of_nonnull_origin nonnull_origin =
    match nonnull_origin with
    | ModelledNonnull ->
        "model"
    | StrictMode ->
        "strict"
    | PrimitiveType ->
        "primitive"
    | EnumValue ->
        "enum"
  in
  match t with
  | Nullable origin ->
      F.fprintf fmt "Nullable[%s]" (string_of_nullable_origin origin)
  | ThirdPartyNonnull ->
      F.fprintf fmt "ThirdPartyNonnull"
  | UncheckedNonnull origin ->
      F.fprintf fmt "UncheckedNonnull[%s]" (string_of_declared_nonnull_origin origin)
  | LocallyCheckedNonnull ->
      F.fprintf fmt "LocallyCheckedNonnull"
  | StrictNonnull origin ->
      F.fprintf fmt "StrictNonnull[%s]" (string_of_nonnull_origin origin)


let of_type_and_annotation ~is_trusted_callee ~nullsafe_mode ~is_third_party typ annotations =
  if not (PatternMatch.type_is_class typ) then StrictNonnull PrimitiveType
  else if Annotations.ia_is_nullable annotations then
    let nullable_origin =
      if Annotations.ia_is_propagates_nullable annotations then AnnotatedPropagatesNullable
      else AnnotatedNullable
    in
    Nullable nullable_origin
  else
    match nullsafe_mode with
    | NullsafeMode.Strict ->
        StrictNonnull StrictMode
    | NullsafeMode.Local _ ->
        LocallyCheckedNonnull
    | NullsafeMode.Default ->
        if is_third_party then ThirdPartyNonnull
        else
          let preliminary_nullability =
            if Annotations.ia_is_nonnull annotations then UncheckedNonnull AnnotatedNonnull
            else UncheckedNonnull ImplicitlyNonnull
          in
          if is_trusted_callee then LocallyCheckedNonnull else preliminary_nullability
