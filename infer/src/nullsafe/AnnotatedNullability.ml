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

type t =
  | Nullable of nullable_origin
  | UncheckedNonnull of unchecked_nonnull_origin  (** See {!Nullability.t} for explanation *)
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
  | UncheckedNonnull _ ->
      Nullability.UncheckedNonnull
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
  | UncheckedNonnull origin ->
      F.fprintf fmt "UncheckedNonnull[%s]" (string_of_declared_nonnull_origin origin)
  | StrictNonnull origin ->
      F.fprintf fmt "StrictNonnull[%s]" (string_of_nonnull_origin origin)


let of_type_and_annotation ~(nullsafe_mode : NullsafeMode.t) typ annotations =
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
    | NullsafeMode.Default ->
        if Annotations.ia_is_nonnull annotations then UncheckedNonnull AnnotatedNonnull
          (* Currently, we treat not annotated types as nonnull *)
        else UncheckedNonnull ImplicitlyNonnull
