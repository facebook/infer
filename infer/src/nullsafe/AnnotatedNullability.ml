(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Representation of a (non local) type in Java program, with added information about its nullability,
    according to the source code.
    Nullability information might come either from explicit annotations, or from other sources,
    including conventions about defaults.
    Note that nullsafe omits Nullability information in types used for local variable declarations:
    this information is inferred according to flow-sensitive inferrence rule.
*)

(* TODO(T52947663) add notion of unknown nullability *)
type t = Nullable of nullable_origin | Nonnull of nonnull_origin [@@deriving compare]

and nullable_origin =
  | AnnotatedNullable  (** The type is expicitly annotated with @Nullable in the code *)
  | AnnotatedPropagatesNullable
      (** If a function param is annotated as @PropagatesNullable, this param is automatically nullable *)
  | HasPropagatesNullableInParam
      (** If a method has at least one param marked as @PropagatesNullable, return value is automatically nullable *)
  | ModelledNullable  (** nullsafe knows it is nullable via its internal models *)
[@@deriving compare]

and nonnull_origin =
  | AnnotatedNonnull
      (** The type is explicitly annotated as non nullable via one of nonnull annotations Nullsafe recognizes *)
  | NotAnnotatedHenceNullableMode
      (** Infer was run in mode where all not annotated (non local) types are treated as non nullable *)
  | ModelledNonnull  (** nullsafe knows it is non-nullable via its internal models *)
[@@deriving compare]

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
  let string_of_nonnull_origin nonnull_origin =
    match nonnull_origin with
    | AnnotatedNonnull ->
        "@"
    | NotAnnotatedHenceNullableMode ->
        "default"
    | ModelledNonnull ->
        "model"
  in
  match t with
  | Nullable nullable_origin ->
      F.fprintf fmt "Nullable[%s]" (string_of_nullable_origin nullable_origin)
  | Nonnull nonnull_origin ->
      F.fprintf fmt "Nonnull[%s]" (string_of_nonnull_origin nonnull_origin)


let of_annot_item ia =
  if Annotations.ia_is_nullable ia then
    let nullable_origin =
      if Annotations.ia_is_propagates_nullable ia then AnnotatedPropagatesNullable
      else AnnotatedNullable
    in
    Nullable nullable_origin
  else if Annotations.ia_is_nonnull ia then Nonnull AnnotatedNonnull
    (* Currently, we treat not annotated types as nonnull *)
  else Nonnull NotAnnotatedHenceNullableMode
