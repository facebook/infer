(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Representation of a type in Java program, but with added information about its nullability.
    Nullability information might come either from explicit annotations, or from other sources,
    including conventions about defaults.
*)

type nullability = Nullable of nullable_origin | Nonnull of nonnull_origin [@@deriving compare]

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

type t = {nullability: nullability; typ: Typ.t} [@@deriving compare]

val pp : Format.formatter -> t -> unit

val nullability_of_annot_item : Annot.Item.t -> nullability
(** Converts the information from the annotation to nullability.
    NOTE: it does not take into account models etc., so this is intended to be used
    as a helper function for more high-level annotation processing.
 *)
