(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | Null  (** The only possible value for that type is null *)
  | Nullable  (** No guarantees on the nullability *)
  | UncheckedNonnull
      (** The type comes from a signature that is annotated (explicitly or implicitly according to
          conventions) as non-nullable. However, it might still contain null since the truthfulness
          of the declaration was not checked. *)
  | LocallyCheckedNonnull
      (** Non-nullable value the comes from a class checked under local mode. Local mode type-checks
          files against its dependencies but does not require the dependencies to be transitively
          checked. Therefore this type of non-nullable value is differentiated from StrictNonnull. *)
  | StrictNonnull
      (** Non-nullable value with the highest degree of certainty, because it is either:

          - a non-null literal,
          - an expression that semantically cannot be null,
          - comes from internal code checked under strict mode,
          - comes from a third-party method with an appropriate built-in model or user-defined
            nullability signature.

          The latter two are potential sources of unsoundness issues for nullsafe, but we need to
          strike the balance between the strictness of analysis, convenience, and real-world risk. *)
[@@deriving compare, equal]

type pair = t * t [@@deriving compare, equal]

let top = Nullable

let join x y =
  match (x, y) with
  | Null, Null ->
      Null
  | Null, _ | _, Null ->
      Nullable
  | Nullable, _ | _, Nullable ->
      Nullable
  | UncheckedNonnull, _ | _, UncheckedNonnull ->
      UncheckedNonnull
  | LocallyCheckedNonnull, _ | _, LocallyCheckedNonnull ->
      LocallyCheckedNonnull
  | StrictNonnull, StrictNonnull ->
      StrictNonnull


let is_subtype ~subtype ~supertype = equal (join subtype supertype) supertype

let to_string = function
  | Null ->
      "Null"
  | Nullable ->
      "Nullable"
  | UncheckedNonnull ->
      "UncheckedNonnull"
  | LocallyCheckedNonnull ->
      "LocallyCheckedNonnull"
  | StrictNonnull ->
      "StrictNonnull"
