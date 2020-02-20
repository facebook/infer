(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Null  (** The only possible value for that type is null *)
  | Nullable  (** No guarantees on the nullability *)
  | ThirdPartyNonnull
      (** Values coming from third-party methods and fields not explictly annotated as [@Nullable].
          We still consider those as non-nullable but with the least level of confidence. *)
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
  | ThirdPartyNonnull, _ | _, ThirdPartyNonnull ->
      ThirdPartyNonnull
  | UncheckedNonnull, _ | _, UncheckedNonnull ->
      UncheckedNonnull
  | LocallyCheckedNonnull, _ | _, LocallyCheckedNonnull ->
      LocallyCheckedNonnull
  | StrictNonnull, StrictNonnull ->
      StrictNonnull


let is_subtype ~subtype ~supertype = equal (join subtype supertype) supertype

let is_considered_nonnull ~nullsafe_mode nullability =
  let least_required =
    match nullsafe_mode with
    | NullsafeMode.Strict ->
        StrictNonnull
    | NullsafeMode.Local (NullsafeMode.Trust.Only _classes) ->
        (* TODO(T61473665). For now treat trust with specified classes as trust=none.  *)
        LocallyCheckedNonnull
    | NullsafeMode.Local NullsafeMode.Trust.All ->
        UncheckedNonnull
    | NullsafeMode.Default ->
        ThirdPartyNonnull
  in
  is_subtype ~subtype:nullability ~supertype:least_required


let is_nonnullish t = is_considered_nonnull ~nullsafe_mode:NullsafeMode.Default t

let to_string = function
  | Null ->
      "Null"
  | Nullable ->
      "Nullable"
  | ThirdPartyNonnull ->
      "ThirdPartyNonnull"
  | UncheckedNonnull ->
      "UncheckedNonnull"
  | LocallyCheckedNonnull ->
      "LocallyCheckedNonnull"
  | StrictNonnull ->
      "StrictNonnull"


let pp fmt t = F.fprintf fmt "%s" (to_string t)
