(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t =
  | Null
  | Nullable
  | ThirdPartyNonnull
  | UncheckedNonnull
  | LocallyTrustedNonnull
  | LocallyCheckedNonnull
  | ProvisionallyNullable
  | StrictNonnull
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
  | ProvisionallyNullable, _ | _, ProvisionallyNullable ->
      ProvisionallyNullable
  | UncheckedNonnull, _ | _, UncheckedNonnull ->
      UncheckedNonnull
  | LocallyTrustedNonnull, _ | _, LocallyTrustedNonnull ->
      LocallyTrustedNonnull
  | LocallyCheckedNonnull, _ | _, LocallyCheckedNonnull ->
      LocallyCheckedNonnull
  | StrictNonnull, StrictNonnull ->
      StrictNonnull


let is_subtype ~subtype ~supertype = equal (join subtype supertype) supertype

let is_considered_nonnull ~nullsafe_mode nullability =
  if equal ProvisionallyNullable nullability then
    (* Deliberately ignoring modes for this type
       (ProvisionallyNullable is currently not indented to work well with all features for non-default modes) *)
    true
  else
    let least_required =
      match nullsafe_mode with
      | NullsafeMode.Strict ->
          StrictNonnull
      | NullsafeMode.Local (NullsafeMode.Trust.Only trust_list)
        when NullsafeMode.Trust.is_trust_none trust_list ->
          (* Though "trust none" is technically a subcase of trust some,
             we need this pattern to be different from the one below so we can detect possible
             promotions from "trust some" to "trust none" *)
          LocallyCheckedNonnull
      | NullsafeMode.Local (NullsafeMode.Trust.Only _) ->
          LocallyTrustedNonnull
      | NullsafeMode.Local NullsafeMode.Trust.All ->
          UncheckedNonnull
      | NullsafeMode.Default ->
          (* We trust all internal method declarations, and, if specified separately,
             even not annotated third party. *)
          if Config.nullsafe_optimistic_third_party_in_default_mode then ThirdPartyNonnull
          else UncheckedNonnull
    in
    is_subtype ~subtype:nullability ~supertype:least_required


let is_nonnullish t = is_considered_nonnull ~nullsafe_mode:NullsafeMode.Default t

let to_string = function
  | Null ->
      "Null"
  | Nullable ->
      "Nullable"
  | ProvisionallyNullable ->
      "ProvisionallyNullable"
  | ThirdPartyNonnull ->
      "ThirdPartyNonnull"
  | UncheckedNonnull ->
      "UncheckedNonnull"
  | LocallyTrustedNonnull ->
      "LocallyTrustedNonnull"
  | LocallyCheckedNonnull ->
      "LocallyCheckedNonnull"
  | StrictNonnull ->
      "StrictNonnull"


let pp fmt t = F.fprintf fmt "%s" (to_string t)
