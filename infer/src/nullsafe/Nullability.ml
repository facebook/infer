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
  | LocallyCheckedNonnull
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
