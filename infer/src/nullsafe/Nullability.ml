(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | Nullable  (** No guarantees on the nullability *)
  | DeclaredNonnull
      (** The type comes from a signature that is annotated (explicitly or implicitly according to conventions)
      as non-nullable. Hovewer, it might still contain null since the truthfullness of the declaration was
      not checked.
   *)
  | Nonnull
      (** We believe that this value can not be null. If it is not the case, this is
          an unsoundness issue for Nullsafe, and we aim to minimize number of such issues
          occuring in real-world programs. *)
[@@deriving compare, equal]

let top = Nullable

let join x y =
  match (x, y) with
  | Nullable, _ | _, Nullable ->
      Nullable
  | DeclaredNonnull, _ | _, DeclaredNonnull ->
      DeclaredNonnull
  | Nonnull, Nonnull ->
      Nonnull


let is_subtype ~subtype ~supertype = equal (join subtype supertype) supertype

let is_strict_subtype ~subtype ~supertype =
  is_subtype ~subtype ~supertype && not (equal subtype supertype)


let to_string = function
  | Nullable ->
      "Nullable"
  | DeclaredNonnull ->
      "DeclaredNonnull"
  | Nonnull ->
      "Nonnull"
