(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  | Nullable  (** No guarantees on the nullability *)
  | Nonnull
      (** We believe that this value can not be null. If it is not the case, this is
          an unsoundness issue for Nullsafe, and we aim to minimize number of such issues
          occuring in real-world programs. *)
[@@deriving compare, equal]

let join x y =
  match (x, y) with Nullable, _ | _, Nullable -> Nullable | Nonnull, Nonnull -> Nonnull


let is_subtype ~subtype ~supertype = equal (join subtype supertype) supertype

let to_string = function Nullable -> "Nullable" | Nonnull -> "Nonnull"
