(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0

(** Hash tables *)

include module type of HashTable_intf
module Make (Key : HashedType) : S with type key = Key.t
