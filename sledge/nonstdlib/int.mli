(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of CCInt
include module type of Base.Int
module Set : Set.S with type elt = int
module Map : Map.S with type key = int
