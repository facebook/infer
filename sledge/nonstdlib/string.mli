(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include module type of Base.String
module Set : Set.S with type elt = string
module Map : Map.S with type key = string
