(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! NS0
include Base.String
module Set = Set.Make (Base.String)
module Map = Map.Make (Base.String)
module Tbl = HashTable.Make (Base.String)
