(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Object representing the status of the join operation *)

type mode =
  | Pre
  | Post
[@@deriving compare]

let equal_mode = [%compare.equal : mode]

(** set to true when we are doing join of footprints *)
let footprint = ref false

let get_footprint () = !footprint

let set_footprint b = footprint := b
