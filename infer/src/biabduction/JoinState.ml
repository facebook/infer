(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Object representing the status of the join operation *)

type mode = Pre | Post [@@deriving compare]

let equal_mode = [%compare.equal: mode]

(** set to true when we are doing join of footprints *)
let footprint = ref false

let get_footprint () = !footprint

let set_footprint b = footprint := b
