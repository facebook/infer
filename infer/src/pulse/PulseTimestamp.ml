(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int [@@deriving compare, equal]

let t0 = 0

let pp = F.pp_print_int

let incr t = t + 1
