(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int

let leq ~lhs ~rhs = lhs <= rhs

let join a b = max a b

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let pp fmt astate = F.fprintf fmt "%d" astate

let initial = 0

let acquire_resource held = held + 1

let release_resource held = held - 1

let has_leak held = held > 0

type summary = t
