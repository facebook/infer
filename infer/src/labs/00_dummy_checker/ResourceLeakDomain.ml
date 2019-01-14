(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = unit

let ( <= ) ~lhs:_ ~rhs:_ = assert false

let join _a _b = assert false

let widen ~prev:_ ~next:_ ~num_iters:_ = assert false

let pp fmt () = F.fprintf fmt "(nothing)"

let initial = ()

type summary = t
