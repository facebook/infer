(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format

(* Extremely simple abstraction of resources: count the number of acquired resources. If there's
   not a corresponding number of releases, there may be a leak. *)
type astate = int

(* 2(a) *)
(* For now, type of abstract state and summary are the same *)
type summary = astate

(* 4(a) *)

let ( <= ) ~lhs ~rhs = lhs <= rhs

let join = Pervasives.max

let widen ~prev ~next ~num_iters:_ = join prev next

let pp fmt astate = F.fprintf fmt "Resource count: %d" astate

(* At the beginning of a procedure, assume no resources are held *)
let initial = 0
