(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type timestamp = int [@@deriving compare]

let t0 = 0

type t = {timestamp: timestamp}

(** path contexts is metadata that do not contribute to the semantics *)
let leq ~lhs:_ ~rhs:_ = true

(** see [leq] *)
let equal_fast _ _ = true

let pp fmt ({timestamp} [@warning "+9"]) = F.fprintf fmt "timestamp= %d" timestamp

let initial = {timestamp= 0}

let post_exec_instr {timestamp} = {timestamp= timestamp + 1}
