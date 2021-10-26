(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type t = {timestamp: Timestamp.t} [@@deriving compare, equal]

(** path contexts is metadata that do not contribute to the semantics *)
let leq ~lhs:_ ~rhs:_ = true

(** see [leq] *)
let equal_fast _ _ = true

let pp fmt ({timestamp} [@warning "+9"]) = F.fprintf fmt "timestamp= %a" Timestamp.pp timestamp

let initial = {timestamp= Timestamp.t0}

let post_exec_instr {timestamp} = {timestamp= Timestamp.incr timestamp}
