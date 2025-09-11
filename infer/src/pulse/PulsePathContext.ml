(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type t = {timestamp: Timestamp.t; is_non_disj: bool} [@@deriving compare, equal]

(** path contexts is metadata that do not contribute to the semantics *)
let leq ~lhs:_ ~rhs:_ = true

(** see [leq] *)
let equal_fast _ _ = true

let join {timestamp= ts1; is_non_disj= nd1} {timestamp= ts2; is_non_disj= nd2} =
  {timestamp= Timestamp.max ts1 ts2; is_non_disj= nd1 || nd2}


let is_normal _ = true

let is_exceptional _ = true

let is_executable _ = true

let exceptional_to_normal x = x

(* pulse-infinite do nothing *)
let back_edge _ _ _ = ([], -1)

let pp fmt ({timestamp; is_non_disj} [@warning "+missing-record-field-pattern"]) =
  F.fprintf fmt "timestamp= %a%s" Timestamp.pp timestamp
    (if is_non_disj then " NON-DISJUNCTIVE EXECUTION" else "")


let initial = {timestamp= Timestamp.t0; is_non_disj= false}

let post_exec_instr path = {path with timestamp= Timestamp.incr path.timestamp}
