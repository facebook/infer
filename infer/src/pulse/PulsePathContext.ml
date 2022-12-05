(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

type t = {conditions: ValueHistory.t list; timestamp: Timestamp.t} [@@deriving compare, equal]

(** path contexts is metadata that do not contribute to the semantics *)
let leq ~lhs:_ ~rhs:_ = true

(** see [leq] *)
let equal_fast _ _ = true

let is_normal _ = true

let is_exceptional _ = true

let is_executable _ = true

let exceptional_to_normal x = x

let pp fmt ({conditions; timestamp} [@warning "+missing-record-field-pattern"]) =
  let pp_condition fmt hist =
    if Config.debug_level_analysis >= 3 then F.fprintf fmt "[%a]" ValueHistory.pp hist
  in
  F.fprintf fmt "conditions= [%a]@;timestamp= %a" (Pp.seq ~sep:";" pp_condition) conditions
    Timestamp.pp timestamp


let initial = {conditions= []; timestamp= Timestamp.t0}

let post_exec_instr {conditions; timestamp} = {conditions; timestamp= Timestamp.incr timestamp}
