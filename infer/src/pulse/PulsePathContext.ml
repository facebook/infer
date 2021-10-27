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

let pp fmt ({conditions; timestamp} [@warning "+9"]) =
  let pp_condition fmt hist =
    if Config.debug_level_analysis >= 3 then F.fprintf fmt "[%a]" ValueHistory.pp hist
  in
  F.fprintf fmt "conditions= [%a]@;timestamp= %a" (Pp.seq ~sep:";" pp_condition) conditions
    Timestamp.pp timestamp


let initial = {conditions= []; timestamp= Timestamp.t0}

let with_context path hist =
  if List.is_empty path.conditions then hist else ValueHistory.Branching (hist :: path.conditions)


let post_exec_instr {conditions; timestamp} = {conditions; timestamp= Timestamp.incr timestamp}
