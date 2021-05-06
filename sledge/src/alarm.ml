(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind = Abort | Invalid_memory_access

let pp_kind fs = function
  | Abort -> Format.fprintf fs "Abort"
  | Invalid_memory_access -> Format.fprintf fs "Invalid memory access"

type t =
  { kind: kind
  ; loc: Llair.Loc.t
  ; pp_action: Format.formatter -> unit
  ; pp_state: Format.formatter -> unit }

let pp fs {kind; loc; pp_action} =
  Format.fprintf fs "%a %a@;<1 2>@[%t@]" Llair.Loc.pp loc pp_kind kind
    pp_action

let pp_trace fs alarm =
  Format.fprintf fs "%a@;<1 2>@[{ %t@ }@]" pp alarm alarm.pp_state
