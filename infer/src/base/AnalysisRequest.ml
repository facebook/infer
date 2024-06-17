(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type checker_without_payload = LoopHoisting [@@deriving equal, hash, show]

type t = All | One of PayloadId.t | CheckerWithoutPayload of checker_without_payload
[@@deriving equal, hash, show]

let all = All

let one payload_id = if Config.detach_analysis_dependency then One payload_id else All

let checker_without_payload checker =
  if Config.detach_analysis_dependency then CheckerWithoutPayload checker else All
