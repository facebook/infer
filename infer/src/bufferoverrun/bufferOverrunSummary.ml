(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations

type t = Dom.Mem.t * PO.ConditionSet.summary_t

let get_output : t -> Dom.Mem.t = fst

let get_cond_set : t -> PO.ConditionSet.summary_t = snd

let pp : F.formatter -> t -> unit =
 fun fmt (exit_mem, condition_set) ->
  F.fprintf fmt "%a@;%a" Dom.Mem.pp exit_mem PO.ConditionSet.pp_summary condition_set
