(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t =
  | Call of Procname.t
  | Model of string
  | SkippedKnownCall of Procname.t
  | SkippedUnknownCall of Exp.t
[@@deriving compare, equal]

let pp_config ~verbose fmt =
  let pp_proc_name = if verbose then Procname.pp else Procname.describe in
  function
  | Call proc_name ->
      F.fprintf fmt "`%a`" pp_proc_name proc_name
  | Model model ->
      F.fprintf fmt "`%s` (modelled)" model
  | SkippedKnownCall proc_name ->
      F.fprintf fmt "function `%a` with no summary" pp_proc_name proc_name
  | SkippedUnknownCall call_exp ->
      F.fprintf fmt "unresolved call expression `%a`" Exp.pp call_exp


let pp = pp_config ~verbose:true

let describe = pp_config ~verbose:false

let pp_name_only fmt = function
  | Call proc_name ->
      F.fprintf fmt "%a" Procname.pp_without_templates proc_name
  | Model model ->
      F.fprintf fmt "%s" model
  | SkippedKnownCall proc_name ->
      F.fprintf fmt "%a" Procname.pp_without_templates proc_name
  | SkippedUnknownCall call_exp ->
      F.fprintf fmt "%a" Exp.pp call_exp
