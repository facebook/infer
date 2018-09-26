(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module Pure = AbstractDomain.BooleanAnd

type summary = Pure.astate

let pp_summary fmt summary = F.fprintf fmt "@\n Purity summary: %a @\n" Pure.pp summary
