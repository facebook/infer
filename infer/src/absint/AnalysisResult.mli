(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type no_summary = AnalysisFailed | InBlockList | MutualRecursionCycle | UnknownProcedure
[@@deriving show]

type 'a t = ('a, no_summary) result

val to_option : 'a t -> 'a option
