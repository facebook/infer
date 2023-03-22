(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type domain

val pp_domain : F.formatter -> domain -> unit

val checker : domain InterproceduralAnalysis.t -> domain option
