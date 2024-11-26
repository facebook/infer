(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

module Mem : sig
  type t = {loc: Location.t}

  val compare : t -> t -> int

  val pp : F.formatter -> t -> unit
end

module Summary : module type of AbstractDomain.FiniteSet (Mem)

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
