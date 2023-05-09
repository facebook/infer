(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val report : t -> Procdesc.t -> unit
end

val checker :
  Summary.t InterproceduralAnalysis.t -> LineageShape.Summary.t option -> Summary.t option
