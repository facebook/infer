(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module UncheckedCallees : sig
  type t

  val encode : t -> string
end

module Summary : sig
  type t

  val pp : Format.formatter -> t -> unit

  val get_unchecked_callees : t -> UncheckedCallees.t
end

val checker : Summary.t InterproceduralAnalysis.t -> Summary.t option
