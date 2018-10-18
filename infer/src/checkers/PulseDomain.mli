(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module AbstractLocation : sig
  type t = private int [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module Access : sig
  type t = AccessPath.access [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module AbstractLocationDomain : AbstractDomain.S with type astate = AbstractLocation.t

module AbstractLocationsDomain : module type of AbstractDomain.FiniteSet (AbstractLocation)

module MemoryEdges : module type of AbstractDomain.InvertedMap (Access) (AbstractLocationDomain)

module MemoryDomain : module type of AbstractDomain.InvertedMap (AbstractLocation) (MemoryEdges)

module AliasingDomain : module type of AbstractDomain.InvertedMap (Var) (AbstractLocationDomain)

module InvalidLocationsDomain : module type of AbstractLocationsDomain

type t =
  { heap: MemoryDomain.astate
        (** Symbolic representation of the heap: a graph where nodes are abstract locations and edges are
            access path elements. *)
  ; stack: AliasingDomain.astate
        (** Symbolic representation of the stack: which memory location do variables point to. No other
            values are being tracked. *)
  ; invalids: InvalidLocationsDomain.astate
        (** Set of locations known to be in an invalid state. *) }

include AbstractDomain.S with type astate = t

val initial : t

module Diagnostic : sig
  type t

  val to_string : t -> string
end

type access_result = (t, t * Diagnostic.t) result

val read_all : AccessExpression.t list -> t -> access_result

val write : AccessExpression.t -> t -> access_result

val invalidate : AccessExpression.t -> t -> access_result
