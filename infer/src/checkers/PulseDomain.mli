(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit
end

module Access : sig
  type t = AccessPath.access [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module AbstractAddressDomain : AbstractDomain.S with type astate = AbstractAddress.t

module MemoryEdges : module type of AbstractDomain.InvertedMap (Access) (AbstractAddressDomain)

module MemoryDomain : module type of AbstractDomain.InvertedMap (AbstractAddress) (MemoryEdges)

module AliasingDomain : module type of AbstractDomain.InvertedMap (Var) (AbstractAddressDomain)

module InvalidAddressesDomain : AbstractDomain.S

type t =
  { heap: MemoryDomain.astate
        (** Symbolic representation of the heap: a graph where nodes are abstract addresses and edges are
            access path elements. *)
  ; stack: AliasingDomain.astate
        (** Symbolic representation of the stack: which memory address do variables point to. No other
            values are being tracked. *)
  ; invalids: InvalidAddressesDomain.astate
        (** Set of addresses known to be in an invalid state. *) }

include AbstractDomain.S with type astate = t

val initial : t

module Diagnostic : sig
  type t

  val get_message : t -> string

  val get_location : t -> Location.t

  val get_issue_type : t -> IssueType.t

  val get_trace : t -> Errlog.loc_trace
end

type 'a access_result = ('a, Diagnostic.t) result

val read : Location.t -> AccessExpression.t -> t -> (t * AbstractAddress.t) access_result

val read_all : Location.t -> AccessExpression.t list -> t -> t access_result

val havoc : Var.t -> t -> t

val write : Location.t -> AccessExpression.t -> AbstractAddress.t -> t -> t access_result

val invalidate : Location.t -> AccessExpression.t -> t -> t access_result
