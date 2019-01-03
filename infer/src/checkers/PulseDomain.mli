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

  val equal : t -> t -> bool

  val init : unit -> unit

  val pp : F.formatter -> t -> unit

  val mk_fresh : unit -> t
end

module Stack : sig
  include
    AbstractDomain.MapS
    with type key = Var.t
     and type value = AbstractAddress.t * Location.t option

  (* need to shadow the declaration in [MapS] even though it is unused since [MapS.compare] has a
     different type *)
  val compare : t -> t -> int [@@warning "-32"]
end

module AddrTracePair : sig
  type t = AbstractAddress.t * PulseTrace.t [@@deriving compare]
end

module Attribute : sig
  type t =
    | Invalid of PulseInvalidation.t
    | AddressOfCppTemporary of Var.t * Location.t option
    | Closure of Typ.Procname.t * AddrTracePair.t list
    | StdVectorReserve
  [@@deriving compare]
end

module Attributes : PrettyPrintable.PPSet with type elt = Attribute.t

module Memory : sig
  module Access :
    PrettyPrintable.PrintableOrderedType with type t = AbstractAddress.t HilExp.Access.t

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  type cell = edges * Attributes.t

  type t [@@deriving compare]

  val find_opt : AbstractAddress.t -> t -> cell option

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val add_edge_and_back_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t -> PulseInvalidation.t -> t -> t

  val get_invalidation : AbstractAddress.t -> t -> PulseInvalidation.t option
  (** None denotes a valid location *)

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end

type t = {heap: Memory.t; stack: Stack.t} [@@deriving compare]

val initial : t

include AbstractDomain.S with type t := t
