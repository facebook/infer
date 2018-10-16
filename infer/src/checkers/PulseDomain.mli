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

  val mk_fresh : unit -> t

  val pp : F.formatter -> t -> unit
end

module MemoryKey : sig
  type t = AbstractLocation.t * AccessPath.access [@@deriving compare]

  val pp : F.formatter -> t -> unit
end

module AbstractLocationDomain : AbstractDomain.S with type astate = AbstractLocation.t

module AbstractLocationsDomain : module type of AbstractDomain.FiniteSet (AbstractLocation)

module MemoryDomain : module type of AbstractDomain.Map (MemoryKey) (AbstractLocationDomain)

module AliasingDomain : module type of AbstractDomain.Map (Var) (AbstractLocationDomain)

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

val check_loc_access : AbstractLocation.t -> astate -> (astate, astate * string) result
(** Check that the location is not known to be invalid *)

val materialize_location :
  astate -> AccessExpression.t -> (astate * AbstractLocation.t, astate * string) result
(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract location representing what the expression points to.

    Return an error state if it traverses some known invalid location or if the end destination is
    known to be invalid. *)

val overwrite_location :
     astate
  -> AccessExpression.t
  -> AbstractLocation.t
  -> (astate * AbstractLocation.t, astate * string) result
(** Use the stack and heap to walk the access path represented by the given expression down to an
    abstract location representing what the expression points to, and replace that with the given
    location.

    Return an error state if it traverses some known invalid location. *)

val mark_invalid : AbstractLocation.t -> astate -> astate
(** Add the given location to the set of know invalid locations. *)
