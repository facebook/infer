(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

(** elements of the base abstract domain (stack, heap, attributes) with a type-safe interface that
    reflects the expected normalization status of the abstract values they mention *)
module type S = sig
  (** an abstract value that is the canonical representative of its equivalence class (of other
      abstract values that are equal to it) in the current state (this may not be true if more
      equalities are added or generally any time the formula inside the state changes) *)
  type t = private AbstractValue.t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t

  val downcast_set : Set.t -> AbstractValue.Set.t [@@inline always]

  val unsafe_cast_set : AbstractValue.Set.t -> Set.t
  [@@deprecated
    "unsafe, obviously; please add a comment why you need to use this and suppress this warning \
     locally with [@alert \"-deprecated\"]"]
  [@@inline always]

  (** an abstract value that needs to be normalized; just [AbstractValue.t] under the hood too *)
  type needs_canon

  (** [PulseAbductiveDomain.t] but we cannot mention it there otherwise it would create a circular
      dependency.*)
  type astate

  (** {2 Functions to normalize abstract values}

      Also provided are helper functions that normalize inside of more complex values such as
      tuples. In that case the functions take care to preserve physical equality if the input value
      is already its own canonical representative.

      We need separate functions for [AbstractValue.t] and [needs_canon] values because of the type
      system. By convention functions that end in ['] are the same as the non-primed ones but
      operate on [AbstractValue.t] instead of [needs_canon] ones. *)

  val canon : astate -> needs_canon -> t
  (** fetch the canonical representative of the given [needs_canon] *)

  val canon' : astate -> AbstractValue.t -> t
  (** fetch the canonical representative of the given [AbstractValue.t] *)

  val canon_fst : astate -> needs_canon * 'a -> t * 'a

  val canon_fst' : astate -> AbstractValue.t * 'a -> t * 'a

  val canon_snd_fst : astate -> 'a * (needs_canon * 'b) -> 'a * (t * 'b)

  val canon_opt : astate -> needs_canon option -> t option

  val canon_opt' : astate -> AbstractValue.t option -> t option

  val canon_opt_fst : astate -> (needs_canon * 'a) option -> (t * 'a) option

  val canon_opt_fst4' :
    astate -> (AbstractValue.t * 'a * 'b * 'c) option -> (t * 'a * 'b * 'c) option

  val mk_fresh : unit -> t
  (** This is just [AbstractValue.mk_fresh] so technically is only guaranteed to produce a canonical
      representative if the value is not immediately made equal to another one... Use with caution! *)

  val unsafe_cast : AbstractValue.t -> t
  [@@deprecated
    "unsafe, obviously; please add a comment why you need to use this and suppress this warning \
     locally with [@alert \"-deprecated\"]"]
  [@@inline always]

  (** {2 Domain elements, revisited to be safe wrt value normalization} *)

  module Stack : sig
    include
      PulseBaseStack.S with type value = needs_canon * ValueHistory.t and type t = PulseBaseStack.t

    val add : Var.t -> AbstractValue.t * ValueHistory.t -> t -> t
    (* we don't care about the normalization status of the value we put in the map since the idea is
       to always normalize upon reading *)
  end

  module Memory :
    PulseBaseMemory.S
      with type key := t
       and type in_map_t := AbstractValue.t * ValueHistory.t
       and type out_of_map_t := needs_canon * ValueHistory.t
       and type t = PulseBaseMemory.t
       and type Edges.t = PulseBaseMemory.Edges.t

  val canon_access : astate -> PulseAccess.t -> Memory.Access.t

  module Attributes :
    PulseBaseAddressAttributes.S with type key := t and type t = PulseBaseAddressAttributes.t
end

(** for use in [PulseAbductiveDomain] to define [PulseAbductiveDomain.CanonValue] *)
module Make (AbductiveDomain : sig
  type astate

  val canon : astate -> AbstractValue.t -> AbstractValue.t
end) : S with type astate = AbductiveDomain.astate
