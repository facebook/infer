(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(** Abstract domains and domain combinators *)

module type S = sig
  type astate

  (** the partial order induced by join *)
  val (<=) : lhs:astate -> rhs:astate -> bool

  val join : astate -> astate -> astate

  val widen : prev:astate -> next:astate -> num_iters:int -> astate

  val pp : F.formatter -> astate -> unit
end

(** A domain with an explicit bottom value *)
module type WithBottom = sig
  include S

  (** The bottom value of the domain.
      Naming it empty instead of bottom helps to bind the empty
      value for sets and maps to the natural definition for bottom *)
  val empty : astate
end

(** A domain with an explicit top value *)
module type WithTop = sig
  include S

  val top : astate
end

(** Lift a pre-domain to a domain *)
module BottomLifted (Domain : S) : sig
  type astate =
    | Bottom
    | NonBottom of Domain.astate

  include WithBottom with type astate := astate
end

(** Create a domain with Top element from a pre-domain *)
module TopLifted (Domain : S) : sig
  type astate =
    | Top
    | NonTop of Domain.astate

  include WithTop with type astate := astate
end


(** Cartesian product of two domains. *)
module Pair (Domain1 : S) (Domain2 : S) : S with type astate = Domain1.astate * Domain2.astate

(** Lift a set to a powerset domain ordered by subset. The elements of the set should be drawn from
    a *finite* collection of possible values, since the widening operator here is just union. *)
module FiniteSet (Set : PrettyPrintable.PPSet) : sig
  include PrettyPrintable.PPSet with type t = Set.t and type elt = Set.elt
  include WithBottom with type astate = t
end

(** Lift a set to a powerset domain ordered by superset, so the join operator is intersection *)
module InvertedSet (Set : PrettyPrintable.PPSet) : sig
  include PrettyPrintable.PPSet with type t = Set.t and type elt = Set.elt
  include S with type astate = t
end

(** Map domain ordered by union over the set of bindings, so the bottom element is the empty map.
    Every element implicitly maps to bottom unless it is explicitly bound to something else *)
module Map (Map : PrettyPrintable.PPMap) (ValueDomain : S) : sig
  include PrettyPrintable.PPMap with type 'a t = 'a Map.t and type key = Map.key
  include WithBottom with type astate = ValueDomain.astate Map.t
end

(** Map domain ordered by intersection over the set of bindings, so the top element is the empty
    map. Every element implictly maps to top unless it is explicitly bound to something else *)
module InvertedMap (Map : PrettyPrintable.PPMap) (ValueDomain : S) : sig
  include PrettyPrintable.PPMap with type 'a t = 'a Map.t and type key = Map.key
  include S with type astate = ValueDomain.astate Map.t

  val empty : [`This_domain_is_not_pointed] (** this domain doesn't have a natural bottom element *)
end

(** Boolean domain ordered by p || ~q. Useful when you want a boolean that's true only when it's
    true in both conditional branches. *)
module BooleanAnd : S with type astate = bool

(** Boolean domain ordered by ~p || q. Useful when you want a boolean that's true only when it's
    true in one conditional branch. *)
module BooleanOr : S with type astate = bool
